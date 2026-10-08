# Tests for the model-fitting entry points. The two fit_task_models_* functions
# get one small smoke test each: they fit a real (tiny) mirt model and check
# that a readable ModelRecord lands at the registry path scoring will look for.
# The by-language wrappers and the multigroup guard clauses are tested without
# fitting anything.

# long trial data in the shape the fitting functions consume: Rasch responses
# with a 4AFC guessing floor, as LEVANTE items are scored.
fit_trials <- function(n_persons, difficulty = c(-1.5, -0.7, 0, 0.7, 1.5),
                       items = paste0("item", seq_along(difficulty)),
                       theta_mean = 0, seed = 42) {
  set.seed(seed)
  theta <- rnorm(n_persons, theta_mean)
  probs <- 0.25 + 0.75 * plogis(outer(theta, difficulty, `-`))
  resp <- matrix(rbinom(length(probs), 1, probs), nrow = n_persons)
  grid <- expand.grid(run = seq_len(n_persons), item = seq_along(difficulty))
  tibble(
    run_id = paste0("run", grid$run),
    item_uid = items[grid$item],
    correct = as.logical(resp[cbind(grid$run, grid$item)]),
    chance = 0.25
  )
}

test_that("fit_task_models_pooled() writes a ModelRecord to the registry path", {
  skip_if_not_installed("mirt")
  trials <- fit_trials(n_persons = 150)
  task_data <- tibble(item_task = "hf", language = "en", data = list(trials))
  registry_dir <- withr::local_tempdir()

  suppressMessages(capture.output(
    fit_task_models_pooled(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "Rasch"),
      priors = list(d = c("norm", 0, 3)),
      task = "hf", subset_var = "language", subset_val = "en",
      registry_dir = registry_dir
    )
  ))

  mod_file <- file.path(registry_dir, "hf", "by_language", "en", "hf_rasch_f1.rds")
  expect_true(file.exists(mod_file))

  mod_rec <- readRDS(mod_file)
  expect_s4_class(mod_rec, "ModelRecord")
  expect_equal(model_class(mod_rec), "SingleGroupClass")
  expect_setequal(items(mod_rec), paste0("item", 1:5, "-1"))
  expect_equal(nrow(scores(mod_rec)), 150)
})

test_that("fit_task_models_multigroup() writes a ModelRecord per group set", {
  skip_if_not_installed("mirt")
  trials <- bind_rows(
    fit_trials(n_persons = 120, seed = 1) |> mutate(site = "s1"),
    fit_trials(n_persons = 120, theta_mean = 0.4, seed = 2) |> mutate(site = "s2",
                                                                     run_id = paste0(run_id, "b"))
  )
  task_data <- tibble(item_task = "hf", data = list(trials))
  registry_dir <- withr::local_tempdir()

  suppressMessages(capture.output(
    fit_task_models_multigroup(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "Rasch", invariance = "configural"),
      priors = list(d = c("norm", 0, 3)),
      # group given as a bare variable; the pooled test above uses the string
      # form, so both ways of naming the variable are covered
      task = "hf", group = site, registry_dir = registry_dir
    )
  ))

  # every item is shared here, so only the overlap_items model is written
  # (the all_items refit is skipped for configural invariance)
  mod_file <- file.path(registry_dir, "hf", "multigroup_site", "overlap_items",
                        "hf_rasch_f1_configural.rds")
  expect_true(file.exists(mod_file))

  mod_rec <- readRDS(mod_file)
  expect_s4_class(mod_rec, "ModelRecord")
  expect_equal(model_class(mod_rec), "MultipleGroupClass")
  expect_setequal(mod_rec@group_names, c("s1", "s2"))
  expect_false(dir.exists(file.path(registry_dir, "hf", "multigroup_site", "all_items")))
})

test_that("fit_task_models_multigroup() skips a task that isn't in the data", {
  task_data <- tibble(item_task = "hf", data = list(tibble()))
  registry_dir <- withr::local_tempdir()

  expect_message(
    out <- fit_task_models_multigroup(
      task_data = task_data, models = tibble(), priors = NULL,
      task = "vocab", group = site, registry_dir = registry_dir
    ),
    "not present in task_data"
  )

  expect_null(out)
  expect_length(list.files(registry_dir), 0)
})

test_that("fit_task_models_multigroup() skips when no items are shared by all groups", {
  # s1 and s2 saw disjoint item sets, so there is nothing to anchor groups on
  trials <- bind_rows(
    tibble(run_id = c("r1", "r2"), item_uid = "a", correct = c(TRUE, FALSE)),
    tibble(run_id = c("r3", "r4"), item_uid = "b", correct = c(TRUE, FALSE))
  ) |>
    mutate(site = rep(c("s1", "s2"), each = 2), chance = 0.25)
  task_data <- tibble(item_task = "hf", data = list(trials))
  registry_dir <- withr::local_tempdir()

  expect_message(
    out <- fit_task_models_multigroup(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "Rasch", invariance = "configural"),
      priors = NULL, task = "hf", group = site, registry_dir = registry_dir
    ),
    "No items in common"
  )

  expect_null(out)
  expect_length(list.files(registry_dir), 0)
})

test_that("fit_bylanguage_task() fits each language of one task", {
  task_data <- tibble(
    item_task = c("hf", "hf", "vocab"),
    language = c("en", "de", "es"),
    data = list(tibble())
  )
  calls <- list()
  local_mocked_bindings(
    fit_task_models_pooled = function(task_data, models, priors, task,
                                      subset_var, subset_val, registry_dir) {
      calls[[length(calls) + 1]] <<- list(task = task, subset_val = subset_val)
    }
  )

  fit_bylanguage_task(task_data, models = NULL, priors = NULL, task = "hf",
                      registry_dir = "reg")

  # one call per language of hf, and none for the vocab-only language
  expect_equal(purrr::map_chr(calls, "task"), c("hf", "hf"))
  expect_equal(purrr::map_chr(calls, "subset_val"), c("en", "de"))
})

test_that("fit_bylanguage_lang() fits each task of one language", {
  task_data <- tibble(
    item_task = c("hf", "vocab", "mg"),
    language = c("en", "en", "de"),
    data = list(tibble())
  )
  calls <- list()
  local_mocked_bindings(
    fit_task_models_pooled = function(task_data, models, priors, task,
                                      subset_var, subset_val, registry_dir) {
      calls[[length(calls) + 1]] <<- list(task = task, subset_val = subset_val)
    }
  )

  fit_bylanguage_lang(task_data, models = NULL, priors = NULL, lang = "en",
                      registry_dir = "reg")

  # one call per task seen in en, and none for the de-only task
  expect_equal(purrr::map_chr(calls, "task"), c("hf", "vocab"))
  expect_equal(purrr::map_chr(calls, "subset_val"), c("en", "en"))
})
