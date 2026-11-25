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

test_that("fit_task_models_multigroup() fixes the reference group's distribution", {
  skip_if_not_installed("mirt")
  trials <- bind_rows(
    fit_trials(n_persons = 150, seed = 1) |> mutate(site = "s1"),
    fit_trials(n_persons = 150, theta_mean = 1, seed = 2) |> mutate(site = "s2",
                                                                   run_id = paste0(run_id, "b"))
  )
  task_data <- tibble(item_task = "hf", data = list(trials))
  # the all_items refit warns (mirt identification notice on the
  # pars = "values" call, tidyselect deprecation of .data); both predate
  # reference_group
  fit_scalar <- \(registry_dir, ...) {
    suppressWarnings(suppressMessages(capture.output(
      fit_task_models_multigroup(
        task_data = task_data,
        models = tibble(nfact = 1, itemtype = "Rasch", invariance = "scalar"),
        priors = list(d = c("norm", 0, 3)),
        task = "hf", group = "site", registry_dir = registry_dir, ...
      )
    )))
    readRDS(file.path(registry_dir, "hf", "multigroup_site", "all_items",
                      "hf_rasch_f1_scalar.rds"))
  }
  group_mean <- \(mod_rec, group) {
    vals <- model_vals(mod_rec)
    vals$value[vals$group == group & vals$name == "MEAN_1"]
  }

  # default: the alphabetically first group is the reference
  rec_default <- fit_scalar(withr::local_tempdir())
  expect_equal(rec_default@group_names, c("s1", "s2"))
  expect_equal(group_mean(rec_default, "s1"), 0)

  rec_ref <- fit_scalar(withr::local_tempdir(), reference_group = "s2")
  expect_equal(rec_ref@group_names, c("s2", "s1"))
  expect_equal(group_mean(rec_ref, "s2"), 0)
  expect_lt(group_mean(rec_ref, "s1"), -0.5)

  # the reconstructed model keeps each group's values with its group
  mod <- model_from_record(rec_ref)
  expect_equal(mirt::extract.mirt(mod, "groupNames"), c("s2", "s1"))
  rescored <- mirt::fscores(mod, method = "EAP")[, 1]
  expect_equal(unname(rescored), scores(rec_ref)$ability, tolerance = 1e-6)

  expect_error(fit_scalar(withr::local_tempdir(), reference_group = "s3"),
               "reference_group must be one of the groups")
})

test_that("scalar fits estimate one bounded lower asymptote per free_guess item", {
  skip_if_not_installed("mirt")
  # items 1-3 have a 0.5 floor; item "timed" (given twice per run) a 0.2 floor
  sim <- \(n, theta_mean, seed, site) {
    set.seed(seed)
    theta <- rnorm(n, theta_mean)
    d <- c(-1, 0, 1, 0.5, 0.5)
    floor <- c(0.5, 0.5, 0.5, 0.2, 0.2)
    probs <- sweep(plogis(outer(theta, d, `-`)), 2, 1 - floor, `*`) |> sweep(2, floor, `+`)
    resp <- matrix(rbinom(length(probs), 1, probs), nrow = n)
    grid <- expand.grid(run = seq_len(n), item = 1:5)
    tibble(run_id = paste0(site, "_", grid$run),
           item_uid = c("i1", "i2", "i3", "timed", "timed")[grid$item],
           correct = as.logical(resp[cbind(grid$run, grid$item)]),
           chance = 0.5, free_guess = grid$item >= 4, site = site)
  }
  trials <- bind_rows(sim(600, 0, 1, "s1"), sim(600, 0.5, 2, "s2"))
  task_data <- tibble(item_task = "hf", data = list(trials))
  fit <- \(itemtype) {
    registry_dir <- withr::local_tempdir(.local_envir = parent.frame())
    suppressWarnings(suppressMessages(capture.output(
      fit_task_models_multigroup(
        task_data = task_data,
        models = tibble(nfact = 1, itemtype = itemtype, invariance = "scalar"),
        priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
        task = "hf", group = "site", registry_dir = registry_dir
      )
    )))
    readRDS(file.path(registry_dir, "hf", "multigroup_site", "all_items",
                      glue::glue("hf_{tolower(itemtype)}_f1_scalar.rds")))
  }
  g_vals <- \(rec, prefix) {
    vals <- model_vals(rec)
    vals$value[vals$name == "g" & startsWith(vals$item, prefix)]
  }

  rasch <- fit("Rasch")
  timed_g <- g_vals(rasch, "timed-")
  # one value for both instances in both groups, below chance
  expect_length(timed_g, 4)
  expect_equal(length(unique(round(timed_g, 8))), 1)
  expect_lt(timed_g[1], 0.4)
  expect_true(all(g_vals(rasch, "i") == 0.5))

  # 2PL keeps every lower asymptote at chance
  expect_true(all(g_vals(fit("2PL"), "") == 0.5))
})

test_that("metric invariance frees the focal groups' variances", {
  skip_if_not_installed("mirt")
  trials <- bind_rows(
    fit_trials(n_persons = 200, seed = 1) |> mutate(site = "s1"),
    fit_trials(n_persons = 200, seed = 2) |> mutate(site = "s2",
                                                    run_id = paste0(run_id, "b"))
  )
  task_data <- tibble(item_task = "hf", data = list(trials))
  registry_dir <- withr::local_tempdir()

  suppressWarnings(suppressMessages(capture.output(
    fit_task_models_multigroup(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "2PL", invariance = "metric"),
      priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
      task = "hf", group = "site", registry_dir = registry_dir
    )
  )))

  mod_rec <- readRDS(file.path(registry_dir, "hf", "multigroup_site", "overlap_items",
                               "hf_2pl_f1_metric.rds"))
  vals <- model_vals(mod_rec)
  est <- \(group, name) vals$est[vals$group == group & vals$name == name]
  # reference group fixed at N(0, 1); focal group variance estimated, mean fixed
  expect_false(est("s1", "COV_11"))
  expect_true(est("s2", "COV_11"))
  expect_false(est("s2", "MEAN_1"))
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

test_that("generate_model_str_numeric() shares one slope per class", {
  # every item dichotomous (both response categories present)
  df <- tibble(item_uid = rep(c("a", "b", "c", "d"), each = 2), correct = rep(c(0, 1), 4))
  df_prepped <- data.frame(`a-1` = 0, `a-2` = 1, `b-1` = 1, `c-1` = 0, `d-1` = 1, check.names = FALSE)
  classes <- c(a = "x", b = "x", c = "y")

  str <- generate_model_str_numeric(df, df_prepped, "2PL", 1, slope_classes = classes)
  # a's instances share an intercept; a and b share a slope; c's class has one
  # item and d has none, so their slopes are free
  expect_match(str, "(1,2,d)", fixed = TRUE)
  expect_match(str, "(1,2,3,a1)", fixed = TRUE)
  expect_no_match(str, "(1,2,a1)", fixed = TRUE)
  expect_no_match(str, "(4,a1)", fixed = TRUE)

  # without classes, a's instances share a slope only with each other
  expect_match(generate_model_str_numeric(df, df_prepped, "2PL", 1), "(1,2,a1)", fixed = TRUE)
  # Rasch models have no slopes to constrain
  expect_no_match(generate_model_str_numeric(df, df_prepped, "Rasch", 1, slope_classes = classes),
                  "a1", fixed = TRUE)
})

test_that("2PLclass multigroup fits share one slope per class", {
  skip_if_not_installed("mirt")
  # items lo1-lo3 discriminate weakly and hi1-hi3 strongly; lo4 (class lo) and
  # solo (its own class) are given only in s1, so they enter the all_items fit
  sim <- \(n, theta_mean, seed, site, extra) {
    set.seed(seed)
    theta <- rnorm(n, theta_mean)
    items <- c("lo1", "lo2", "lo3", "hi1", "hi2", "hi3", if (extra) c("lo4", "solo"))
    a <- c(lo1 = 0.6, lo2 = 0.6, lo3 = 0.6, hi1 = 1.8, hi2 = 1.8, hi3 = 1.8, lo4 = 0.6, solo = 1.2)[items]
    b <- c(lo1 = -1, lo2 = 0, lo3 = 1, hi1 = -1, hi2 = 0, hi3 = 1, lo4 = 0.5, solo = -0.5)[items]
    probs <- plogis(sweep(outer(theta, b, `-`), 2, a, `*`))
    resp <- matrix(rbinom(length(probs), 1, probs), nrow = n)
    grid <- expand.grid(run = seq_len(n), item = seq_along(items))
    tibble(run_id = paste0(site, "_", grid$run), item_uid = items[grid$item],
           correct = as.logical(resp[cbind(grid$run, grid$item)]), chance = 0, site = site)
  }
  trials <- bind_rows(sim(800, 0, 1, "s1", TRUE), sim(800, 0.3, 2, "s2", FALSE))
  task_data <- tibble(item_task = "sds", data = list(trials))
  classes <- \(item_uids) dplyr::case_when(startsWith(item_uids, "lo") ~ "lo",
                                           startsWith(item_uids, "hi") ~ "hi",
                                           .default = item_uids)
  registry_dir <- withr::local_tempdir()
  suppressWarnings(suppressMessages(capture.output(
    fit_task_models_multigroup(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "2PLclass", invariance = "scalar"),
      priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
      task = "sds", group = "site", registry_dir = registry_dir,
      slope_classes = classes
    )
  )))
  read_model <- \(subset) readRDS(file.path(registry_dir, "sds", "multigroup_site", subset,
                                            "sds_2plclass_f1_scalar.rds"))
  slopes <- \(rec) {
    vals <- model_vals(rec)
    a1 <- vals[vals$name == "a1" & vals$group == "s1", ]
    purrr::set_names(a1$value, sub("-\\d+$", "", a1$item))
  }

  overlap <- slopes(read_model("overlap_items"))
  expect_setequal(names(overlap), c("lo1", "lo2", "lo3", "hi1", "hi2", "hi3"))
  expect_equal(length(unique(round(overlap[c("lo1", "lo2", "lo3")], 8))), 1)
  expect_equal(length(unique(round(overlap[c("hi1", "hi2", "hi3")], 8))), 1)
  expect_lt(overlap[["lo1"]], 1)
  expect_gt(overlap[["hi1"]], 1.3)

  full_rec <- read_model("all_items")
  full <- slopes(full_rec)
  # lo4 takes class lo's slope from the overlap model; solo's is estimated
  expect_equal(full[["lo4"]], overlap[["lo1"]], tolerance = 1e-8)
  expect_equal(full[["hi1"]], overlap[["hi1"]], tolerance = 1e-8)
  expect_false(isTRUE(all.equal(full[["solo"]], overlap[["lo1"]])))
  expect_equal(full_rec@fit$slope_classes$class[full_rec@fit$slope_classes$item_uid == "lo4"], "lo")
})

test_that("2PLclass models need slope_classes", {
  task_data <- tibble(item_task = "sds", data = list(fit_trials(50) |> mutate(site = "s1")))
  expect_error(
    fit_task_models_multigroup(
      task_data = task_data,
      models = tibble(nfact = 1, itemtype = "2PLclass", invariance = "scalar"),
      priors = list(), task = "sds", group = "site", registry_dir = withr::local_tempdir()
    ),
    "2PLclass models need slope_classes"
  )
})

test_that("sds_slope_classes() classes picks by position and earlier non-matches", {
  expect_equal(
    sds_slope_classes(c("sds_dimensions_color", "sds_same_shape", "sds_2match_choice1_0m0n",
                        "sds_2match_choice2_1m0n", "sds_3match_choice3_1m0n",
                        "sds_3match_choice3_1m1n", "sds_4match_choice4_1m2n", "hf_other")),
    c("dimensions", "same", "match_first", "match_later", "match_later",
      "match_after_nonmatch", "match_after_nonmatch", "hf_other"))
})
