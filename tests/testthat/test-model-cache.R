# Tests for the session cache of model records and reconstructed models used
# by get_model_record() and model_from_record().

clear_model_cache <- function() {
  model_cache$registry_dir <- NULL
  model_cache$records <- list()
  model_cache$models <- list()
}

test_that("get_model_record() downloads each model once per registry directory", {
  clear_model_cache()
  withr::defer(clear_model_cache())
  fx <- irt_fixture_multigroup()
  downloads <- character()
  local_mocked_bindings(get_registry_file = function(mod_filename, registry_dir) {
    downloads <<- c(downloads, mod_filename)
    fx$mod_rec
  })
  spec_a <- fx$spec
  spec_b <- modifyList(fx$spec, list(item_task = "other"))
  registry_1 <- new.env()
  registry_2 <- new.env()

  rec_1 <- get_model_record(spec_a, registry_1)
  rec_2 <- get_model_record(spec_a, registry_1)
  get_model_record(spec_b, registry_1)
  expect_identical(rec_1, rec_2)
  expect_length(downloads, 2)

  # a different registry directory clears the cache
  get_model_record(spec_a, registry_2)
  expect_length(downloads, 3)
})

test_that("model_from_record() reconstructs each model record once", {
  clear_model_cache()
  withr::defer(clear_model_cache())
  fx <- irt_fixture_multigroup()

  mod_1 <- model_from_record(fx$mod_rec)
  mod_2 <- model_from_record(fx$mod_rec)
  expect_identical(mod_1, mod_2)
  expect_length(model_cache$models, 1)

  # an equal but separately created record also uses the cached model
  model_from_record(irt_fixture_multigroup()$mod_rec)
  expect_length(model_cache$models, 1)

  # a different record gets its own model
  model_from_record(irt_fixture_2pl()$mod_rec)
  expect_length(model_cache$models, 2)
})

test_that("score_irt() gives the same scores with a cold and a warm cache", {
  clear_model_cache()
  withr::defer(clear_model_cache())
  fx <- irt_fixture_multigroup()
  g1_trials <- subset(fx$trials, dataset == "g1")
  new_trials <- transform(subset(fx$trials, dataset == "g2"), dataset = "g_new")

  cold_g1 <- suppressMessages(score_irt(g1_trials, fx$spec, fx$mod_rec))
  cold_new <- suppressMessages(score_irt(new_trials, fx$spec, fx$mod_rec))
  warm_g1 <- suppressMessages(score_irt(g1_trials, fx$spec, fx$mod_rec))
  warm_new <- suppressMessages(score_irt(new_trials, fx$spec, fx$mod_rec))

  expect_identical(cold_g1, warm_g1)
  expect_identical(cold_new, warm_new)
})
