# Unit tests for the registry lookup / model-specification helpers. These build
# the strings and filenames used to locate a fitted model in the registry, so a
# silent change here would point scoring at the wrong (or no) model.

test_that("mod_spec_str() joins non-NA spec fields with underscores", {
  spec <- list(model_set = "multigroup_dataset", subset = "all_items",
               itemtype = "Rasch", nfact = "f1", invariance = "scalar")
  expect_equal(mod_spec_str(spec),
               "multigroup_dataset_all_items_Rasch_f1_scalar")

  # NA fields are dropped
  spec_na <- modifyList(spec, list(invariance = NA, nfact = NA))
  expect_equal(mod_spec_str(spec_na), "multigroup_dataset_all_items_Rasch")
})

test_that("model_spec_filename() builds the registry path", {
  spec <- list(item_task = "math", model_set = "multigroup_dataset",
               subset = "all_items", itemtype = "Rasch", nfact = "f1",
               invariance = "scalar")

  expect_equal(
    as.character(model_spec_filename(spec)),
    "math/multigroup_dataset/all_items/math_Rasch_f1_scalar.rds"
  )
})

test_that("get_model_spec() returns a single-row spec or NULL", {
  scoring_table <- tibble(
    item_task = c("math", "vocab", "math"),
    dataset = c("co", "co", "us"),
    model_set = "multigroup_dataset"
  )

  spec <- get_model_spec("math", "co", scoring_table)
  expect_type(spec, "list")
  expect_equal(spec$item_task, "math")
  expect_equal(spec$dataset, "co")

  # no match -> NULL
  expect_null(get_model_spec("math", "de", scoring_table))
})

test_that("translate_invariance() maps mirt invariance terms to shorthand", {
  expect_equal(translate_invariance(""), "configural")
  expect_equal(translate_invariance(c("slopes")), "metric")
  expect_equal(
    translate_invariance(c("free_means", "free_var", "intercepts", "slopes")),
    "scalar"
  )
  # an unrecognized set matches nothing
  expect_length(translate_invariance(c("intercepts")), 0)
})
