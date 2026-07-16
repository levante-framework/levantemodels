# Tests for score()'s routing: it sends (task, dataset) to IRT scoring when the
# scoring table has a matching spec, to a custom scorer (sre) otherwise, and
# returns NULL when nothing applies. get_model_record() is mocked so the IRT
# path does not touch the registry.

test_that("score() routes to IRT scoring when a model spec matches", {
  fx <- irt_fixture_2pl()
  scoring_table <- tibble::as_tibble(fx$spec)

  local_mocked_bindings(get_model_record = function(spec, registry_dir) fx$mod_rec)

  scored <- suppressMessages(
    score("test", "all", fx$trials, runs = NULL,
          scoring_table = scoring_table, registry_dir = NULL)
  )

  expect_equal(nrow(scored), length(fx$run_ids))
  expect_true(all(scored$score_type == "ability"))
})

test_that("score() routes to the custom sre scorer when no spec matches", {
  # scoring table with no row for (sre, any)
  scoring_table <- tibble::tibble(item_task = "math", dataset = "co")
  trials <- tibble::tibble(
    run_id = rep(c("a", "b"), each = 2),
    trial_number = rep(1:2, 2),
    rt_numeric = rep(20, 4),
    dataset = "co",
    correct = c(TRUE, TRUE, TRUE, FALSE)
  )

  scored <- suppressMessages(
    score("sre", "any", trials, runs = NULL,
          scoring_table = scoring_table, registry_dir = NULL)
  )

  expect_true(all(scored$score_type == "guessing_adjusted_number_correct_scaled"))
})

test_that("score() returns NULL when no scoring method applies", {
  scoring_table <- tibble::tibble(item_task = "math", dataset = "co")

  scored <- suppressMessages(
    score("not_a_task", "any", trials = NULL, runs = NULL,
          scoring_table = scoring_table, registry_dir = NULL)
  )

  expect_null(scored)
})
