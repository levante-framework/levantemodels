# Unit tests for the non-IRT scoring paths (CAT, SRE, PA). These are pure
# transforms and don't need a fitted model.

test_that("score_cat() drops runs without a theta estimate and renames", {
  runs <- tibble(
    run_id = c("r1", "r2", "r3"),
    test_comp_theta_estimate = c(0.5, NA, -1.2),
    test_comp_theta_se = c(0.3, NA, 0.4)
  )

  out <- suppressMessages(score_cat(runs))

  expect_equal(out$run_id, c("r1", "r3"))
  expect_equal(out$score, c(0.5, -1.2))
  expect_equal(out$score_se, c(0.3, 0.4))
  expect_true(all(out$score_type == "ability_cat"))
})

test_that("score_sre() z-scales the guessing-adjusted rate within dataset", {
  # 4 trials spanning 40 seconds per run: "hi" all correct, "lo" all incorrect
  trials <- tibble(
    run_id = rep(c("hi", "lo"), each = 4),
    dataset = "any",
    timestamp = rep(as.POSIXct("2025-03-01 12:00:00") + c(0, 10, 20, 40), 2),
    correct = c(rep(TRUE, 4), rep(FALSE, 4))
  )

  out <- suppressMessages(score_sre(trials, dataset = "any"))

  # z-scaled across two runs -> mean 0, and "hi" > "lo"
  expect_equal(mean(out$score), 0, tolerance = 1e-8)
  expect_gt(out$score[out$run_id == "hi"], out$score[out$run_id == "lo"])
  expect_true(all(out$score_type == "guessing_adjusted_rate_scaled"))
})

test_that("score_sre() drops runs shorter than 30 seconds", {
  # "a" and "b" span 40 seconds with distinct net scores; "short" spans only 10
  trials <- tibble(
    run_id = rep(c("a", "b", "short"), each = 2),
    dataset = "any",
    timestamp = as.POSIXct("2025-03-01 12:00:00") + c(0, 40, 0, 40, 0, 10),
    correct = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )

  out <- suppressMessages(score_sre(trials, dataset = "any"))

  expect_equal(sort(out$run_id), c("a", "b"))
})

test_that("score_pa() uses per-dataset trial maxima and drops short runs", {
  trials <- tibble(
    run_id = c(rep("long", 5), rep("short", 2)),
    correct = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )

  out <- suppressMessages(score_pa(trials, dataset = "pilot_uniandes_co_bogota"))

  # "short" run (<= 3 trials) is dropped; "long" has 3 correct / 20 max
  expect_equal(out$run_id, "long")
  expect_equal(out$score, 3 / 20)
  expect_true(all(out$score_type == "prop_correct"))
})

test_that("score_pa() returns NULL for an unknown dataset", {
  trials <- tibble(run_id = rep("r", 5), correct = TRUE)
  expect_null(suppressMessages(score_pa(trials, dataset = "not_a_dataset")))
})
