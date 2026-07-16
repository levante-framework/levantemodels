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

# test_that("score_sre() scores within a 180-trial cap and z-scales", {
#   trials <- tibble(
#     run_id = rep(c("hi", "lo"), each = 4),
#     trial_number = rep(1:4, 2),
#     # "hi" gets all correct, "lo" gets all incorrect
#     correct = c(rep(TRUE, 4), rep(FALSE, 4))
#   )
#
#   out <- suppressMessages(score_sre(trials, dataset = "any")) |> arrange(run_id)
#
#   # z-scaled across two runs -> mean 0, and "hi" > "lo"
#   expect_equal(mean(out$score), 0, tolerance = 1e-8)
#   expect_gt(out$score[out$run_id == "hi"], out$score[out$run_id == "lo"])
#   expect_true(all(out$score_type == "guessing_adjusted_number_correct_scaled"))
# })
#
# test_that("score_sre() ignores trials beyond number 180", {
#   # two runs with distinct within-cap scores (so the z-scaling is defined)
#   base <- tibble(
#     run_id = c("a", "a", "b", "b"),
#     trial_number = c(1, 2, 1, 2),
#     correct = c(TRUE, TRUE, TRUE, FALSE)
#   )
#   # same data plus trials beyond the 180 cap, which should be ignored
#   extra <- bind_rows(base, tibble(
#     run_id = c("a", "b"),
#     trial_number = c(181, 181),
#     correct = c(FALSE, TRUE)
#   ))
#
#   out_base <- suppressMessages(score_sre(base, dataset = "any")) |> arrange(run_id)
#   out_extra <- suppressMessages(score_sre(extra, dataset = "any")) |> arrange(run_id)
#
#   expect_equal(out_extra$score, out_base$score)
# })

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
