# Tests for score_irt()'s branches beyond the simple single-group case:
# multigroup model reconstruction, missing-item backfill, and the handling of
# data whose group is not in the model.

test_that("score_irt() reconstructs a multigroup model and scores one group", {
  fx <- irt_fixture_multigroup()
  g1_trials <- subset(fx$trials, dataset == "g1")

  scored <- suppressMessages(score_irt(g1_trials, fx$spec, fx$mod_rec))
  gold <- scores(fx$mod_rec)
  joined <- merge(scored, gold, by = "run_id")

  expect_equal(nrow(scored), sum(fx$groups == "g1"))
  # extracting the group and scoring its calibration runs reproduces the
  # model's stored EAP scores
  expect_equal(joined$score, joined$ability, tolerance = 0.05)
})

test_that("score_irt() backfills items missing from the data and scores all runs", {
  fx <- irt_fixture_2pl()
  # drop one model item entirely from the trial data
  missing <- subset(fx$trials, item_uid != "item3")

  scored <- suppressMessages(score_irt(missing, fx$spec, fx$mod_rec))

  expect_equal(nrow(scored), length(fx$run_ids))
  expect_true(all(is.finite(scored$score)))
})

test_that("score_irt() falls back to a model group for scalar models", {
  fx <- irt_fixture_2pl()
  # a group not present in the model; scalar invariance -> use the model's group
  trials_baddataset <- transform(fx$trials, dataset = "not_a_group")

  scored <- suppressMessages(score_irt(trials_baddataset, fx$spec, fx$mod_rec))

  expect_equal(nrow(scored), length(fx$run_ids))
})

test_that("score_irt() scores a group not in a scalar multigroup model under its own estimated prior", {
  fx <- irt_fixture_multigroup()
  g2_trials <- subset(fx$trials, dataset == "g2")
  gold <- merge(g2_trials[!duplicated(g2_trials$run_id), "run_id", drop = FALSE],
                scores(fx$mod_rec), by = "run_id")

  # g2's calibration data relabelled as a group the model doesn't have:
  # re-estimating its mean and variance with items fixed recovers g2's
  # calibrated distribution, so its stored EAP scores are reproduced
  new_trials <- transform(g2_trials, dataset = "g_new")
  scored_new <- suppressMessages(score_irt(new_trials, fx$spec, fx$mod_rec))
  joined_new <- merge(scored_new, gold, by = "run_id")
  expect_equal(nrow(joined_new), sum(fx$groups == "g2"))
  expect_equal(joined_new$score, joined_new$ability, tolerance = 0.01)

  # the same data scored under the first group's prior (the old fallback) is
  # further from g2's calibration scores
  g1_prior_trials <- transform(g2_trials, dataset = "g1")
  scored_g1 <- suppressMessages(score_irt(g1_prior_trials, fx$spec, fx$mod_rec))
  joined_g1 <- merge(scored_g1, gold, by = "run_id")
  expect_lt(mean(abs(joined_new$score - joined_new$ability)),
            mean(abs(joined_g1$score - joined_g1$ability)))
})

test_that("score_irt() scores a small group not in a scalar multigroup model using the first group", {
  fx <- irt_fixture_multigroup()
  g2_trials <- subset(fx$trials, dataset == "g2")
  few_runs <- unique(g2_trials$run_id)[1:10]
  few_trials <- subset(g2_trials, run_id %in% few_runs)

  scored_new <- suppressMessages(score_irt(transform(few_trials, dataset = "g_new"), fx$spec, fx$mod_rec))
  scored_g1 <- suppressMessages(score_irt(transform(few_trials, dataset = "g1"), fx$spec, fx$mod_rec))

  expect_equal(scored_new$score, scored_g1$score)
  # including a single run
  one_run <- subset(few_trials, run_id == few_runs[1])
  scored_one <- suppressMessages(score_irt(transform(one_run, dataset = "g_new"), fx$spec, fx$mod_rec))
  expect_equal(nrow(scored_one), 1)
  expect_true(is.finite(scored_one$score))

  # the new group model itself can be built from a single run
  one_resp <- fx$resp[fx$groups == "g2", , drop = FALSE][1, , drop = FALSE]
  mod_one <- suppressMessages(new_group_model(fx$mod_rec, one_resp, "g_new"))
  expect_s4_class(mod_one, "SingleGroupClass")
})

test_that("score_irt() returns NULL for metric/configural models with an unknown group", {
  fx <- irt_fixture_2pl()
  trials_baddataset <- transform(fx$trials, dataset = "not_a_group")

  for (inv in c("metric", "configural")) {
    spec <- modifyList(fx$spec, list(invariance = inv))
    expect_null(suppressMessages(score_irt(trials_baddataset, spec, fx$mod_rec)))
  }
})
