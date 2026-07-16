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

test_that("score_irt() returns NULL for metric/configural models with an unknown group", {
  fx <- irt_fixture_2pl()
  trials_baddataset <- transform(fx$trials, dataset = "not_a_group")

  for (inv in c("metric", "configural")) {
    spec <- modifyList(fx$spec, list(invariance = inv))
    expect_null(suppressMessages(score_irt(trials_baddataset, spec, fx$mod_rec)))
  }
})
