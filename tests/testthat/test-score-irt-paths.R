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

test_that("score_irt() scores a group not in a scalar multigroup model under the pooled prior", {
  fx <- irt_fixture_multigroup()
  new_trials <- transform(subset(fx$trials, dataset == "g2"), dataset = "g_new")
  scored <- suppressMessages(score_irt(new_trials, fx$spec, fx$mod_rec))

  # independent EAP by quadrature under the mixture of the two groups' priors
  vals <- model_vals(fx$mod_rec)
  par <- \(grp, name) vals$value[vals$group == grp & vals$name == name]
  m <- c(par("g1", "MEAN_1"), par("g2", "MEAN_1"))
  v <- c(par("g1", "COV_11"), par("g2", "COV_11"))
  w <- as.numeric(table(fx$groups)[c("g1", "g2")]) / length(fx$groups)
  pooled_mean <- sum(w * m)
  pooled_var <- sum(w * v) + sum(w * (m - pooled_mean)^2)
  a <- par("g1", "a1")
  d <- par("g1", "d")
  theta <- seq(-8, 8, length.out = 401)
  resp <- fx$resp[fx$groups == "g2", , drop = FALSE]
  eap <- apply(resp, 1, \(x) {
    lik <- vapply(theta, \(t) prod(ifelse(x == 1, plogis(a * t + d), 1 - plogis(a * t + d))), numeric(1))
    post <- lik * dnorm(theta, pooled_mean, sqrt(pooled_var))
    sum(theta * post) / sum(post)
  })
  expected <- data.frame(run_id = fx$run_ids[fx$groups == "g2"], expected = eap)

  joined <- merge(scored, expected, by = "run_id")
  expect_equal(nrow(joined), sum(fx$groups == "g2"))
  expect_equal(joined$score, joined$expected, tolerance = 0.01)
})

test_that("score_irt() scores a single run from a group not in a scalar multigroup model", {
  fx <- irt_fixture_multigroup()
  one_run <- subset(fx$trials, run_id == fx$run_ids[fx$groups == "g2"][1])
  scored <- suppressMessages(score_irt(transform(one_run, dataset = "g_new"), fx$spec, fx$mod_rec))

  expect_equal(nrow(scored), 1)
  expect_true(is.finite(scored$score))
})

test_that("score_irt() returns NULL for metric/configural models with an unknown group", {
  fx <- irt_fixture_2pl()
  trials_baddataset <- transform(fx$trials, dataset = "not_a_group")

  for (inv in c("metric", "configural")) {
    spec <- modifyList(fx$spec, list(invariance = inv))
    expect_null(suppressMessages(score_irt(trials_baddataset, spec, fx$mod_rec)))
  }
})
