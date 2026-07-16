# Correctness / property tests for the scoring pipeline.
#
# Unlike characterization tests (which pin current behavior), these assert
# properties that *should* hold regardless of implementation, so they can fail
# and reveal real bugs. The original column-order bug corrupted individual theta
# values while barely changing their rank order -- so a "recovered scores
# correlate with the truth" check is a WEAK detector of it (see the coarse
# recovery test below). The sharp detectors are value-level: reproducing the
# model's own calibration EAP scores (see test-score_irt.R) and using the right
# group's prior (below).

test_that("score_irt() uses the data's group prior, not another group's", {
  # This is the multigroup analog of the column-order bug: if the wrong group
  # were extracted, a child would be scored against the wrong prior. Scoring
  # group g2's runs must (a) reproduce g2's calibration scores and (b) give
  # different answers than if the same responses were mislabeled as g1.
  fx <- irt_fixture_multigroup()
  g2 <- subset(fx$trials, dataset == "g2")

  scored_g2 <- suppressMessages(score_irt(g2, fx$spec, fx$mod_rec))
  mislabeled <- suppressMessages(score_irt(transform(g2, dataset = "g1"), fx$spec, fx$mod_rec))

  gold <- merge(scored_g2, scores(fx$mod_rec), by = "run_id")
  expect_gt(cor(gold$score, gold$ability), 0.99)         # correct group reproduces calibration

  cmp <- merge(scored_g2, mislabeled, by = "run_id")
  expect_gt(mean(abs(cmp$score.x - cmp$score.y)), 0.05)   # the group label actually matters
})

test_that("score_irt() ignores items present in the data but not in the model", {
  fx <- irt_fixture_2pl()
  base <- suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec))

  with_extra <- rbind(
    fx$trials,
    data.frame(run_id = fx$run_ids[1], dataset = "all",
               item_uid = "not_a_model_item", correct = TRUE)
  )
  extra <- suppressMessages(score_irt(with_extra, fx$spec, fx$mod_rec))

  joined <- merge(base, extra, by = "run_id")
  expect_equal(nrow(extra), length(fx$run_ids))
  expect_equal(joined$score.x, joined$score.y, tolerance = 1e-8)
})

test_that("score_irt() returns an NA score (not an error) for a run with no model items", {
  fx <- irt_fixture_2pl()
  with_ghost <- rbind(
    fx$trials,
    data.frame(run_id = "ghost_run", dataset = "all",
               item_uid = "not_a_model_item", correct = TRUE)
  )
  scored <- suppressMessages(score_irt(with_ghost, fx$spec, fx$mod_rec))

  ghost <- scored[scored$run_id == "ghost_run", ]
  expect_equal(nrow(ghost), 1)
  expect_true(is.na(ghost$score))
})

test_that("recovered abilities track the true thetas (coarse end-to-end sanity)", {
  # NOTE: this is a COARSE check. Rank correlation with the truth is largely
  # preserved even when items are mis-aligned, so it does NOT reliably catch the
  # column-order class of bug -- the calibration-score match in test-score_irt.R
  # is what does. This guards against gross breakage (e.g. scoring returning
  # noise or a constant).
  skip_if_not_installed("mirt")

  set.seed(7)
  n_items <- 15
  n_persons <- 600
  item_uids <- paste0("i", sprintf("%02d", seq_len(n_items)))
  discrimination <- runif(n_items, 0.8, 2.2)
  difficulty <- seq(-2, 2, length.out = n_items)
  true_theta <- rnorm(n_persons)
  probs <- plogis(sweep(outer(true_theta, difficulty, `-`), 2, discrimination, `*`))
  resp <- matrix(rbinom(length(probs), 1, probs), nrow = n_persons,
                 dimnames = list(NULL, paste0(item_uids, "-1")))

  mod <- mirt::mirt(resp, 1, itemtype = "2PL", verbose = FALSE)
  run_ids <- paste0("run", seq_len(n_persons))
  mod_rec <- modelrecord(mod, run_ids)
  spec <- list(item_task = "t", dataset = "all", model_set = "t", subset = "all",
               itemtype = "2PL", nfact = "f1", invariance = "scalar",
               redivis_source = "t:a:v1")
  grid <- expand.grid(run = seq_len(n_persons), item = seq_len(n_items))
  trials <- data.frame(run_id = run_ids[grid$run], dataset = "all",
                       item_uid = item_uids[grid$item],
                       correct = as.logical(resp[cbind(grid$run, grid$item)]))

  scored <- suppressMessages(score_irt(trials, spec, mod_rec))
  scored <- scored[match(run_ids, scored$run_id), ]

  expect_gt(cor(scored$score, true_theta), 0.8)
  expect_lt(abs(mean(scored$score - true_theta)), 0.1)
})
