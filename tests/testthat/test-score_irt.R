# Regression test for the score_irt() column-order bug.
#
# mirt::fscores(mod, response.pattern = data) matches the columns of
# response.pattern to the model's items *by position, not by name* (verified:
# reordering named columns changes the returned theta). score_irt() must
# therefore reorder its data to items(mod_rec) before scoring. If it does not,
# each response vector is scored against a permuted item set, producing wrong
# (but plausible-looking) abilities.
#
# The fixture (irt_fixture_2pl, in helper-irt.R) uses a 2PL model with varying
# discrimination: under a Rasch model the raw score is sufficient for EAP, so
# permuting columns of complete data would not change the score and could not
# detect the bug.

test_that("score_irt() reproduces the model's calibration EAP scores", {
  fx <- irt_fixture_2pl()
  scored <- suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec))
  gold <- scores(fx$mod_rec)
  joined <- merge(scored, gold, by = "run_id")
  expect_equal(joined$score, joined$ability, tolerance = 0.05)
})

test_that("score_irt() is invariant to the item column order of its input", {
  fx <- irt_fixture_2pl()
  trials_fwd <- fx$trials[order(fx$trials$item_uid), ]
  trials_rev <- fx$trials[order(fx$trials$item_uid, decreasing = TRUE), ]
  scored_fwd <- suppressMessages(score_irt(trials_fwd, fx$spec, fx$mod_rec))
  scored_rev <- suppressMessages(score_irt(trials_rev, fx$spec, fx$mod_rec))
  joined <- merge(scored_fwd, scored_rev, by = "run_id")
  expect_equal(joined$score.x, joined$score.y, tolerance = 1e-8)
})

test_that("score_irt() respects the method argument", {
  fx <- irt_fixture_2pl()
  scored_eap <- suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec))
  scored_ml  <- suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec, method = "ML"))
  joined <- merge(scored_eap, scored_ml, by = "run_id")
  expect_false(isTRUE(all.equal(joined$score.x, joined$score.y)))
  gold <- scores(fx$mod_rec)
  joined_ml <- merge(scored_ml, gold, by = "run_id")
  joined_ml <- joined_ml[is.finite(joined_ml$score), ]
  expect_gt(cor(joined_ml$score, joined_ml$ability), 0.9)
})
