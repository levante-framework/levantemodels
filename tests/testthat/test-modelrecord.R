# Tests for the ModelRecord constructor, accessors, slots, and S4 methods.

test_that("modelrecord() accessors expose the fitted model", {
  fx <- irt_fixture_2pl()
  mr <- fx$mod_rec

  expect_s4_class(mr, "ModelRecord")
  expect_equal(model_class(mr), "SingleGroupClass")
  expect_setequal(items(mr), paste0(fx$item_uids, "-1"))
  expect_s3_class(model_vals(mr), "data.frame")

  sc <- scores(mr)
  expect_named(sc, c("run_id", "ability", "se"))
  expect_equal(nrow(sc), length(fx$run_ids))
  expect_setequal(sc$run_id, fx$run_ids)
})

test_that("ModelRecord @data columns are in items() order", {
  # this is the invariant score_irt() relies on when aligning new data
  fx <- irt_fixture_2pl()
  expect_identical(colnames(fx$mod_rec@data), items(fx$mod_rec))
})

test_that("ModelRecord stores calibration groups for a multigroup model", {
  fx <- irt_fixture_multigroup()
  expect_setequal(fx$mod_rec@group_names, c("g1", "g2"))
  expect_equal(length(fx$mod_rec@groups), length(fx$groups))
})

test_that("AIC/BIC/logLik methods return the stored fit values", {
  fx <- irt_fixture_2pl()
  mr <- fx$mod_rec

  expect_equal(AIC(mr), mr@fit$AIC)
  expect_equal(BIC(mr), mr@fit$BIC)
  expect_equal(logLik(mr), mr@fit$logLik)
  expect_type(AIC(mr), "double")
})

test_that("convergence() reports how estimation ended", {
  fx <- irt_fixture_2pl()
  conv <- convergence(fx$mod_rec)
  expect_named(conv, c("converged", "iterations"))
  expect_true(conv$converged)
  expect_gt(conv$iterations, 0)

  # records made before convergence was stored
  old <- fx$mod_rec
  old@fit <- old@fit[setdiff(names(old@fit), c("converged", "iterations"))]
  expect_true(is.na(convergence(old)$converged))
  expect_true(is.na(convergence(old)$iterations))
})
