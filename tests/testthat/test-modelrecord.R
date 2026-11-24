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

test_that("response_counts() and run_counts() count the calibration data", {
  fx <- irt_fixture_multigroup()
  mr <- fx$mod_rec

  rc <- response_counts(mr)
  expect_named(rc, c("group", "item", "n_responses"))
  expect_equal(nrow(rc), 2 * length(items(mr)))
  expect_equal(sum(rc$n_responses), sum(!is.na(fx$resp)))
  g1_counts <- rc |> filter(group == "g1")
  expect_equal(g1_counts$n_responses[match(items(mr), g1_counts$item)],
               unname(colSums(!is.na(fx$resp[fx$groups == "g1", ]))))

  expect_equal(run_counts(mr), tibble(group = c("g1", "g2"), n_runs = c(250L, 250L)))
})

test_that("response_counts() and run_counts() work on records made before they were stored", {
  fx <- irt_fixture_multigroup()
  old <- fx$mod_rec
  attr(old, "response_counts") <- NULL
  attr(old, "run_counts") <- NULL
  expect_false(methods::.hasSlot(old, "response_counts"))
  expect_equal(response_counts(old), fx$mod_rec@response_counts)
  expect_equal(run_counts(old), fx$mod_rec@run_counts)
})

test_that("sanitize_record() removes calibration data but keeps counts", {
  fx <- irt_fixture_multigroup()
  san <- sanitize_record(fx$mod_rec)

  expect_length(san@runs, 0)
  expect_equal(nrow(scores(san)), 0)
  expect_named(scores(san), names(scores(fx$mod_rec)))
  # one row per response category per group
  expect_equal(nrow(san@data), 2 * 2)
  expect_equal(san@groups, rep(c("g1", "g2"), each = 2))
  expect_identical(colnames(san@data), items(san))

  expect_equal(response_counts(san), response_counts(fx$mod_rec))
  expect_equal(run_counts(san), run_counts(fx$mod_rec))
  expect_equal(mod_coefs(san), mod_coefs(fx$mod_rec))

  # records made before counts were stored get them from the data
  old <- fx$mod_rec
  attr(old, "response_counts") <- NULL
  attr(old, "run_counts") <- NULL
  expect_equal(response_counts(sanitize_record(old)), response_counts(fx$mod_rec))
})

test_that("sanitize_record() keeps every item's response categories", {
  # polytomous item with categories 0, 1, 2 alongside dichotomous items
  set.seed(42)
  resp <- cbind(matrix(rbinom(600, 1, 0.5), ncol = 3), sample(0:2, 200, replace = TRUE))
  colnames(resp) <- paste0("item", 1:4, "-1")
  mod <- mirt::mirt(resp, 1, itemtype = c(rep("2PL", 3), "graded"), verbose = FALSE)
  mr <- modelrecord(mod, paste0("run", 1:200))
  san <- sanitize_record(mr)

  expect_equal(nrow(san@data), 3)
  expect_setequal(san@data[, "item4-1"], 0:2)
  expect_setequal(san@data[, "item1-1"], 0:1)
  expect_equal(mirt::fscores(model_from_record(san), response.pattern = resp[1:20, ], full.scores.SE = TRUE),
               mirt::fscores(model_from_record(mr), response.pattern = resp[1:20, ], full.scores.SE = TRUE))
})

test_that("sanitized records score the same as the originals", {
  fx <- irt_fixture_2pl()
  expect_equal(suppressMessages(score_irt(fx$trials, fx$spec, sanitize_record(fx$mod_rec))),
               suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec)))

  fx <- irt_fixture_multigroup()
  san <- sanitize_record(fx$mod_rec)
  # a group in the model, and a group not in it (pooled distribution)
  g1 <- fx$trials |> filter(dataset == "g1")
  new <- g1 |> mutate(dataset = "g3")
  expect_equal(suppressMessages(score_irt(g1, fx$spec, san)),
               suppressMessages(score_irt(g1, fx$spec, fx$mod_rec)))
  expect_equal(suppressMessages(score_irt(new, fx$spec, san)),
               suppressMessages(score_irt(new, fx$spec, fx$mod_rec)))
  expect_equal(mirt::coef(model_from_record(san)), mirt::coef(model_from_record(fx$mod_rec)))
})
