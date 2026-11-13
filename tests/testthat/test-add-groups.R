# Tests for add_groups_multigroup(), which estimates new groups' means and
# variances in a scalar multigroup model with everything else fixed.

# nested task data for new groups simulated from the multigroup fixture's items
new_group_data <- \(fx, means, sds, n = 400, seed = 11) {
  set.seed(seed)
  vals <- model_vals(fx$mod_rec)
  d <- vals$value[vals$group == "g1" & vals$name == "d"]
  item_uids <- paste0("m", seq_along(d))
  groups <- rep(names(means), each = n)
  theta <- rnorm(length(groups), means[groups], sds[groups])
  resp <- matrix(rbinom(length(groups) * length(d), 1, plogis(outer(theta, d, `+`))),
                 nrow = length(groups))
  run_ids <- paste0("new_run", seq_along(groups))
  trials <- irt_trials_from_matrix(resp, run_ids, item_uids, seq_along(d), dataset = groups)
  list(task_data = tidyr::nest(tibble::tibble(item_task = "mg", trials), data = -"item_task"),
       resp = resp, groups = groups, run_ids = run_ids)
}

# new group data subset to some groups
only <- \(new, groups) {
  new$task_data$data[[1]] <- subset(new$task_data$data[[1]], dataset %in% groups)
  new
}

add_groups <- \(mod_rec, new, ...) {
  add_groups_multigroup(mod_rec, new$task_data, task = "mg", group = "dataset", ...)
}

test_that("add_groups_multigroup() leaves the existing groups unchanged", {
  fx <- irt_fixture_multigroup()
  new <- new_group_data(fx, means = c(g3 = -0.5), sds = c(g3 = 0.7))
  ext <- add_groups(fx$mod_rec, new)

  expect_identical(ext@group_names, c("g1", "g2", "g3"))
  base_vals <- model_vals(fx$mod_rec)
  expect_identical(model_vals(ext)[seq_len(nrow(base_vals)), ], base_vals)
  base_scores <- scores(ext)[seq_along(fx$run_ids), ]
  expect_identical(base_scores$run_id, scores(fx$mod_rec)$run_id)
  expect_equal(base_scores$ability, scores(fx$mod_rec)$ability, tolerance = 1e-8)
})

test_that("add_groups_multigroup() matches a fit to the new group's data alone", {
  fx <- irt_fixture_multigroup()
  new <- new_group_data(fx, means = c(g3 = -0.5), sds = c(g3 = 0.7))
  ext <- add_groups(fx$mod_rec, new)

  # single group model with the fixed items and a free mean and variance
  pars <- group_vals(fx$mod_rec, "g2")
  pars$est <- pars$name %in% c("MEAN_1", "COV_11")
  resp <- new$resp
  colnames(resp) <- items(fx$mod_rec)
  alone <- mirt::mirt(resp, pars = pars, verbose = FALSE)
  alone_pars <- mirt::coef(alone)$GroupPars

  added <- ext@fit$added_groups
  expect_equal(added$mean, alone_pars[, "MEAN_1"], tolerance = 1e-3)
  expect_equal(added$var, alone_pars[, "COV_11"], tolerance = 1e-3)
  # and recovers the generating distribution
  expect_lt(abs(added$mean - -0.5), 4 * added$mean_se)
  expect_lt(abs(added$var - 0.49), 4 * added$var_se)
  expect_identical(added$n_runs, 400)
  expect_identical(added$n_items, 5L)
})

test_that("add_groups_multigroup() gives the same estimates together or one at a time", {
  fx <- irt_fixture_multigroup()
  new <- new_group_data(fx, means = c(g3 = -0.5, g4 = 0.8), sds = c(g3 = 0.7, g4 = 1.2))
  together <- add_groups(fx$mod_rec, new)
  one_at_a_time <- fx$mod_rec |> add_groups(only(new, "g3")) |> add_groups(only(new, "g4"))

  expect_identical(one_at_a_time@group_names, together@group_names)
  expect_equal(one_at_a_time@fit$added_groups, together@fit$added_groups, tolerance = 1e-3)
  expect_equal(model_vals(one_at_a_time)$value, model_vals(together)$value, tolerance = 1e-3)
})

test_that("score_irt() scores an added group under its own prior", {
  fx <- irt_fixture_multigroup()
  new <- new_group_data(fx, means = c(g3 = -0.5), sds = c(g3 = 0.7))
  ext <- add_groups(fx$mod_rec, new)

  scored <- suppressMessages(score_irt(new$task_data$data[[1]], fx$spec, ext))
  pooled <- suppressMessages(score_irt(new$task_data$data[[1]], fx$spec, fx$mod_rec))
  joined <- merge(scored, scores(ext), by = "run_id")

  expect_equal(nrow(joined), 400)
  expect_equal(joined$score, joined$ability, tolerance = 1e-6)
  # the pooled prior pulls the low-scoring group's scores up
  expect_gt(mean(pooled$score), mean(scored$score))
  # the extended record also reconstructs as a multigroup model
  expect_s4_class(model_from_record(ext), "MultipleGroupClass")
})

test_that("add_groups_multigroup() refuses existing groups, small groups, and non-scalar models", {
  fx <- irt_fixture_multigroup()
  new <- new_group_data(fx, means = c(g3 = -0.5), sds = c(g3 = 0.7))

  existing <- new
  existing$task_data$data[[1]]$dataset <- "g2"
  expect_error(add_groups(fx$mod_rec, existing), "already in the model: g2")
  expect_error(add_groups(fx$mod_rec, new, min_runs = 500), "fewer than 500 runs: g3")
  metric <- fx$mod_rec
  metric@invariance <- invariances$metric
  expect_error(add_groups(metric, new), "scalar multigroup")
})
