# Shared IRT fixtures for the scoring/model tests. testthat sources helper-*.R
# before the test files. These build small synthetic mirt models together with
# the ModelRecord and long-format trial data that the scoring code consumes, so
# the IRT tests run without any network access.

# long-format trial data (one row per run x item) from a response matrix, with
# items appearing in a chosen order. `dataset` is a scalar or a per-run vector.
irt_trials_from_matrix <- function(resp, run_ids, item_uids, item_order, dataset) {
  grid <- expand.grid(run = seq_along(run_ids), item = item_order)
  dataset_col <- if (length(dataset) == 1) dataset else dataset[grid$run]
  data.frame(
    run_id = run_ids[grid$run],
    dataset = dataset_col,
    item_uid = item_uids[grid$item],
    correct = as.logical(resp[cbind(grid$run, grid$item)])
  )
}

# single-group 2PL fixture. 2PL (varying discrimination), not Rasch, on purpose:
# under Rasch the raw score is sufficient for EAP, so permuting item columns
# would not change scores and could not exercise the column-alignment logic.
# Items appear in `item_order`, deliberately not the model's order, so the wide
# matrix differs from items(mod_rec).
irt_fixture_2pl <- function(n_persons = 400, item_order = c(3, 1, 5, 2, 6, 4)) {
  skip_if_not_installed("mirt")

  set.seed(123)
  item_uids <- paste0("item", 1:6)
  discrimination <- c(0.6, 1.8, 1.0, 2.2, 0.8, 1.5)
  difficulty <- c(-2, -1.5, -1, 0.5, 1.8, 2.5)
  theta <- rnorm(n_persons)
  probs <- plogis(sweep(outer(theta, difficulty, `-`), 2, discrimination, `*`))
  resp <- matrix(rbinom(length(probs), 1, probs),
                 nrow = n_persons,
                 dimnames = list(NULL, paste0(item_uids, "-1")))

  mod <- mirt::mirt(resp, 1, itemtype = "2PL", verbose = FALSE)
  run_ids <- paste0("run", seq_len(n_persons))
  mod_rec <- modelrecord(mod, run_ids)
  trials <- irt_trials_from_matrix(resp, run_ids, item_uids, item_order, dataset = "all")

  spec <- list(item_task = "test", dataset = "all", model_set = "test",
               subset = "all", itemtype = "2PL", nfact = "f1",
               invariance = "scalar", redivis_source = "test:abcd:v1_0")

  list(mod_rec = mod_rec, trials = trials, resp = resp,
       run_ids = run_ids, item_uids = item_uids, spec = spec)
}

# two-group scalar (Rasch) multigroup fixture, to exercise the
# MultipleGroupClass path in score_irt().
irt_fixture_multigroup <- function(n_per_group = 250) {
  skip_if_not_installed("mirt")

  set.seed(321)
  item_uids <- paste0("m", 1:5)
  difficulty <- c(-1.2, -0.4, 0.2, 0.9, 1.6)
  n_per_group <- rep_len(n_per_group, 2)
  groups <- rep(c("g1", "g2"), times = n_per_group)
  theta <- c(rnorm(n_per_group[1], 0), rnorm(n_per_group[2], 0.4))
  probs <- plogis(outer(theta, difficulty, `-`))
  resp <- matrix(rbinom(length(probs), 1, probs),
                 nrow = length(groups),
                 dimnames = list(NULL, paste0(item_uids, "-1")))

  mod <- mirt::multipleGroup(
    resp, 1, group = groups, itemtype = "Rasch",
    invariance = c("slopes", "intercepts", "free_means", "free_var"),
    verbose = FALSE
  )
  run_ids <- paste0("run", seq_along(groups))
  mod_rec <- modelrecord(mod, run_ids)
  trials <- irt_trials_from_matrix(resp, run_ids, item_uids,
                                   seq_along(item_uids), dataset = groups)

  spec <- list(item_task = "mg", dataset = "all", model_set = "test",
               subset = "all", itemtype = "Rasch", nfact = "f1",
               invariance = "scalar", redivis_source = "test:abcd:v1_0")

  list(mod_rec = mod_rec, trials = trials, resp = resp,
       run_ids = run_ids, groups = groups, spec = spec)
}

# three-group scalar multigroup fixture with sparse groups: 2PL items plus a
# graded item, an item never given to one group, items with only one response
# category within a group, and a graded category that one group never uses.
# Checks that single group models built from group_vals() don't depend on each
# group's own data having all of the model's response categories.
irt_fixture_multigroup_sparse <- function(n_per_group = c(300, 200, 150)) {
  skip_if_not_installed("mirt")

  set.seed(654)
  groups <- rep(c("g1", "g2", "g3"), times = n_per_group)
  theta <- rnorm(length(groups), c(g1 = 0, g2 = 0.5, g3 = -0.4)[groups],
                 c(g1 = 1, g2 = 0.8, g3 = 1.2)[groups])
  discrimination <- c(0.8, 1.4, 1.0, 1.8, 1.2, 0.9)
  difficulty <- c(-1.5, -0.5, 0, 0.6, 1.2, -2.5)
  probs <- plogis(sweep(outer(theta, difficulty, `-`), 2, discrimination, `*`))
  resp <- matrix(rbinom(length(probs), 1, probs), nrow = length(groups))
  # graded item with 3 categories
  graded <- findInterval(theta + rnorm(length(theta), 0, 0.7), c(-0.5, 0.7))
  resp <- cbind(resp, graded)
  colnames(resp) <- paste0("s", 1:7, "-1")
  # s5 never given to g3; s6 always correct in g2; graded item never in top category in g1
  resp[groups == "g3", 5] <- NA
  resp[groups == "g2", 6] <- 1
  resp[groups == "g1" & resp[, 7] == 2, 7] <- 1

  mod <- mirt::multipleGroup(
    resp, 1, group = groups, itemtype = c(rep("2PL", 6), "graded"),
    invariance = c("slopes", "intercepts", "free_means", "free_var"),
    verbose = FALSE
  )
  run_ids <- paste0("run", seq_along(groups))
  mod_rec <- modelrecord(mod, run_ids)

  list(mod_rec = mod_rec, resp = resp, run_ids = run_ids, groups = groups)
}
