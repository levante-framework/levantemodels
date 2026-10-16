# Tests for group_vals(), which builds single group models for the groups of a
# multigroup model in place of mirt::extract.group() on the reconstructed
# multigroup model. mirt::extract.group() is the reference throughout.

group_model <- \(mod_rec, group) {
  mirt::mirt(data = mod_rec@data, pars = group_vals(mod_rec, group), TOL = NaN)
}

extract_group_model <- \(mod_rec, group) {
  mirt::extract.group(model_from_record(mod_rec), group = group)
}

# response patterns to score: the calibration data with some responses removed
score_patterns <- \(resp) {
  set.seed(99)
  resp[sample(length(resp), length(resp) %/% 5)] <- NA
  resp[rowSums(!is.na(resp)) > 0, , drop = FALSE]
}

multigroup_fixtures <- \() {
  list(rasch = irt_fixture_multigroup(), sparse = irt_fixture_multigroup_sparse())
}

test_that("group_vals() gives the same parameters as mirt::extract.group()", {
  par_cols <- c("group", "item", "class", "name", "parnum", "value", "lbound", "ubound", "est")
  for (fx in multigroup_fixtures()) {
    for (g in fx$mod_rec@group_names) {
      ours <- mirt::mod2values(group_model(fx$mod_rec, g))
      theirs <- mirt::mod2values(extract_group_model(fx$mod_rec, g))
      expect_equal(ours[par_cols], theirs[par_cols])
      # the parameters are exactly the record's (reconstructing the multigroup
      # model can change them in the last bit)
      vals <- model_vals(fx$mod_rec)
      expect_identical(ours$value, vals$value[vals$group == g])
    }
  }
})

test_that("group_vals() models give the same EAP scores as mirt::extract.group()", {
  for (fx in multigroup_fixtures()) {
    patterns <- score_patterns(fx$resp)
    for (g in fx$mod_rec@group_names) {
      expect_equal(mirt::fscores(group_model(fx$mod_rec, g), response.pattern = patterns),
                   mirt::fscores(extract_group_model(fx$mod_rec, g), response.pattern = patterns))
    }
  }
})

test_that("group_vals() models give the same MAP and ML scores as mirt::extract.group()", {
  # these scores are found by optimization, which turns last-bit differences in
  # the parameters into differences on the order of the optimizer's tolerance;
  # the warnings are for patterns that don't converge (e.g. all correct for ML)
  for (fx in multigroup_fixtures()) {
    patterns <- score_patterns(fx$resp)
    for (g in fx$mod_rec@group_names) {
      for (method in c("MAP", "ML")) {
        fs <- \(mod) suppressWarnings(mirt::fscores(mod, method = method, response.pattern = patterns))
        expect_equal(fs(group_model(fx$mod_rec, g)), fs(extract_group_model(fx$mod_rec, g)),
                     tolerance = 1e-5)
      }
    }
  }
})

test_that("group_vals() keeps each group's latent mean and variance", {
  fx <- irt_fixture_multigroup_sparse()
  vals <- model_vals(fx$mod_rec)
  for (g in fx$mod_rec@group_names) {
    mod_vals <- mirt::mod2values(group_model(fx$mod_rec, g))
    for (par in c("MEAN_1", "COV_11")) {
      expect_identical(mod_vals$value[mod_vals$name == par],
                       vals$value[vals$group == g & vals$name == par])
    }
  }
  # groups other than the reference group have estimated means and variances
  g3_vals <- mirt::mod2values(group_model(fx$mod_rec, "g3"))
  expect_false(isTRUE(all.equal(g3_vals$value[g3_vals$name == "MEAN_1"], 0)))
  expect_false(isTRUE(all.equal(g3_vals$value[g3_vals$name == "COV_11"], 1)))
})

test_that("group_vals() errors for a group not in the model", {
  fx <- irt_fixture_multigroup()
  expect_error(group_vals(fx$mod_rec, "g_new"))
})

test_that("score_irt() scores one group the same as mirt::extract.group()", {
  fx <- irt_fixture_multigroup()
  for (g in fx$mod_rec@group_names) {
    trials <- subset(fx$trials, dataset == g)
    scored <- suppressMessages(score_irt(trials, fx$spec, fx$mod_rec))
    resp <- fx$resp[fx$groups == g, , drop = FALSE]
    expected <- mirt::fscores(extract_group_model(fx$mod_rec, g), response.pattern = resp)
    expect_identical(scored$run_id, fx$run_ids[fx$groups == g])
    expect_equal(scored$score, unname(expected[, "F1"]))
    expect_equal(scored$score_se, unname(expected[, "SE_F1"]))
  }
})

test_that("pooled_group_model() scores the same as with mirt::extract.group()", {
  for (fx in multigroup_fixtures()) {
    mod_rec <- fx$mod_rec
    # pooled model built from mirt::extract.group() for the reference group
    vals <- model_vals(mod_rec)
    group_par <- \(par) vals$value[match(paste(mod_rec@group_names, par), paste(vals$group, vals$name))]
    means <- group_par("MEAN_1")
    vars <- group_par("COV_11")
    ref_vals <- mirt::mod2values(extract_group_model(mod_rec, mod_rec@group_names[[1]]))
    ref_vals$value[ref_vals$name == "MEAN_1"] <- mean(means)
    ref_vals$value[ref_vals$name == "COV_11"] <- mean(vars) + mean((means - mean(means))^2)
    theirs <- mirt::mirt(data = mod_rec@data, pars = ref_vals, TOL = NaN)

    ours <- suppressMessages(pooled_group_model(mod_rec))
    patterns <- score_patterns(fx$resp)
    expect_equal(mirt::fscores(ours, response.pattern = patterns),
                 mirt::fscores(theirs, response.pattern = patterns))
  }
})
