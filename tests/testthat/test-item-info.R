# Tests for irf(), info(), item_info(), and empirical_reliability() against mirt.

theta_grid <- seq(-4, 4, by = 0.5)

test_that("irf() and info() match mirt for a 3PL item", {
  skip_if_not_installed("mirt")
  a <- 1.3; b <- 0.4; c <- 0.2
  # fix parameters of a 3PL model (same approach as model_from_record())
  set.seed(1)
  resp <- matrix(rbinom(400, 1, 0.6), ncol = 2,
                 dimnames = list(NULL, c("Item_1", "Item_2")))
  vals <- mirt::mirt(resp, 1, itemtype = "3PL", pars = "values")
  vals$est <- FALSE
  vals$value[vals$item == "Item_1" & vals$name == "a1"] <- a
  vals$value[vals$item == "Item_1" & vals$name == "d"] <- -a * b
  vals$value[vals$item == "Item_1" & vals$name == "g"] <- c
  mod <- mirt::mirt(resp, 1, itemtype = "3PL", pars = vals, TOL = NaN, verbose = FALSE)
  item <- mirt::extract.item(mod, 1)
  theta <- matrix(theta_grid)

  expect_equal(irf(theta_grid, a, b, c), mirt::probtrace(item, theta)[, 2])
  expect_equal(info(theta_grid, a, b, c), mirt::iteminfo(item, theta))
})

test_that("item_info() matches mirt::iteminfo() for each item of a fitted model", {
  fx <- irt_fixture_2pl()
  mod <- model_from_record(fx$mod_rec)
  ii <- item_info(fx$mod_rec, theta = theta_grid)

  expect_equal(nrow(ii), length(fx$item_uids) * length(theta_grid))
  for (i in seq_along(items(fx$mod_rec))) {
    item_name <- sub("-1$", "", items(fx$mod_rec)[i])
    expected <- mirt::iteminfo(mirt::extract.item(mod, i), matrix(theta_grid))
    observed <- ii |> filter(.data$item == item_name) |> arrange(.data$theta)
    expect_equal(observed$info, expected, label = item_name)
  }
})

test_that("empirical_reliability() matches mirt::empirical_rxx()", {
  fx <- irt_fixture_2pl()
  mod <- model_from_record(fx$mod_rec)
  expected <- mirt::empirical_rxx(mirt::fscores(mod, full.scores.SE = TRUE,
                                                verbose = FALSE))
  rel <- empirical_reliability(fx$mod_rec)
  expect_equal(rel$group, "all")
  expect_equal(rel$rxx, unname(expected))
})

test_that("empirical_reliability() matches mirt per group and pooled for a multigroup model", {
  fx <- irt_fixture_multigroup()
  mod <- model_from_record(fx$mod_rec)
  fs <- mirt::fscores(mod, full.scores.SE = TRUE, verbose = FALSE)
  rel <- empirical_reliability(fx$mod_rec)

  expect_setequal(rel$group, c("all", "g1", "g2"))
  for (g in c("g1", "g2")) {
    expected <- mirt::empirical_rxx(fs[fx$groups == g, , drop = FALSE])
    expect_equal(rel$rxx[rel$group == g], unname(expected), label = g)
  }
  expect_equal(rel$rxx[rel$group == "all"], unname(mirt::empirical_rxx(fs)))
})
