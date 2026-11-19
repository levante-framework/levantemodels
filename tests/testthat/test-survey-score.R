# Tests for survey (CFA) scoring. Everything except the last test runs offline:
# a small two-group, two-factor ordinal CFA is fitted with lavaan on simulated
# data, stored as a scheme-B record in a temporary local registry, and scored
# from there. No real participant data.

survey_fixture <- local({
  fx <- NULL
  function() {
    if (!is.null(fx)) return(fx)
    set.seed(42)
    items <- paste0("x", 1:6)
    groups <- c("uniandes-co", "mpieva-de")
    n <- 300
    sim <- purrr::map_dfr(seq_along(groups), \(g) {
      eta1 <- stats::rnorm(n, mean = c(0, 0.4)[g])
      eta2 <- 0.4 * eta1 + stats::rnorm(n, sd = 0.9)
      ystar <- cbind(sapply(1:3, \(j) 0.8 * eta1 + stats::rnorm(n, sd = 0.6)),
                     sapply(1:3, \(j) 0.8 * eta2 + stats::rnorm(n, sd = 0.6)))
      y <- apply(ystar, 2, \(v) as.integer(cut(v, c(-Inf, -0.8, 0.3, Inf))) - 1L)
      colnames(y) <- items
      tibble::as_tibble(y) |> mutate(site = groups[g])
    })
    model <- "fa =~ x1 + x2 + x3\nfb =~ x4 + x5 + x6"
    fit <- lavaan::cfa(model, data = as.data.frame(sim), ordered = items, group = "site",
                       group.label = groups, estimator = "WLSMV", std.lv = TRUE,
                       group.equal = c("loadings", "thresholds", "intercepts"))
    # raw values 1..3 map to model categories 0..2
    category_map <- tidyr::expand_grid(item = items, raw_value = 1:3) |>
      mutate(model_value = .data$raw_value - 1, source = "observed")
    meta <- list(
      survey_type = "child", model = "toy", items = items, lv = c("fa", "fb"),
      factors = tibble::tibble(lv = c("fa", "fb"), factor = c("Factor one", "Factor two")),
      reverse_flags = tibble::tibble(item = items, reverse_coded = FALSE),
      items_by_lv = list(fa = items[1:3], fb = items[4:6]),
      n_items_by_lv = c(fa = 3L, fb = 3L),
      unit = "respondent", group_var = "site", group_labels = groups, id_group = groups[1],
      invariance = "scalar",
      lavaan_options = list(estimator = "DWLS", parameterization = "delta", missing = "pairwise"),
      category_map = category_map,
      categories = purrr::set_names(purrr::map(items, \(i) 0:2), items),
      reverse_overrides = tibble::tibble(item = character(), reverse_coded = logical()))
    rec <- list(scheme = "B", meta = meta, partable = lavaan::parTable(fit))
    reg <- tempfile("survey_registry_")
    dir.create(file.path(reg, "child"), recursive = TRUE)
    saveRDS(rec, file.path(reg, "child", "toy_cfa_scalar.rds"))
    fx <<- list(fit = fit, sim = sim, rec = rec, registry_dir = normalizePath(reg), items = items)
    fx
  }
})

# long survey data (process_surveys() + link_surveys() shape) from a wide
# matrix of raw values (NA = not answered)
toy_long <- function(raw, team, dataset) {
  ids <- paste0("s", seq_len(nrow(raw)))
  long <- tibble::as_tibble(raw) |> mutate(survey_id = ids) |>
    tidyr::pivot_longer(-"survey_id", names_to = "question", values_to = "value") |>
    mutate(value = as.numeric(.data$value))
  long |> mutate(team = team, dataset = dataset, survey_type = "child", survey_part = "general",
                 respondent_id = paste0("c_", .data$survey_id), child_id = .data$respondent_id,
                 age = 10, survey_timestamp = as.POSIXct("2025-01-01", tz = "UTC"))
}

toy_table <- function(dataset = "pilot_uniandes_co_bogota", team = "pilot_uniandes_co",
                      scoring_group = "uniandes-co", new_site = FALSE) {
  tibble::tibble(survey_type = "child", survey_model = "toy", dataset = dataset, team = team,
                 model_type = "cfa", invariance = "scalar", scoring_group = scoring_group,
                 new_site = new_site, registry_version = "test")
}

toy_items_meta <- function(items) {
  tibble::tibble(survey_type = "student", variable = items, reverse_coded = "FALSE")
}

test_that("single_group_partable() fixes every parameter and puts the model in one group", {
  skip_if_not_installed("lavaan")
  fx <- survey_fixture()
  p <- single_group_partable(fx$rec$partable, 2)
  expect_true(all(p$free == 0L))
  expect_true(all(p$group == 1L) && all(p$block == 1L))
  expect_false(any(p$op == "=="))
  expect_equal(p$ustart, fx$rec$partable$est[fx$rec$partable$group == 2])
})

test_that("scorer_B() reproduces lavPredict(EBM) of the fitted model for each group", {
  skip_if_not_installed("lavaan")
  fx <- survey_fixture()
  ebm <- lavaan::lavPredict(fx$fit, method = "EBM")
  for (g in seq_along(fx$rec$meta$group_labels)) {
    lab <- fx$rec$meta$group_labels[g]
    newdata <- as.data.frame(fx$sim[fx$sim$site == lab, fx$items])
    x <- scorer_B(fx$rec, group = lab, new_site = FALSE, newdata = newdata)
    expect_equal(nrow(x), nrow(newdata))
    expect_equal(unname(x[, c("fa", "fb")]), unname(ebm[[g]][, c("fa", "fb")]), tolerance = 1e-6)
  }
})

test_that("pooled_single_group_partable() uses the equal-weight mixture of the groups", {
  skip_if_not_installed("lavaan")
  fx <- survey_fixture()
  meta <- fx$rec$meta
  pt <- fx$rec$partable
  pooled <- pooled_single_group_partable(pt, meta)
  # scale factors are the ones implied in the theta parameterization
  sc <- pooled[pooled$op == "~*~", ]
  expect_setequal(sc$lhs, fx$items)
  for (v in sc$lhs) {
    lam <- pooled$ustart[pooled$op == "=~" & pooled$rhs == v]
    f <- pooled$lhs[pooled$op == "=~" & pooled$rhs == v]
    th <- pooled$ustart[pooled$op == "~~" & pooled$lhs == v & pooled$rhs == v]
    S <- attr(pooled, "pooled")$cov
    expect_equal(sc$ustart[sc$lhs == v], 1 / sqrt(drop(t(lam) %*% S[f, f, drop = FALSE] %*% lam) + th))
  }
  mu <-sapply(1:2, \(g) pt$est[pt$group == g & pt$op == "~1" & pt$lhs == "fa"])
  v <- sapply(1:2, \(g) pt$est[pt$group == g & pt$op == "~~" & pt$lhs == "fa" & pt$rhs == "fa"])
  expect_equal(pooled$ustart[pooled$op == "~1" & pooled$lhs == "fa"], mean(mu))
  expect_equal(pooled$ustart[pooled$op == "~~" & pooled$lhs == "fa" & pooled$rhs == "fa"],
               mean(v) + mean((mu - mean(mu))^2))
  expect_equal(attr(pooled, "pooled")$mean[["fa"]], mean(mu))
  # loadings are equal across groups, so the pooled loading is the common one
  expect_equal(pooled$ustart[pooled$op == "=~" & pooled$rhs == "x1"],
               pt$est[pt$group == 1 & pt$op == "=~" & pt$rhs == "x1"])
})

test_that("survey_template_rows() has one row per item x category", {
  fx <- survey_fixture()
  tm <- survey_template_rows(fx$rec$meta)
  expect_equal(nrow(tm), 6 * 3)
  expect_equal(names(tm), fx$items)
  expect_true(all(rowSums(!is.na(tm)) == 1))
})

test_that("survey_model_spec_filename() and get_survey_model_spec() find the record row", {
  tbl <- toy_table()
  expect_equal(survey_model_spec_filename(as.list(tbl)), file.path("child", "toy_cfa_scalar.rds"))
  expect_null(get_survey_model_spec("child", "toy", "other_dataset", tbl))
  expect_equal(get_survey_model_spec("child", "toy", tbl$dataset, tbl)$scoring_group, "uniandes-co")
  expect_error(get_survey_model_spec("child", "toy", tbl$dataset, bind_rows(tbl, tbl)), "has 2 rows")
})

test_that("check_survey_scoring_table() stops on table errors", {
  fx <- survey_fixture()
  reg <- fx$registry_dir
  expect_invisible(check_survey_scoring_table(toy_table(), reg))
  expect_error(check_survey_scoring_table(select(toy_table(), -"team"), reg), "lacks columns: team")
  expect_error(check_survey_scoring_table(bind_rows(toy_table(), toy_table()), reg), "duplicated scoring-table rows")
  expect_error(check_survey_scoring_table(mutate(toy_table(), invariance = "metric"), reg), "no model record for table row 1")
  expect_error(check_survey_scoring_table(toy_table(scoring_group = "western-ca"), reg), "is not a group of toy")
  expect_error(check_survey_scoring_table(toy_table(scoring_group = "mpieva-de"), reg), "team pilot_uniandes_co is site uniandes-co")
  expect_error(check_survey_scoring_table(toy_table(team = "pilot_new_xx", scoring_group = "uniandes-co", new_site = TRUE), reg),
               "must have scoring group \"pooled\"")
  expect_error(check_survey_scoring_table(toy_table(scoring_group = "pooled", new_site = TRUE), reg),
               "is a calibration site, not a new site")
})

test_that("score_surveys() scores units, with no_items and no_factor_items exclusions", {
  fx <- survey_fixture()
  raw <- as.matrix(fx$sim[fx$sim$site == "uniandes-co", fx$items][1:5, ]) + 1
  raw[2, ] <- NA                      # answered nothing -> no_items
  raw[3, 4:6] <- NA                   # no fb item answered -> fb no_factor_items
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_bogota")
  # an unanswered survey still has rows in the data (value NA)
  res <- suppressMessages(score_surveys(long, toy_table(), fx$registry_dir, toy_items_meta(fx$items)))
  s <- res$survey_scores
  expect_equal(nrow(s), 5 * 2)
  expect_named(s, c("team", "dataset", "scoring_group", "new_site", "survey_type", "survey_part", "survey_id",
                    "respondent_id", "child_id", "age", "survey_timestamp", "survey_model", "lv", "factor", "score",
                    "n_items_answered", "n_factor_items_answered", "n_items", "exclusion",
                    "score_type", "scoring_model", "registry_version", "scheme"))
  expect_equal(s$exclusion[s$survey_id == "s2"], c("no_items", "no_items"))
  expect_true(all(is.na(s$score[s$survey_id == "s2"])))
  s3 <- s[s$survey_id == "s3", ]
  expect_equal(s3$exclusion, c(NA, "no_factor_items"))
  expect_false(is.na(s3$score[s3$lv == "fa"]))
  expect_true(is.na(s3$score[s3$lv == "fb"]))
  expect_equal(unique(s$registry_version), "test")
  expect_equal(unique(s$score_type), "factor_score_ebm")
  # complete rows equal the scorer applied directly
  x <- scorer_B(fx$rec, "uniandes-co", FALSE, as.data.frame(raw[c(1, 4, 5), ] - 1))
  expect_equal(s$score[s$survey_id %in% c("s1", "s4", "s5")], as.vector(t(x[, c("fa", "fb")])))
  expect_equal(nrow(res$unscored), 0)
})

test_that("a value outside an item's response options stops scoring", {
  fx <- survey_fixture()
  raw <- matrix(2, 3, 6, dimnames = list(NULL, fx$items))
  raw[1, 1] <- 7
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_bogota")
  expect_error(suppressMessages(score_surveys(long, toy_table(), fx$registry_dir, toy_items_meta(fx$items))),
               "values not among the item's response options: x1=7")
})

test_that("a new site is scored under the pooled model", {
  fx <- survey_fixture()
  raw <- matrix(2, 2, 6, dimnames = list(NULL, fx$items))
  long <- toy_long(raw, "pilot_new_xx", "pilot_new_xx_main")
  tbl <- toy_table(dataset = "pilot_new_xx_main", team = "pilot_new_xx", scoring_group = "pooled", new_site = TRUE)
  s <- suppressMessages(score_surveys(long, tbl, fx$registry_dir, toy_items_meta(fx$items)))$survey_scores
  expect_true(all(s$new_site))
  expect_false(anyNA(s$score))
  x <- suppressMessages(scorer_B(fx$rec, "pooled", TRUE, as.data.frame(raw - 1)))
  expect_equal(s$score, as.vector(t(x[, c("fa", "fb")])))
})

test_that("datasets without a table row are listed as unscored", {
  fx <- survey_fixture()
  raw <- matrix(2, 2, 6, dimnames = list(NULL, fx$items))
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_rural")
  res <- suppressMessages(score_surveys(long, toy_table(), fx$registry_dir, toy_items_meta(fx$items)))
  expect_equal(nrow(res$survey_scores), 0)
  expect_equal(res$unscored$reason, "no scoring-table row")
  expect_null(suppressMessages(score_survey("child", "toy", "pilot_uniandes_co_rural", long, toy_table(),
                                            fx$registry_dir, toy_items_meta(fx$items))))
})

test_that("an unknown record scheme stops scoring", {
  fx <- survey_fixture()
  rec <- fx$rec; rec$scheme <- "Z"
  raw <- matrix(2, 2, 6, dimnames = list(NULL, fx$items))
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_bogota")
  expect_error(suppressMessages(score_survey_cfa(long, as.list(toy_table()), rec, toy_items_meta(fx$items))),
               "scorer_Z")
})

test_that("link_survey_scores() copies household scores to each linked child", {
  scores <- tibble::tibble(survey_id = c("h1", "k1"), child_id = c(NA, "c9"), age = c(NA, 8), score = c(0.5, 1))
  surveys <- tibble::tibble(survey_id = c("h1", "h1", "h1", "k1"), child_id = c("c1", "c2", "c2", "c9"),
                            age = c(5, NA, 7, 8))
  out <- link_survey_scores(scores, surveys)
  expect_equal(nrow(out), 3)
  # a household survey with no linked child is kept as it is
  out2 <- link_survey_scores(bind_rows(scores, tibble::tibble(survey_id = "h2", child_id = NA, age = NA, score = 2)), surveys)
  expect_equal(nrow(out2), 4)
  expect_true(is.na(out2$child_id[out2$survey_id == "h2"]))
  expect_false(out2$linked_from_household[out2$survey_id == "h2"])
  hh <- out[out$linked_from_household, ]
  expect_equal(hh$child_id, c("c1", "c2"))
  expect_equal(hh$age, c(5, 7))
  expect_equal(hh$score, c(0.5, 0.5))
  expect_false(out$linked_from_household[out$survey_id == "k1"])
})

test_that("the scoring table and records on Redivis can be fetched and checked", {
  # online test, off by default: set LEVANTEMODELS_TEST_REDIVIS=true to run it
  skip_if_not(identical(Sys.getenv("LEVANTEMODELS_TEST_REDIVIS"), "true"),
              "set LEVANTEMODELS_TEST_REDIVIS=true to run the Redivis test")
  skip_on_cran()
  skip_if_offline()
  # the survey tables exist only from the levante_metadata_scoring release
  # that adds them; set LEVANTEMODELS_TEST_SCORING_VERSION (e.g. "next") to
  # test a draft version
  version <- Sys.getenv("LEVANTEMODELS_TEST_SCORING_VERSION", "current")
  tbl <- tryCatch(fetch_survey_scoring_table(version = version),
                  redivis_not_found_error = \(e) NULL)
  skip_if(is.null(tbl), paste0("no survey scoring table in levante_metadata_scoring version ", version))
  expect_true(all(c("survey_type", "survey_model", "dataset", "scoring_group", "new_site", "redivis_source")
                  %in% names(tbl)))
  reg <- tryCatch(fetch_survey_registry_dir(version = version), redivis_not_found_error = \(e) NULL)
  skip_if(is.null(reg), paste0("no survey model table in levante_metadata_scoring version ", version))
  expect_invisible(check_survey_scoring_table(tbl[1, ], reg))
  expect_equal(get_survey_model_record(as.list(tbl[1, ]), reg)$scheme, "B")
})

# ---- data-preparation paths ---------------------------------------------------

# one unit per row of `raw` (raw values 1..3), scored under uniandes-co
score_toy <- function(raw, items_meta = NULL, rec = NULL) {
  fx <- survey_fixture()
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_bogota")
  suppressMessages(score_survey_cfa(long, as.list(toy_table()), rec %||% fx$rec,
                                    items_meta %||% toy_items_meta(fx$items)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("an item whose reverse flag changed since calibration is flipped back", {
  fx <- survey_fixture()
  raw <- matrix(c(1, 2, 3, 3, 2, 1), 1, 6, dimnames = list(NULL, fx$items))
  ref <- score_toy(raw)
  # the data were processed with x1 reversed (live flag TRUE, calibration FALSE):
  # the processed value 3 (= 1 + 3 - 1) is flipped back to 1, same scores
  raw2 <- raw; raw2[, "x1"] <- 3
  meta_live <- toy_items_meta(fx$items) |> mutate(reverse_coded = if_else(.data$variable == "x1", "TRUE", "FALSE"))
  expect_message(out <- suppressWarnings(score_survey_cfa(toy_long(raw2, "pilot_uniandes_co", "pilot_uniandes_co_bogota"),
                                                          as.list(toy_table()), fx$rec, meta_live)),
                 "reverse_coded flag of x1 changed since calibration")
  expect_equal(out$score, ref$score)
  expect_equal(attr(out, "info")$reverse_flags_changed, "x1")
})

test_that("a model reverse override flips the item once more", {
  fx <- survey_fixture()
  rec <- fx$rec; rec$meta$reverse_overrides <- tibble::tibble(item = "x4", reverse_coded = FALSE)
  raw <- matrix(c(1, 2, 3, 3, 2, 1), 1, 6, dimnames = list(NULL, fx$items))
  raw_flipped <- raw; raw_flipped[, "x4"] <- 1
  expect_equal(score_toy(raw, rec = rec)$score, score_toy(raw_flipped)$score)
})

test_that("flipping stops when the response options are not equally spaced", {
  fx <- survey_fixture()
  rec <- fx$rec
  rec$meta$category_map$raw_value[rec$meta$category_map$item == "x4"] <- c(1, 2, 4)
  rec$meta$reverse_overrides <- tibble::tibble(item = "x4", reverse_coded = FALSE)
  raw <- matrix(c(1, 2, 3, 4, 2, 1), 1, 6, dimnames = list(NULL, fx$items))
  expect_error(score_toy(raw, rec = rec), "not equally spaced")
})

test_that("conflicting answers are dropped and counted; an invalid value in a conflict still stops", {
  fx <- survey_fixture()
  raw <- matrix(c(1, 2, 3, 3, 2, 1), 1, 6, dimnames = list(NULL, fx$items))
  long <- toy_long(raw, "pilot_uniandes_co", "pilot_uniandes_co_bogota")
  dup <- bind_rows(long, long |> filter(.data$question == "x1") |> mutate(value = 2))
  out <- suppressMessages(score_survey_cfa(dup, as.list(toy_table()), fx$rec, toy_items_meta(fx$items)))
  expect_equal(attr(out, "info")$n_conflicting_answers, 1)
  expect_equal(unique(out$n_items_answered), 5)
  bad <- bind_rows(long, long |> filter(.data$question == "x1") |> mutate(value = 99))
  expect_error(suppressMessages(score_survey_cfa(bad, as.list(toy_table()), fx$rec, toy_items_meta(fx$items))),
               "not among the item's response options: x1=99")
})

test_that("listwise models exclude units with any item missing", {
  fx <- survey_fixture()
  rec <- fx$rec; rec$meta$lavaan_options$missing <- "listwise"
  raw <- rbind(c(1, 2, 3, 3, 2, 1), c(1, NA, 3, 3, 2, 1))
  colnames(raw) <- fx$items
  out <- score_toy(raw, rec = rec)
  expect_true(all(is.na(out$score[out$survey_id == "s2"])))
  expect_true(all(out$exclusion[out$survey_id == "s2"] == "incomplete_items"))
  expect_true(all(!is.na(out$score[out$survey_id == "s1"])))
})
