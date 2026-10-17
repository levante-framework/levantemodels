# Tests for apply_exclusions(): which trials each exclusion row removes. ToM
# uids are story-level after recode_trials(), so a story-less row must match
# every story and a story-level row only its own.

exclusion_trials <- tribble(
  ~item_uid,                              ~dataset, ~language, ~timestamp,
  "tom_story5_reference_reference",       "de",     "de-DE",   "2024-06-30 23:59:59",
  "tom_story11_reference_reference",      "co",     "es-CO",   "2024-07-01 00:00:00",
  "tom_story17_reference_reference",      "ar",     "es-AR",   "2025-01-01 12:00:00",
  "tom_story4_deception_reality_check_2", "de",     "de-DE",   "2024-10-23 10:00:00",
  "tom_story4_deception_reality_check_2", "co",     "es-CO",   "2024-10-24 10:00:00",
  "tom_story10_deception_reality_check_2","de",     "de-DE",   "2024-10-24 10:00:00",
  "math_line_1",                          "de",     "de-DE",   "2024-06-01 10:00:00",
  "math_line_10",                         "co",     "es-CO",   "2024-06-01 10:00:00"
) |>
  mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))

# item_uids of the trials apply_exclusions() removes
excluded_uids <- \(exclusions, trials = exclusion_trials) {
  kept <- suppressMessages(apply_exclusions(trials, exclusions))
  trials |> anti_join(kept, by = names(trials)) |> pull("item_uid")
}

test_that("a story-less ToM row matches that question in every story", {
  ex <- tibble(item_uid = "tom_reference_reference")
  expect_setequal(excluded_uids(ex), c("tom_story5_reference_reference",
                                       "tom_story11_reference_reference",
                                       "tom_story17_reference_reference"))
})

test_that("a story-level ToM row matches only its story", {
  ex <- tibble(item_uid = "tom_story4_deception_reality_check_2")
  expect_equal(excluded_uids(ex), rep("tom_story4_deception_reality_check_2", 2))
})

test_that("a non-ToM row matches its uid exactly", {
  ex <- tibble(item_uid = "math_line_1")
  expect_equal(excluded_uids(ex), "math_line_1")
})

test_that("dataset NA matches all datasets and a dataset matches only itself", {
  ex <- tibble(item_uid = "tom_reference_reference", dataset = c(NA, "co"))
  expect_length(excluded_uids(ex[1, ]), 3)
  expect_equal(excluded_uids(ex[2, ]), "tom_story11_reference_reference")
})

test_that("language scopes a row", {
  ex <- tibble(item_uid = "tom_reference_reference", language = c("es-CO", "es-AR"))
  expect_setequal(excluded_uids(ex), c("tom_story11_reference_reference",
                                       "tom_story17_reference_reference"))
})

test_that("start_date is inclusive and end_date exclusive", {
  from <- tibble(item_uid = "tom_deception_reality_check_2", start_date = as.Date("2024-10-24"))
  expect_equal(excluded_uids(from), c("tom_story4_deception_reality_check_2",
                                      "tom_story10_deception_reality_check_2"))
  expect_equal(excluded_uids(from |> mutate(item_uid = "tom_story4_deception_reality_check_2")),
               "tom_story4_deception_reality_check_2")

  until <- tibble(item_uid = "tom_reference_reference", end_date = as.Date("2024-07-01"))
  expect_equal(excluded_uids(until), "tom_story5_reference_reference")
})

test_that("rows with exclude FALSE or NA are ignored", {
  ex <- tibble(item_uid = c("math_line_1", "math_line_10", "tom_reference_reference"),
               exclude = c(TRUE, FALSE, NA))
  expect_equal(excluded_uids(ex), "math_line_1")
})

test_that("every row applies when the exclude column is absent", {
  ex <- tibble(item_uid = c("math_line_1", "math_line_10"))
  expect_setequal(excluded_uids(ex), c("math_line_1", "math_line_10"))
})

test_that("a row with a blank item_uid matches nothing", {
  trials <- exclusion_trials |> mutate(item_uid = replace(item_uid, 7, NA))
  expect_length(excluded_uids(tibble(item_uid = NA_character_), trials), 0)
})

test_that("a blank scope synced as \"\" or \"NULL\" is an error", {
  expect_error(apply_exclusions(exclusion_trials, tibble(item_uid = "math_line_1", dataset = "")), "blank")
  expect_error(apply_exclusions(exclusion_trials, tibble(item_uid = "math_line_1", language = "NULL")), "blank")
})

test_that("a scope column missing from trials is an error", {
  ex <- tibble(item_uid = "math_line_1", language = "de-DE")
  expect_error(apply_exclusions(exclusion_trials |> select(-"language"), ex), "language")
  # an unused scope needs no column
  expect_no_error(suppressMessages(
    apply_exclusions(exclusion_trials |> select(-"language"), ex |> mutate(language = NA))
  ))
})

test_that("apply_exclusions() reports the number of trials removed", {
  ex <- tibble(item_uid = "tom_reference_reference")
  expect_message(apply_exclusions(exclusion_trials, ex), "Excluding 3 of 8 trials")
})

test_that("score() scores without the excluded trials", {
  fx <- irt_fixture_2pl()
  scoring_table <- tibble::as_tibble(fx$spec)
  local_mocked_bindings(get_model_record = function(spec, registry_dir) fx$mod_rec)
  ex <- tibble(item_uid = "item2", dataset = "all")

  scored <- suppressMessages(
    score("test", "all", fx$trials, runs = NULL, scoring_table = scoring_table,
          registry_dir = NULL, exclusions = ex)
  )
  expected <- suppressMessages(
    score_irt(fx$trials |> filter(item_uid != "item2"), fx$spec, fx$mod_rec)
  )
  unexcluded <- suppressMessages(score_irt(fx$trials, fx$spec, fx$mod_rec))

  expect_equal(scored, expected)
  expect_false(isTRUE(all.equal(scored$score, unexcluded$score)))
})
