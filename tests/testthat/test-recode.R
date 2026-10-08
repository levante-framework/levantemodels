# Unit tests for the trial-recoding functions that make get_trials() output
# scoring-ready. These corrections feed directly into IRT scoring, so pinning
# their behavior guards the scoring pipeline. Inputs are tiny hand-built frames;
# expected values are derived from the function logic.

test_that("recode_slider() thresholds responses and drops out-of-range ones", {
  # item "01_10" -> target "01" -> "0.1", max_value 10. A response is correct
  # when abs(response - 0.1) / 10 < threshold (0.15), i.e. abs(response - 0.1) < 1.5.
  df <- tibble(
    item_group = "slider",
    item = "01_10",
    response = c("0", "5", "20"),  # within, outside, and above max_value
    correct = NA
  )

  out <- recode_slider(df, slider_threshold = 0.15)

  # the response above max_value (20 > 10) is removed
  expect_equal(nrow(out), 2)
  expect_equal(out$correct, c(TRUE, FALSE))
  # original item string is retained (cols_remove = FALSE)
  expect_true("item" %in% names(out))
})

test_that("recode_hf() codes extreme RTs incorrect and labels start/stay/switch", {
  df <- tibble(
    item_task = "hf",
    run_id = "r1",
    item_group = "g",
    trial_number = 1:3,
    item = c("a", "a", "b"),
    rt_numeric = c(150, 1000, 1000),  # first is too fast (<200 ms)
    correct = c(TRUE, TRUE, TRUE)
  )

  out <- recode_hf(df) |> arrange(trial_number)

  expect_equal(out$correct, c(FALSE, TRUE, TRUE))
  expect_equal(out$item_uid, c("hf_g_a_start", "hf_g_a_stay", "hf_g_b_switch"))
})

test_that("recode_wrong_items() rescores against the corrected answer key", {
  df <- tibble(
    item_uid = c("math_subtract_37_24", "math_subtract_37_24", "other"),
    response = c("13", "99", "x"),
    correct = c(NA, NA, TRUE)
  )
  fixes <- tibble(item_uid = "math_subtract_37_24", answer_fixed = "13")

  out <- suppressMessages(recode_wrong_items(df, fixes))

  fixed <- out |> filter(item_uid == "math_subtract_37_24") |> arrange(response)
  expect_equal(fixed$response, c("13", "99"))
  expect_equal(fixed$correct, c(TRUE, FALSE))
  # untouched item passes through unchanged
  expect_true(out |> filter(item_uid == "other") |> pull(correct))
  # the join column is dropped again
  expect_false("answer_fixed" %in% names(out))
})

test_that("recode_wrong_items() adds no row when the fixed item is absent", {
  # data contains none of the wrong-answer items; output must be unchanged and
  # must NOT gain a spurious all-NA row for the missing item bank entry
  df <- tibble(
    item_uid = c("other1", "other2"),
    response = c("a", "b"),
    correct = c(TRUE, FALSE)
  )
  fixes <- tibble(item_uid = "math_subtract_37_24", answer_fixed = "13")

  out <- suppressMessages(recode_wrong_items(df, fixes))

  expect_equal(nrow(out), 2)
  expect_setequal(out$item_uid, c("other1", "other2"))
  expect_false(any(is.na(out$item_uid)))
})

test_that("recode_tom() builds item_uid from story, group, and item", {
  df <- tibble(
    item_task = "tom",
    item_original = "3_belief_story",
    item_group = "grp",
    item = "q1"
  )

  out <- recode_tom(df)

  expect_equal(as.character(out$item_uid), "tom_story3_grp_q1")
})

test_that("recode_trials() orchestrates recodes and backfills chance", {
  df <- tibble(
    item_task = "math",
    item_group = "number",
    item = c("x", "s"),
    item_uid = c("math_add_1_1", "math_subtract_37_24"),
    item_original = c("x", "s"),
    correct = c(TRUE, FALSE),
    response = c("2", "13"),
    rt_numeric = c(1000, 1000),
    run_id = "r1",
    trial_number = 1:2,
    chance = NA_real_,
    dataset = "d",
    timestamp = as.POSIXct(c("2025-03-01", "2025-03-01"))
  )

  out <- suppressMessages(recode_trials(df))

  # original correctness is preserved before recoding
  orig <- out |> arrange(item_uid)
  expect_equal(orig$original_correct, c(TRUE, FALSE))
  # math_subtract_37_24 is rescored from FALSE to TRUE against the fixed key
  expect_true(out |> filter(item_uid == "math_subtract_37_24") |> pull(correct))
  # non-slider chance is backfilled to 0
  expect_true(all(out$chance == 0))
})

test_that("recode_trials() refuses to recode already-recoded data", {
  df <- tibble(
    item_task = "math",
    item_group = "number",
    item = c("x", "s"),
    item_uid = c("math_add_1_1", "math_subtract_37_24"),
    item_original = c("x", "s"),
    correct = c(TRUE, FALSE),
    response = c("2", "13"),
    rt_numeric = c(1000, 1000),
    run_id = "r1",
    trial_number = 1:2,
    chance = NA_real_,
    dataset = "d",
    timestamp = as.POSIXct(c("2025-03-01", "2025-03-01"))
  )

  once <- suppressMessages(recode_trials(df))
  # recoding the already-recoded output (which now has original_correct) errors
  expect_error(recode_trials(once), "already been applied")
})
