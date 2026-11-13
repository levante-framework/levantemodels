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

# H&F run with a pure hearts block and a 48-trial mixed block, all correct
hf_run <- \(run_id, rt_mixed, hf_form = NULL) {
  df <- tibble(
    item_task = "hf", run_id = run_id,
    item_group = c(rep("hearts", 2), rep("heartsflowers", 48)),
    trial_number = 1:50,
    item = c("heart", "heart", rep(c("heart", "flower"), 24)),
    rt_numeric = c(2500, 1000, rt_mixed),
    correct = TRUE
  )
  if (!is.null(hf_form)) df$hf_form <- hf_form
  df
}

test_that("recode_hf() codes v2 mixed trials by window and form-specific items", {
  # mixed trials 1-16 have a 3 s window, 17-32 2 s, 33-48 1.5 s
  rt_mixed <- rep(c(2500, 1800, 1000), each = 16)
  v2 <- suppressMessages(recode_hf(hf_run("r2", rt_mixed, "v2"))) |> arrange(trial_number)
  mixed <- v2 |> filter(item_group == "heartsflowers")

  # 2.5 s is within the 3 s window, 1.8 s within the 2 s window
  expect_true(all(mixed$correct))
  expect_equal(unique(stringr::str_extract(mixed$item_uid, "v2_[0-9]+ms")),
               c("v2_3000ms", "v2_2000ms", "v2_1500ms"))
  expect_equal(mixed$item_uid[1:2], c("hf_heartsflowers_heart_start_v2_3000ms",
                                      "hf_heartsflowers_flower_switch_v2_3000ms"))
  expect_true(all(mixed$free_guess))
  # pure blocks keep the shared items and the 2 s cutoff
  pure <- v2 |> filter(item_group == "hearts")
  expect_equal(pure$item_uid, c("hf_hearts_heart_start", "hf_hearts_heart_stay"))
  expect_equal(pure$correct, c(FALSE, TRUE))
  expect_false(any(pure$free_guess))

  # a response slower than a trial's window is incorrect
  slow <- suppressMessages(recode_hf(hf_run("r3", rep(c(3200, 2200, 1700), each = 16), "v2")))
  expect_false(any(slow$correct[slow$item_group == "heartsflowers"]))
})

test_that("recode_hf() keeps the 2 s cutoff and shared items for v1 runs", {
  rt_mixed <- rep(c(2500, 1800, 1000), each = 16)
  v1 <- suppressMessages(recode_hf(hf_run("r1", rt_mixed, "v1"))) |> arrange(trial_number)
  mixed <- v1 |> filter(item_group == "heartsflowers")
  expect_equal(mixed$correct, rt_mixed <= 2000)
  expect_false(any(grepl("v2", mixed$item_uid)))
  expect_false(any(v1$free_guess))

  # without an hf_form column every run is treated as v1
  no_form <- suppressMessages(recode_hf(hf_run("r1", rt_mixed))) |> arrange(trial_number)
  expect_equal(no_form$correct, v1$correct)
  expect_equal(no_form$item_uid, v1$item_uid)
})

test_that("recode_hf() gives other tasks' trials free_guess = FALSE", {
  df <- bind_rows(hf_run("r1", rep(1000, 48), "v2"),
                  tibble(item_task = "math", run_id = "m1", item_group = "g",
                         trial_number = 1L, item = "x", rt_numeric = 1000, correct = TRUE))
  out <- suppressMessages(recode_hf(df))
  expect_false(out$free_guess[out$item_task == "math"])
})

test_that("hf_forms() marks a variant v2 by its mixed-block length or start date", {
  runs <- tibble(
    run_id = c("a1", "a2", "b1", "c1"),
    task_id = "hearts-and-flowers",
    variant_id = c("A", "A", "B", "C"),
    time_started = as.POSIXct(c("2026-01-10", "2026-05-01", "2025-12-01", "2026-06-01"), tz = "UTC")
  )
  # variant A has one 48-trial run; variant C was first run after v2 shipped
  # and its run stopped early
  trials <- tibble(
    item_task = "hf",
    run_id = c(rep("a1", 32), rep("a2", 48), rep("b1", 32), rep("c1", 10)),
    item_group = "heartsflowers"
  )
  forms <- hf_forms(runs, trials) |> arrange(run_id)
  expect_equal(forms$hf_form, c("v2", "v2", "v1", "v2"))
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

test_that("recode_trials() sets slider chance to random-placement probability", {
  df <- tibble(
    item_task = "math",
    item_group = "slider",
    # targets at 0.5, 0 and 0.9 of the line
    item = c("05_1", "0_10", "900_1000"),
    item_uid = paste0("math_slider_", c("05_1", "0_10", "900_1000")),
    item_original = c("05_1", "0_10", "900_1000"),
    correct = TRUE,
    response = c("0.5", "0", "900"),
    rt_numeric = 1000,
    run_id = "r1",
    trial_number = 1:3,
    chance = NA_real_,
    dataset = "d",
    timestamp = as.POSIXct("2025-03-01")
  )

  out <- suppressMessages(recode_trials(df, slider_threshold = 0.15)) |>
    arrange(trial_number)

  # window inside the line, clipped to one half, partly clipped
  expect_equal(out$chance, c(0.3, 0.15, 0.25))
  expect_true(all(out$correct))
})
