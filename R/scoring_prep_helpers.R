#' Recode trial correctness for scoring
#'
#' `recode_trials()` applies the task-specific corrections that make
#' [get_trials()] output scoring-ready, and **must be applied before IRT
#' scoring**: the calibrated models were fit on recoded trials, so scoring raw
#' `get_trials()` output yields subtly wrong values. Corrections include slider
#' thresholding (a response is correct within `slider_threshold` of the target,
#' with chance set to `1 / slider_threshold / 100`), Hearts & Flowers RT and
#' start/stay/switch coding, Same/Different Selection and Theory of Mind
#' recodes, known item-key fixes (e.g. `math_subtract_37_24`), and chance-level
#' backfill. See `vignette("scoring-and-model-registry")`.
#'
#' @export
#'
#' @param df trial data
#' @param slider_threshold max normalized distance from slider target
#' @returns A data frame of recoded, scoring-ready trials.
recode_trials <- \(df, slider_threshold = 0.15) {
  # recode_trials() is not idempotent: re-applying it double-applies item-id
  # transforms (e.g. the Hearts & Flowers start/stay/switch suffixes), silently
  # corrupting item_uids. It adds an `original_correct` column, so guard against
  # being run on already-recoded data.
  if ("original_correct" %in% names(df)) {
    stop("recode_trials() has already been applied to this data (an ",
         "`original_correct` column is present); apply it exactly once.",
         call. = FALSE)
  }

  item_fixes <- tribble(
    ~item_uid,             ~answer_fixed,
    "math_subtract_37_24", "13"
  )

  # recode correctness for HF, SDS, math slider items, and items with wrong answers
  df |>
    mutate(original_correct = .data$correct, .after = "correct") |>
    recode_hf() |>
    recode_sds() |>
    recode_wrong_items(item_fixes) |>
    recode_slider(slider_threshold) |>
    recode_tom() |>
    # set chance values for slider items accordingly
    mutate(chance = if_else(.data$item_group == "slider", 1 / slider_threshold / 100, .data$chance),
           chance = .data$chance |> tidyr::replace_na(0))
}

#' Identify the Hearts & Flowers form of each run
#'
#' H&F v2 (core-tasks #442, shipped 2026-04-21) adds response windows and
#' feedback, gated on a variant parameter that isn't exported to Redivis. A
#' variant is v2 if any of its runs has more than 32 mixed-block trials (v1 has
#' 32, v2 48) or its first run is on or after `v2_start`, since every variant
#' created from 2026-04-23 on runs v2. (On the four pilot datasets as of
#' 2026-11 the two signals agree on every variant.)
#'
#' @param runs run data with columns `run_id`, `task_id`, `variant_id`,
#'   `time_started`
#' @param trials trial data with columns `run_id`, `item_task`, `item_group`
#' @param v2_start first date of v2 variants
#' @returns tibble with columns `run_id` and `hf_form` (`"v1"` or `"v2"`), one
#'   row per H&F run
#' @export
hf_forms <- \(runs, trials, v2_start = "2026-04-23") {
  n_mixed <- trials |>
    filter(.data$item_task == "hf", .data$item_group == "heartsflowers") |>
    count(.data$run_id, name = "n_mixed")
  runs |>
    filter(.data$task_id == "hearts-and-flowers") |>
    distinct(.data$run_id, .data$variant_id, .data$time_started) |>
    left_join(n_mixed, by = "run_id") |>
    mutate(v2 = any(.data$n_mixed > 32, na.rm = TRUE) |
             min(.data$time_started) >= as.POSIXct(v2_start, tz = "UTC"),
           .by = "variant_id") |>
    transmute(.data$run_id, hf_form = if_else(.data$v2, "v2", "v1"))
}

#' Recode correctness + reclassify items for HF
#'
#' Codes each trial's item as start, stay, or switch within its block and codes
#' responses faster than 200 ms or slower than the trial's window as incorrect.
#' The window is 2000 ms, except in the mixed block of H&F v2, where it is the
#' form's response window: 3000, 2000, and 1500 ms for mixed trials 1-16,
#' 17-32, and 33-48. v2 mixed-block items are form-specific and split by
#' window (e.g. `hf_heartsflowers_heart_switch_v2_1500ms`) and get
#' `free_guess = TRUE`, so that their lower asymptote is estimated (a missed
#' window scores 0, not chance); the pure-block items are shared by both forms.
#'
#' @inheritParams recode_trials
#' @param df trial data; H&F trials with an `hf_form` column (see [hf_forms()])
#'   are recoded by form, and without one are all treated as v1
#' @export
recode_hf <- \(df) {
  message("Recoding hearts and flowers trials")
  hf_trials <- df |> filter(.data$item_task == "hf")
  if (nrow(hf_trials) == 0) return(df |> mutate(free_guess = FALSE))
  if (!"hf_form" %in% names(hf_trials)) hf_trials$hf_form <- "v1"
  hf_trials <- hf_trials |>
    # recode items based on whether they're same as previous item, and number
    # trials within their block
    group_by(.data$run_id, .data$item_group) |>
    arrange(.data$trial_number) |>
    mutate(hf_type = case_when(
      is.na(lag(.data$item)) ~ "start",
      .data$item == lag(.data$item) ~ "stay",
      .data$item != lag(.data$item) ~ "switch"),
      block_pos = .data$trial_number - min(.data$trial_number) + 1) |>
    ungroup() |>
    mutate(v2_mixed = .data$hf_form %in% "v2" & .data$item_group == "heartsflowers",
           window = case_when(!.data$v2_mixed ~ 2000,
                              .data$block_pos <= 16 ~ 3000,
                              .data$block_pos <= 32 ~ 2000,
                              TRUE ~ 1500)) |>
    # code too fast/slow RTs as incorrect
    mutate(response_fast = .data$rt_numeric < 200, response_slow = .data$rt_numeric > .data$window,
           correct = .data$correct & !.data$response_fast & !.data$response_slow) |>
    mutate(item = paste(.data$item, .data$hf_type, sep = "_"),
           item_uid = paste("hf", .data$item_group, .data$item, sep = "_"),
           item_uid = if_else(.data$v2_mixed, paste0(.data$item_uid, "_v2_", .data$window, "ms"), .data$item_uid),
           free_guess = .data$v2_mixed) |>
    select(-"hf_type", -"block_pos", -"v2_mixed", -"window", -"response_fast", -"response_slow")

  df |>
    filter(.data$item_task != "hf") |>
    mutate(free_guess = FALSE) |>
    bind_rows(hf_trials)
}

#' Recode correctness for slider
#'
#' @inheritParams recode_trials
#' @export
recode_slider <- \(df, slider_threshold) {
  message("Recoding math slider trials")
  slider_trials <- df |>
    filter(.data$item_group == "slider") |>
    # get target and max values out of item
    tidyr::separate_wider_delim(cols = "item", "_",
                                names = c("target", "max_value"),
                                cols_remove = FALSE) |>
    # convert target and max values to numeric and compute if within threshold
    mutate(target = .data$target |> stringr::str_replace("^0", "0."),
           across(c("target", "max_value"), as.numeric),
           correct = (abs(as.numeric(.data$response) - .data$target) / .data$max_value < slider_threshold)) |>
    # remove trials where response greater than max value (must be from a bug)
    filter(as.numeric(.data$response) <= .data$max_value) |>
    select(-c("target", "max_value"))
  df |>
    filter(.data$item_group != "slider") |>
    bind_rows(slider_trials)
}

#' Recode correctness for items with wrong answers
#'
#' @inheritParams recode_trials
#' @param wrong_items tibble with columns item_uid and answer_fixed
#' @export
recode_wrong_items <- \(df, wrong_items) {
  # inner_join (not right_join): only recode wrong-answer items that are
  # actually present. right_join would keep unmatched wrong_items keys, adding
  # a spurious all-NA trial row when an item bank entry isn't in the data.
  wrong_trials <- df |>
    inner_join(wrong_items, by = "item_uid") |>
    mutate(correct = !is.na(.data$response) & .data$response == .data$answer_fixed)
  df |>
    anti_join(wrong_items, by = "item_uid") |>
    bind_rows(wrong_trials) |>
    select(-"answer_fixed")
}

#' Recode items for ToM
#'
#' @inheritParams recode_trials
#' @export
recode_tom <- \(df) {
  message("Recoding theory of mind trials")
  tom <- df |> filter(.data$item_task == "tom")
  tom_disagg <- tom |>
    mutate(story = stringr::str_extract(.data$item_original, "^[0-9]+"),
           item_uid = glue::glue("{item_task}_story{story}_{item_group}_{item}")) |>
    select(-"story")
  df |>
    filter(.data$item_task != "tom") |>
    bind_rows(tom_disagg)
}

#' Recode correctness for SDS
#'
#' @inheritParams recode_trials
#' @export
recode_sds <- function(df) {

  message("Recoding same different selection trials")
  # subset to SDS data and remove known brokenness
  sds_data <- df |>
    filter(.data$item_task == "sds") |>
    filter_out(stringr::str_detect(.data$response, "mittel|rote|gelb|blau|gr\u00FCn")) |>
    filter_out(.data$dataset == "pilot_western_ca_main" & .data$timestamp < "2025-02-21")

  non_sds <- df |> filter_out(.data$item_task == "sds")
  # escape hatch if no valid data remains
  if (nrow(sds_data) == 0) return(non_sds)

  # separate out non-match blocks (dimensions and same)
  sds_dimensions <- sds_data |> filter(.data$item_group == "dimensions")
  sds_same <- sds_data |> filter(.data$item_group == "same")
  # some datasets have no "same" test trials
  if (nrow(sds_same) == 0) {
    sds_same_coded <- tibble()
  } else {
    # code same block for match dimension
    sds_same_items <- sds_same |>
      distinct(.data$answer, .data$distractors) |>
      mutate(opts_parsed = .data$distractors |> purrr::map(parse_response) |> purrr::map(sort)) |>
      mutate(opts_dims = .data$opts_parsed |> purrr::map(code_dims) |> purrr::map(same_dim)) |>
      filter(purrr::map(.data$opts_dims, length) == 1) |>
      mutate(dims = unlist(.data$opts_dims)) |>
      mutate(item_uid = glue::glue("sds_same_{dims}")) |>
      select("answer", "distractors", "item_uid")
    sds_same_coded <- sds_same |> select(-"item_uid") |>
      inner_join(sds_same_items, by = c("answer", "distractors"))
  }
  sds_intro <- bind_rows(sds_dimensions, sds_same_coded)

  non_match <- non_sds |> bind_rows(sds_intro)

  # figure out which responses correspond to each trial
  sds_match_indexed <- sds_data |>
    filter(stringr::str_detect(.data$item_group, "match")) |>
    # later choices in a trial are prompted "...the same in a different way..."
    # (to 2026-07) or "...the same in a new way..." (from 2026-08)
    mutate(different = stringr::str_detect(.data$item_original, "different|new way")) |>
    group_by(.data$run_id, .data$item_group) |>
    arrange(.data$timestamp, .by_group = TRUE) |>
    # use positions of "choice1" to infer a trial index grouping choices together
    mutate(trial_index = if_else(.data$item == "choice1", 1, 0)) |>
    mutate(trial_index = cumsum(.data$trial_index)) |>
    # use positions of "different" prompt to infer trial index
    mutate(trial_index_s = as.numeric(!.data$different)) |>
    mutate(trial_index_s = cumsum(.data$trial_index_s)) |>
    ungroup()

  # remove variously non-compliant trials
  sds_match <- sds_match_indexed |>
    filter(.data$trial_index != 0) |>
    # remove blocks that have any mis-indexed trials
    group_by(.data$run_id, .data$item_group) |>
    filter(all(.data$trial_index == .data$trial_index_s)) |>
    # remove trials if they have any missing or invalid responses, or too few/many rows
    # e.g. only 2 rows for 3match
    mutate(match_k = stringr::str_extract(.data$item_group, "^.") |> as.numeric()) |>
    group_by(.data$run_id, .data$item_group, .data$trial_index) |>
    filter_out(any(.data$response == "{}")) |>
    filter_out(any(stringr::str_count(.data$response, ":") != 2)) |>
    filter(n() == unique(.data$match_k)) |>
    # remove trials that don't have consistent response options for every response
    filter(n_distinct(.data$distractors) == 1) |>
    # recreate choice number
    mutate(choice_i = seq_len(n())) |>
    ungroup() |>
    select("run_id", "trial_index", "item_group", "match_k", "choice_i", "trial_id",
           "item", resp = "response", opts = "distractors", "correct", "original_correct")

  # escape hatch if no valid data remains
  if (nrow(sds_match) == 0) return(non_match)

  # code each distinct response + options pair once (many trials share them)
  sds_pairs <- sds_match |>
    distinct(.data$resp, .data$opts) |>
    # parse response and options strings into vectors of stimuli
    mutate(resp_parsed = .data$resp |> purrr::map(parse_response) |> purrr::map(sort),
           opts_parsed = .data$opts |> purrr::map(parse_response) |> purrr::map(sort)) |>
    # code dimension values for each stimulus in response and options
    mutate(resp_coded = purrr::map(.data$resp_parsed, code_dims),
           opts_coded = purrr::map(.data$opts_parsed, code_dims)) |>
    # match response and options to figure out set of possible matches
    mutate(opts_dims = purrr::map(.data$opts_coded, match_opts_dims),
           resp_dims = purrr::map2(.data$resp_coded, .data$opts_dims, match_resp_dims)) |>
    mutate(n_matches = purrr::map_int(.data$opts_dims, sum)) |>
    mutate(resp_norm = purrr::map_chr(.data$resp_parsed, \(s) paste(s, collapse = " "))) |>
    mutate(subtrial_match = purrr::map_int(.data$resp_dims, length) > 0) |>
    select("resp", "opts", "n_matches", "resp_norm", "subtrial_match")

  # determine outcomes of each trial – was the response a match + was the response new
  trial_vars <- c("run_id", "item_group", "match_k", "n_matches", "trial_index")
  sds_correct <- sds_match |>
    left_join(sds_pairs, by = c("resp", "opts")) |>
    select(all_of(trial_vars), "trial_id", "choice_i", "resp_norm", "subtrial_match") |>
    mutate(row = row_number()) |>
    # response is new if no earlier response in the same trial was the same
    mutate(new = !duplicated(.data$resp_norm), trial_start = min(.data$row),
           .by = all_of(trial_vars)) |>
    mutate(correct = .data$subtrial_match & .data$new) |>
    # make each trial's rows contiguous, with trials in order of first appearance
    arrange(.data$trial_start, .data$row) |>
    select(-"row", -"trial_start") |>
    filter(.data$match_k == .data$n_matches)

  # determine the item status at each response (counts of previous matches and non-matches)
  # construct item_uid out of block + choice + status
  # set guessing to 0
  sds_correct_itemized <- sds_correct |>
    group_by(.data$run_id, .data$match_k, .data$trial_index) |>
    mutate(prev_matches = lag(cumsum(.data$subtrial_match & .data$new), default = 0),
           prev_nonmatches = lag(cumsum(!.data$subtrial_match & .data$new), default = 0)) |>
    ungroup() |>
    mutate(item_choice = paste0("choice", .data$choice_i),
           item_status = paste0(.data$prev_matches, "m", .data$prev_nonmatches, "n"),
           item_uid = glue::glue("sds_{item_group}_{item_choice}_{item_status}") |> as.character(),
           chance = 0) |> # analyses revealed guessing floors hurt SDS models
    select("trial_id", "correct", "item_uid", "chance")

  # substitute recoded item_uid + correct + chance
  # add back in intro trials
  sds_trials <- sds_data |>
    select(-c("correct", "item_uid", "chance")) |>
    inner_join(sds_correct_itemized, by = "trial_id")

  non_match |> bind_rows(sds_trials)
}
