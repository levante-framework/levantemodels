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

#' recode correctness + reclassify items for HF
#'
#' @inheritParams recode_trials
recode_hf <- \(df) {
  hf_trials <- df |>
    filter(.data$item_task == "hf") |>
    # code too fast/slow RTs as incorrect
    mutate(response_fast = .data$rt_numeric < 200, response_slow = .data$rt_numeric > 2000,
           correct = .data$correct & !.data$response_fast & !.data$response_slow) |>
    select(-"response_fast", -"response_slow") |>
    # recode items based on whether they're same as previous item
    group_by(.data$run_id, .data$item_group) |>
    arrange(.data$trial_number) |>
    mutate(hf_type = case_when(
      is.na(lag(.data$item)) ~ "start",
      .data$item == lag(.data$item) ~ "stay",
      .data$item != lag(.data$item) ~ "switch")) |>
    ungroup() |>
    mutate(item = paste(.data$item, .data$hf_type, sep = "_"),
           item_uid = paste(.data$item_group, .data$item, sep = "_")) |>
    # filter(hf_type != "start") |>
    select(-"hf_type")

  df |>
    filter(.data$item_task != "hf") |>
    bind_rows(hf_trials)
}

#' recode correctness for slider
#'
#' @inheritParams recode_trials
recode_slider <- \(df, slider_threshold) {
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

#' recode correctness for items with wrong answers
#'
#' @inheritParams recode_trials
#' @param wrong_items tibble with columns item_uid and answer_fixed
recode_wrong_items <- \(df, wrong_items) {
  # inner_join (not right_join): only recode wrong-answer items that are
  # actually present. right_join would keep unmatched wrong_items keys, adding
  # a spurious all-NA trial row when an item bank entry isn't in the data.
  wrong_trials <- df |>
    inner_join(wrong_items) |>
    mutate(correct = !is.na(.data$response) & .data$response == .data$answer_fixed)
  df |>
    anti_join(wrong_items) |>
    bind_rows(wrong_trials) |>
    select(-"answer_fixed")
}

#' recode items for ToM
#'
#' @inheritParams recode_trials
recode_tom <- \(df) {
  tom <- df |> filter(.data$item_task == "tom")
  tom_disagg <- tom |>
    mutate(story = stringr::str_extract(.data$item_original, "^[0-9]+"),
           item_uid = glue::glue("{item_task}_story{story}_{item_group}_{item}")) |>
    select(-"story")
  df |>
    filter(.data$item_task != "tom") |>
    bind_rows(tom_disagg)
}

#' recode correctness for SDS
#'
#' @inheritParams recode_trials
recode_sds <- function(df) {

  # subset to SDS data and remove known brokenness
  sds_data <- df |>
    filter(.data$item_task == "sds") |>
    filter(!stringr::str_detect(.data$response, "mittel|rote|gelb|blau|gr\u00FCn")) |>
    filter(!(.data$dataset == "pilot_western_ca_main" & .data$timestamp < "2025-02-21"))

  # escape hatch if no valid data remains
  if (nrow(sds_data) == 0) return(df)

  # separate out non-match blocks (dimensions and same)
  sds_dimensions <- sds_data |> filter(.data$item_group == "dimensions")
  sds_same <- sds_data |> filter(.data$item_group == "same")
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
  sds_intro <- bind_rows(sds_dimensions, sds_same_coded)

  # figure out which responses correspond to each trial
  sds_indexed <- sds_data |>
    filter(stringr::str_detect(.data$item_group, "match")) |>
    mutate(different = stringr::str_detect(.data$item_original, "different")) |>
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
  sds_match <- sds_indexed |>
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
    mutate(choice_i = 1:n()) |>
    ungroup() |>
    select("run_id", "trial_index", "item_group", "match_k", "choice_i", "trial_id",
           "item", resp = "response", opts = "distractors", "correct", "original_correct")

  # parse response and options strings into vectors of stimuli
  sds_opts <- sds_match |>
    mutate(resp_parsed = .data$resp |> purrr::map(parse_response) |> purrr::map(sort),
           opts_parsed = .data$opts |> purrr::map(parse_response) |> purrr::map(sort))

  # code dimension values for each stimulus in response and options
  sds_coded <- sds_opts |>
    mutate(resp_coded = purrr::map(.data$resp_parsed, code_dims),
           opts_coded = purrr::map(.data$opts_parsed, code_dims))

  # match response and options to figure out set of possible matches
  sds_dims <- sds_coded |>
    mutate(opts_dims = purrr::map(.data$opts_coded, match_opts_dims),
           resp_dims = purrr::map2(.data$resp_coded, .data$opts_dims, match_resp_dims)) |>
    mutate(n_matches = purrr::map_int(.data$opts_dims, sum))

  # escape hatch if no valid data remains
  if (nrow(sds_dims) == 0) return(df)

  # determine outcomes of each trial – was the response a match + was the response new
  sds_correct <- sds_dims |>
    mutate(resp_norm = purrr::map_chr(.data$resp_parsed, \(s) paste(s, collapse = " "))) |>
    mutate(subtrial_match = purrr::map_int(.data$resp_dims, length) > 0) |>
    select("run_id", "item_group", "match_k", "n_matches", "trial_index",
           "trial_id", "choice_i", "resp_norm", "subtrial_match") |>
    tidyr::nest(trials = c("trial_id", "choice_i", "resp_norm", "subtrial_match")) |>
    mutate(new = purrr::map(.data$trials, \(tr) purrr::map_lgl(1:nrow(tr), \(i) i == 1 | !(tr$resp_norm[i] %in% tr$resp_norm[1:(i-1)]))),
           correct = purrr::map2(.data$trials, .data$new, \(tr, ne) tr |> mutate(new = ne, correct = .data$subtrial_match & new))) |>
    select(-"new", -"trials") |>
    tidyr::unnest("correct") |>
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
           chance = 0) |>
    select("trial_id", "correct", "item_uid", "chance")

  # substitute recoded item_uid + correct + chance
  # add back in intro trials
  sds_trials <- sds_data |>
    select(-c("correct", "item_uid", "chance")) |>
    inner_join(sds_correct_itemized, by = "trial_id") |>
    bind_rows(sds_intro)

  df |>
    filter_out(.data$item_task == "sds") |>
    bind_rows(sds_trials)
}
