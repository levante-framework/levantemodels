#' Process trial data
#'
#' @inheritParams process_trials
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' trials_prelim <- process_trials_prelim(dataset_spec)
#' }
process_trials_prelim <- function(dataset_spec,
                                  remove_incomplete_runs = TRUE,
                                  remove_invalid_runs = TRUE,
                                  remove_invalid_trials = FALSE,
                                  tasks = NULL, # all tasks if null
                                  participants = NULL, # all participants if null
                                  max_results = NULL) {

  where_str <- levante:::build_filter("task_id", tasks)
  query_str <- glue::glue("SELECT * FROM trials {where_str}") |> stringr::str_trim()

  trials <- levante:::get_datasets_data(
    dataset_spec, levante:::query_getter("trials", query_str, max_results))

  # if participants supplied, filter to trials for only those participants
  if (!is.null(participants)) {
    trials <- trials |> semi_join(participants, by = "user_id")
  }

  # if run filters supplied, get corresponding runs and filter to their trials
  if (any(remove_incomplete_runs, remove_invalid_runs)) {
    runs <- process_runs(dataset_spec,
                         remove_incomplete_runs = remove_incomplete_runs,
                         remove_invalid_runs = remove_invalid_runs)
    trials <- trials |> semi_join(runs, by = c("run_id"))
  }

  # filter to valid trials
  if (remove_invalid_trials) trials <- trials |> filter(.data$valid_trial)

  trials
}

#' Process trial data
#'
#' @inheritParams process_runs
#' @param remove_invalid_trials Boolean indicating whether to drop trials that
#'   were marked as invalid (defaults to FALSE).
#' @param tasks Character vector of tasks to include.
#' @param participants (Optional) Data frame that includes the columns "dataset"
#'   and "user_id", if supplied trial data will be filtered to only those user
#'   IDs.
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' trials <- process_trials(dataset_spec)
#' }
process_trials <- function(dataset_spec,
                           remove_incomplete_runs = TRUE,
                           remove_invalid_runs = TRUE,
                           remove_invalid_trials = FALSE,
                           tasks = NULL, # all tasks if null
                           participants = NULL, # all participants if null
                           max_results = NULL) {

  trials_prelim <- process_trials_prelim(dataset_spec = dataset_spec,
                                         remove_incomplete_runs = remove_incomplete_runs,
                                         remove_invalid_runs = remove_invalid_runs,
                                         remove_invalid_trials = remove_invalid_trials,
                                         tasks = tasks,
                                         participants = participants,
                                         max_results = max_results)
  trials_prelim |>
    remove_practice_trials() |>
    add_item_ids() |>
    add_item_metadata() |>
    add_trial_numbers() |>
    mutate(task_id = .data$task_id |> stringr::str_remove("-es|-de$")) |>
    arrange(.data$task_id, .data$user_id, .data$run_id, .data$trial_number) |>
    select("redivis_source", "task_id", "user_id", "run_id", "trial_id",
           "trial_number", "item_uid", "item_task", "item_group", "item",
           "correct", "rt", "rt_numeric", "response", "response_type",
           "item_original", "answer", "distractors", "chance", "difficulty",
           "theta_estimate", "theta_se", timestamp = "server_timestamp",
           "valid_trial", "validation_msg_trial")
}


# helper function for processing trial data

remove_practice_trials <- function(trials) {
  trials |>
    mutate(practice = .data$is_practice_trial |
             stringr::str_detect(.data$assessment_stage, "practice") |
             stringr::str_detect(.data$corpus_trial_type, "practice")) |>
    filter(is.na(.data$practice) | !.data$practice) |>
    select(-c("practice", "is_practice_trial"))
}

# adds trial indices
add_trial_numbers <- function(trials) {
  trials |>
    group_by(.data$user_id, .data$run_id) |>
    arrange(.data$server_timestamp) |>
    mutate(trial_number = 1:n()) |>
    ungroup()
}

add_item_ids <- function(trials, metadata_version = "current") {

  # get item UIDs coded by trial
  # trial_items <- fetch_trial_items()
  mapping_trial <- fetch_item_mapping_trial(version = metadata_version)
  trial_map <- mapping_trial |>
    mutate(trials = trials |> purrr::map(jsonlite::fromJSON) |> purrr::map(unlist)) |>
    tidyr::unnest(trials) |>
    select(item_uid_trial = "item_uid", trial_id = "trials")

  # get item UIDs coded by item (corpus_trial_type, item, answer, distractors)
  # mapping_items <- fetch_mapping_items()
  mapping_fields <- fetch_item_mapping_fields(version = metadata_version)
  item_map <- mapping_fields |>
    mutate(across(c("corpus_trial_type", "item", "answer", "distractors"),
                  \(s) tidyr::replace_na(s, ""))) |>
    mutate(item_key = paste(.data$corpus_trial_type, .data$item,
                            .data$answer, .data$distractors) |>
             stringr::str_trim()) |>
    select(item_uid_mapping = "item_uid", "item_key") |>
    distinct()

  # get item UIDs coded by item_id
  # mapping_items <- fetch_mapping_items()
  mapping_id <- fetch_item_mapping_id(version = metadata_version)
  id_map <- mapping_id |>
    select(item_uid_id = "item_uid", "item_id")

  # fixes for wrong item UIDs in item banks
  itembank_recodes <- c(
    "sds_same_same"            = "sds_same_",
    "math_compare_65_67"       = "math_compare_67_65",
    "math_compare_27_36"       = "math_compare_36_27",
    "math_compare_390_435"     = "math_compare_435_390",
    "math_compare_69_82"       = "math_compare_82_69",
    "math_compare_823_861"     = "math_compare_861_823",
    "math_fraction_12_13_16"   = "math_fraction_12_13",
    "math_fraction_12_14_34"   = "math_fraction_12_14",
    "math_fraction_13_17_421"  = "math_fraction_13_17",
    "math_fraction_13_23_1"    = "math_fraction_13_23",
    "math_fraction_13_25_1115" = "math_fraction_13_25",
    "math_fraction_14_24_34"   = "math_fraction_14_24",
    "math_fraction_16_13_12"   = "math_fraction_16_13",
    "math_fraction_17_314_514" = "math_fraction_17_314",
    "math_fraction_18_48_58"   = "math_fraction_18_48",
    "math_fraction_19_110_190" = "math_fraction_19_110",
    "math_fraction_23_13_13"   = "math_fraction_23_13",
    "math_fraction_28_39_712"  = "math_fraction_28_39",
    "math_fraction_32_14_74"   = "math_fraction_32_14",
    "math_fraction_34_12_14"   = "math_fraction_34_12",
    "math_fraction_34_716_516" = "math_fraction_34_716",
    "math_fraction_37_114_514" = "math_fraction_37_114",
    "math_fraction_39_13_23"   = "math_fraction_39_13",
    "math_fraction_48_18_38"   = "math_fraction_48_18",
    "math_fraction_178_54_78"  = "math_fraction_178_54",
    "math_fraction_34_716_516" = "math_fraction_34_7_16",
    # "math_fraction_512_16_14"  = "math_fraction_512_16",
    # "math_fraction_512_16_712" = "math_fraction_512_16",
    "math_fraction_710_25_310" = "math_fraction_710_25",
    "math_subtract_17_12"        = "math_subtract_17_!2",
    "math_line_639_1000"         = "math_line_649_1000",
    "math_missing_x_300_400_500" = "math_missing_300_400_500",
    "ha_knock_action"            = "tom_knock_papers_action",
    "ha_knock_attribution"       = "tom_knock_papers_attribution"
  )

  suppressWarnings(
    trials_prepped <- trials |>
      # create item IDs for ROAR tasks (pa -> subtask + answer, sre -> item_id, swr -> answer)
      mutate(subtask = if ("subtask" %in% colnames(trials)) .data$subtask else "") |>
      mutate(item_uid_roar = case_when(
        stringr::str_detect(task_id, "^pa(-|$)") ~ glue::glue("pa_{stringr::str_to_lower(subtask)}_{answer}"),
        stringr::str_detect(task_id, "^sre(-|$)") ~ glue::glue("sre_{item_id}"),
        stringr::str_detect(task_id, "^swr(-|$)") ~ glue::glue("swr_{answer}"),
      ) |> as.character() |> stringr::str_replace_all("-", "_")) |>
      # fix wrong item IDs in item banks
      mutate(item_uid = .data$item_uid |>
               stringr::str_replace("^mrot_3d_.*?_", "mrot_3d_shape_") |>
               stringr::str_replace("^mrot_(.*)_200", "mrot_\\1_160") |>
               stringr::str_replace("^mrot_(.*)_240", "mrot_\\1_120") |>
               stringr::str_replace("^mrot_(.*)_280", "mrot_\\1_080") |>
               stringr::str_replace("^mrot_(.*)_320", "mrot_\\1_040") |>
               stringr::str_replace("^tom_ha_", "ha_") |>
               stringr::str_replace("^vocab__", "vocab_word_") |>
               forcats::fct_recode(!!!itembank_recodes) |>
               as.character() |>
               na_if("_") |>
               na_if("math_fraction_512_16") |>
               na_if("mg_forward_3grid_len2")) |>
      # remove stray SDS instructions and something-same trials (not actually test trials)
      # filter(is.na(.data$item_uid) | !stringr::str_detect(.data$item_uid, "-instr")) |>
      filter_out(stringr::str_detect(.data$item_uid, "-instr") |
                   stringr::str_detect(.data$item_id, "-instr")) |>
      filter_out(.data$corpus_trial_type == "something-same-1") |>
      # recode memory-game answers
      mutate(answer_sub = if_else(.data$task_id == "memory-game",
                                  as.character(stringr::str_count(.data$answer, ":")),
                                  .data$answer)) |>
      # create item key for joining with item map
      mutate(across(c(.data$corpus_trial_type, .data$item, .data$answer_sub, .data$distractors),
                    \(s) tidyr::replace_na(s, ""))) |>
      mutate(item_key = paste(.data$corpus_trial_type, .data$item, .data$answer_sub, .data$distractors) |>
               stringr::str_trim()) |>
      # remove trials with no item information
      filter(!is.na(.data$item_uid) | .data$item_key != "")
  )

  trials_joined <- trials_prepped |>
    select("trial_id", "task_id", "item_id", "item_key", "item_uid", "item_uid_roar") |>
    # join in trial-based item UID map
    left_join(trial_map, by = "trial_id") |>
    # join in item-based item UID map
    left_join(item_map, by = "item_key") |>
    # join in item_id-based item UID map
    left_join(id_map, by = "item_id")

  trials_mapped <- trials_joined |>
    # move all item ID columns to rows
    tidyr::pivot_longer(contains("item_uid"),
                        names_to = "item_uid_source", values_to = "item_uid") |>
    # filter to each trial's present item IDs
    # group_by(.data$trial_id, .data$task_id) |>
    filter(!is.na(.data$item_uid), .data$item_uid != "") |>
    # collapse item ID sources
    group_by(.data$trial_id, .data$task_id, .data$item_uid) |>
    summarise(item_uid_source = list(.data$item_uid_source),
              .groups = "drop")

  # validate that no trial maps to multiple conflicting item IDs
  conflicts <- trials_mapped |> group_by(.data$trial_id) |> filter(n() > 1) |> ungroup()
  if (nrow(conflicts) > 0) {
    warning(glue::glue("{n_distinct(conflicts$trial_id)} trial(s) map to multiple ",
                       "conflicting item IDs and may be scored incorrectly"),
            call. = FALSE)
  }

  # join mapped trials back into overall trials
  trials_prepped |>
    select(-"item_uid") |>
    left_join(trials_mapped, by = c("task_id", "trial_id"))
}


add_item_metadata <- function(trials, metadata_version = "current") {
  corpus_items <- fetch_corpus_items(version = metadata_version) |> distinct()
  trials |>
    filter(!is.na(.data$item_uid)) |>
    left_join(corpus_items, by = "item_uid") |>
    # code item_task for roar tasks
    mutate(item_task = if_else(
      .data$item_uid_source == "item_uid_roar",
      stringr::str_extract(.data$task_id, "^[A-z]*"),
      .data$item_task
    )) |>
    # code chance for roar tasks
    mutate(chance = if_else(.data$item_task %in% c("swr", "sre"), 0.5, chance)) |>
    mutate(group = tidyr::replace_na(.data$group, ""),
           entry = tidyr::replace_na(.data$entry, "")) |>
    rename(item_original = "item", item_group = "group", item = "entry")
}
