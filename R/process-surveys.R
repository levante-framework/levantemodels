#' Process survey data
#' @export
#'
#' @inheritParams process_runs
#' @param survey_types Character vector of survey types to include (caregiver,
#'   student, teacher).
#' @param remove_incomplete_surveys Boolean indicating whether to drop surveys
#'   that were marked as incomplete (defaults to FALSE).
#'
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' surveys <- process_surveys(dataset_spec)
#' }
process_surveys <- function(dataset_spec,
                            survey_types = c("caregiver", "student", "teacher"),
                            remove_incomplete_surveys = FALSE,
                            max_results = NULL) {

  where_str <- levante:::build_filter("surveys.survey_type", survey_types, allow_null = TRUE)
  query_str <- glue::glue(
    "SELECT * FROM survey_responses
     LEFT JOIN surveys ON survey_responses.survey_id = surveys.survey_id
      {where_str}"
  )

  surveys <- levante:::get_datasets_data(
    dataset_spec, levante:::query_getter("survey_responses", query_str, max_results))

  if (nrow(surveys) == 0) return()

  if (remove_incomplete_surveys) surveys <- surveys |> filter(.data$is_complete)

  surveys |>
    select(-matches("_1")) |>
    mutate(survey_type = if_else(
      is.na(.data$survey_type) & stringr::str_detect(.data$survey_id, ":data"),
      "student",
      .data$survey_type)) |>
    mutate(survey_part = if_else(
      is.na(.data$survey_part) & stringr::str_detect(.data$survey_id, ":data"),
      "general",
      .data$survey_part)) |>
    add_survey_items() |>
    code_survey_data()
}


# get survey item metadata and join with survey data
add_survey_items <- function(surveys) {

  survey_items <- fetch_survey_items()
  suppressWarnings(
    survey_items_coded <- survey_items |>
      rename(question = "variable", question_order = "variable_order") |>
      mutate(values = if_else(is.na(.data$values) | .data$values == "", "[]", .data$values),
             values = .data$values |> purrr::map(jsonlite::fromJSON) |>
               purrr::map(\(v) if (length(v) == 0) v else replace_values(v, "No" ~ "0", "Yes" ~ "1")) |>
               purrr::map(as.numeric))
  )

  surveys |>
    inner_join(survey_items_coded, by = c("survey_type", "survey_part", "question"))
}

reverse_value <- \(v, vals) if (any(is.na(vals)) || !(v %in% vals)) NA else rev(vals)[which(vals == v)]

code_survey_data <- function(surveys) {
  surveys |>
    # code 0/1 for true/false
    mutate(value = if_else(is.na(.data$numeric_response) & !is.na(.data$boolean_response),
                           as.numeric(.data$boolean_response), .data$numeric_response),
           .after = "question") |>
    arrange(.data$question_order) |>
    mutate(question = forcats::fct_inorder(.data$question)) |>
    # reverse code values if needed
    mutate(reverse_coded = tidyr::replace_na(.data$reverse_coded, FALSE),
           value = if_else(.data$reverse_coded,
                           purrr::map2_dbl(.data$value, .data$values, reverse_value),
                           .data$value)) |>
    select("redivis_source", "survey_type", "survey_part", "survey_id",
           "survey_schema_source", "specific_scope", "specific_scope_id",
           "administration_id", "user_id", contains("construct"),
           "question_type", "question_order", "question_text" = "item_text",
           "question", "value", "boolean_response", "string_response",
           "numeric_response", "survey_timestamp" = "created_at",
           "response_timestamp" = "timestamp", "is_complete", "valid_survey",
           "valid_survey_response") |>
    arrange(.data$redivis_source, .data$survey_id, .data$question_order)
}

#' Link survey data to participant data
#' @export
#'
#' @param surveys Survey data as returned by `process_surveys()`.
#' @param participants Participant data as returned by `process_participants()`.
#'
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' surveys <- process_surveys(dataset_spec)
#' participants <- process_participants(dataset_spec)
#' survey_data <- surveys |> link_surveys(participants)
#' }
link_surveys <- function(surveys, participants) {

  survey_vars <- c("survey_id", "administration_id", "user_id", "survey_part",
                   "specific_scope", "specific_scope_id", "survey_type",
                   "is_complete", "survey_timestamp",
                   "survey_schema_source", "valid_survey")
  user_survey_data <- surveys |>
    mutate(survey_group = .data$survey_part) |>
    tidyr::nest(survey_data = -all_of(c("survey_group", survey_vars))) |>
    mutate(n_responses = purrr::map_int(.data$survey_data, nrow)) |>
    filter(.data$n_responses > 1)

  children <- participants |> rename(child_id = "user_id")

  # student survey -- user_id is child
  survey_student <- user_survey_data |>
    filter(.data$survey_type == "student") |>
    mutate(child_id = .data$user_id) |>
    select(-"survey_group")

  # teacher survey -- user_id is teacher
  teachers <- children |>
    filter(.data$teacher_id != "") |>
    select("child_id", "teacher_id") |>
    tidyr::nest(children = -c("teacher_id"))

  survey_teacher <- user_survey_data |>
    filter(.data$survey_type == "teacher") |>
    # select(-"child_id") |>
    left_join(teachers, by = c("user_id" = "teacher_id")) |>
    tidyr::unnest("children") |>
    select(-"survey_group", -matches("children"))

  parents <- children |>
    select("child_id", "parent1_id", "parent2_id") |>
    tidyr::pivot_longer(cols = c("parent1_id", "parent2_id"), names_to = NULL, values_to = "parent_id") |>
    filter(!is.na(.data$parent_id))

  # caregiver survey, household (across children) section
  survey_household <- user_survey_data |>
    filter(.data$survey_type == "caregiver", stringr::str_detect(.data$survey_group, "general")) |>
    select(-"survey_group") |> #, -"child_id") |>
    inner_join(parents, by = c("user_id" = "parent_id"), relationship = "many-to-many") |>
    relocate("child_id", .after = "user_id")

  # caregiver survey, child-specific section
  survey_child <- user_survey_data |>
    filter(.data$survey_type == "caregiver", .data$survey_group == "specific") |>
    mutate(child_id = if_else(.data$specific_scope == "child_id",
                              .data$specific_scope_id, NA)) |>
    select(-"survey_group")

  # caregiver survey combined
  survey_caregiver <- bind_rows(survey_household, survey_child)

  # recombine separated out survey types
  children_demapped <- children |>
    select("child_id", "birth_month", "birth_year", "team", "dataset")
  survey_combined <- bind_rows(survey_student, survey_teacher, survey_caregiver) |>
    select(-c("survey_schema_source", "specific_scope", "specific_scope_id",
              "n_responses")) |>
    rename(respondent_id = "user_id") |>
    left_join(children_demapped, by = c("child_id")) |>
    mutate(age = compute_age(.data$birth_month, .data$birth_year, .data$survey_timestamp)) |>
    select(-contains("birth_")) |>
    mutate(survey_type = .data$survey_type |> stringr::str_replace("student", "child")) |>
    relocate("survey_data", .after = everything())

  survey_combined |> tidyr::unnest("survey_data") |>
    relocate("redivis_source", "team", "dataset", .before = everything()) |>
    relocate("child_id", .after = "respondent_id")
}
