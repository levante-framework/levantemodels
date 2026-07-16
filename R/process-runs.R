#' Process run data
#'
#' @param dataset_spec List of dataset names and versions to retrieve.
#' @param remove_incomplete_runs Boolean indicating whether to drop runs that
#'   were marked as incomplete (defaults to TRUE).
#' @param remove_invalid_runs Boolean indicating whether to drop runs that were
#'   marked as invalid (defaults to TRUE).
#' @param max_results Max number of records to load for each table (defaults to
#'   entire table).
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' runs <- process_runs(dataset_spec)
#' }
process_runs <- function(dataset_spec,
                         remove_incomplete_runs = TRUE,
                         remove_invalid_runs = TRUE,
                         max_results = NULL) {

  run_vars <- c(
    "run_id",
    "runs.user_id",
    "runs.age",
    "runs.task_id",
    "runs.task_version",
    "runs.administration_id",
    "runs.time_started",
    "runs.time_finished",
    "runs.num_attempted",
    "runs.num_correct",
    "runs.test_comp_theta_estimate",
    "runs.test_comp_theta_se",
    "runs.completed",
    "runs.valid_run",
    "runs.validation_msg_run",
    "runs.variant_id",
    "variants.variant_name",
    "variants.language",
    "variants.adaptive",
    "variants.max_incorrect",
    "variants.max_time",
    "variants.sequential_stimulus",
    "variants.corpus",
    "users.valid_user",
    "users.validation_msg_user"
  )
  query_str <- glue::glue(
    "SELECT {paste(run_vars, collapse = ', ')} FROM runs
     LEFT JOIN variants ON runs.variant_id = variants.variant_id
     LEFT JOIN users ON runs.user_id = users.user_id"
  )

  runs <- levante:::get_datasets_data(
    dataset_spec, levante:::query_getter("runs", query_str, max_results))

  if (remove_invalid_runs) runs <- runs |> filter(.data$valid_run)
  if (remove_incomplete_runs) runs <- runs |> filter(.data$completed)

  missing_language <- tribble(
    ~variant_id,            ~language_,
    "FPbJw79lcfHKJR3fjABb", "de-DE",
    "KNaxHVqdpe2CtS9NLoX8", "de-DE",
    "LTQ0EQ4pvI4FAkjY98Pq", "de-DE",
    "zlOE3yc4n4JimAhGtQ6r", "de-DE",

    "1Y3K5lAs6yocDwkHH5aT", "es-CO",
    "OYKVpWxFYhA9Qh9w58Qy", "es-CO",
    "oD16mNDBnKwPnK7lvaCA", "es-CO",

    "3fFvykenyEYGlAsRfYiJ", "en-US",
    "5qBz8FFYIsuoYkuGXwVd", "en-US",
    "8NLzzprrkwJPeY18iRRH", "en-US",
    "r7o97xl8GcdtcCq651n4", "en-US",
    "rq7PRMzgtkw52HMfxvNW", "en-US",
  )

  missing_adaptive <- tribble(
    ~variant_id,            ~adaptive_,
    "OYKVpWxFYhA9Qh9w58Qy",  FALSE,     # confirmed non-adaptive (memory-game)
    "zlOE3yc4n4JimAhGtQ6r",  FALSE,     # TODO: confirm non-adaptive
    "rq7PRMzgtkw52HMfxvNW",  FALSE,     # TODO: confirm non-adaptive
    "8NLzzprrkwJPeY18iRRH",  FALSE,     # TODO: confirm non-adaptive
  )

  runs |>
    # fill in language/adaptive for variants missing them
    left_join(missing_language, by = "variant_id") |>
    left_join(missing_adaptive, by = "variant_id") |>
    mutate(language = if_else(!is.na(.data$language), .data$language, .data$language_),
           adaptive = if_else(!is.na(.data$adaptive), .data$adaptive, .data$adaptive_)) |>
    select(-"language_", -"adaptive_") |>
    # remove language suffix in task_id
    mutate(task_id = .data$task_id |> stringr::str_remove("-es|-de$")) |>
    # invalidate ages for invalid users
    mutate(age = if_else(!.data$valid_user, NA, .data$age)) |>
    arrange(.data$time_started)
}
