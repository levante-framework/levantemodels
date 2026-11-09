#' Process participants
#'
#' @inheritParams process_runs
#'
#' @export
#' @examples
#' \dontrun{
#' dataset_spec <- list(list(name = "levante_data_example:d0rt", version = "current"))
#' participants <- process_participants(dataset_spec)
#' }
process_participants <- function(dataset_spec, max_results = NULL) {

  user_vars <- c(
    "sites.site_name AS dataset",
    "users.created_at",
    "users.user_id",
    "users.birth_month",
    "users.birth_year",
    "users.parent1_id",
    "users.parent2_id",
    "users.teacher_id",
    "user_schools.school_id",
    "user_classes.class_id"
  )
  query_str <- glue::glue(
    "SELECT {paste(user_vars, collapse = ', ')} FROM users
     LEFT JOIN user_sites ON users.user_id = user_sites.user_id
     LEFT JOIN sites ON user_sites.site_id = sites.site_id
     LEFT JOIN user_schools ON users.user_id = user_schools.user_id
     LEFT JOIN user_classes ON users.user_id = user_classes.user_id
     WHERE users.user_type IN ('student', 'guest')"
  )

  participants <- get_datasets_data(
    dataset_spec, query_getter("users", query_str, max_results))

  legacy_dataset_names <- list(
    "pilot_uniandes_co_bogota"     = "CO-bogota-pilot",
    "pilot_uniandes_co_rural"      = "CO-rural-pilot",
    "pilot_western_ca_main"        = "CA-western-pilot",
    "pilot_mpieva_de_main"         = "DE-mpieva-pilot",
    "pilot_langcog_us_downex"      = "US-downward_extension-pilot",
    "partner_mpib_de_main"         = "partner-mpib-de",
    "partner_childexplore_intl_ef" = "partner-childexplore-intl",
    "partner_sparklab_us_downex"   = "partner-sparklab-us"
  )

  co_guest_end <- as.POSIXct("2024-06-30")
  suppressWarnings(
    participants |>
      # recode site=dataset names
      mutate(dataset = if_else(.data$dataset %in% legacy_dataset_names,
                               forcats::fct_recode(.data$dataset, !!!legacy_dataset_names),
                               stringr::str_replace_all(.data$dataset, "-", "_"))) |>
      # assume all guest users from before 2024-06-30 are part of pilot_uniandes_co_bogota
      mutate(dataset = if_else(is.na(.data$dataset) & .data$created_at < co_guest_end,
                               "pilot_uniandes_co_bogota", .data$dataset)) |>
      select(-"created_at") |>
      # extract team (fka site) from dataset
      mutate(team = .data$dataset |> stringr::str_extract("^[A-z0-9]+_[A-z]+_[A-z]+(?=_)"),
             .before = "dataset") |>
      relocate("redivis_source", .after = "dataset") |>
      arrange(.data$dataset, .data$user_id)
  )
}
