fetch_metadata_table <- function(table_name, version = "current") {
  metadata <- redivis::redivis$organization("levante")$dataset("levante_metadata_items:czjv", version = version)
  message(glue::glue("Fetching item metadata from {table_name}"))
  suppressWarnings(
    metadata$table(table_name)$to_tibble()
  )
}

#' Get item UIDs mapped by trial_id
#'
#' @param version Redivis version tag of the `levante_metadata_items` dataset
#' @export
#' @examples
#' \dontrun{
#' mapping_trial <- fetch_item_mapping_trial()
#' }
fetch_item_mapping_trial <- function(version = "current") {
  fetch_metadata_table("item_mapping_trial:hjas", version = version)
}

#' Get item UIDs mapped by fields
#'
#' @export
#' @examples
#' \dontrun{
#' mapping_fields <- fetch_item_mapping_fields()
#' }
fetch_item_mapping_fields <- function(version = "current") {
  fetch_metadata_table("item_mapping_fields:6v86", version = version)
}

#' Get item UIDs mapped by item_id
#'
#' @export
#' @examples
#' \dontrun{
#' mapping_id <- fetch_item_mapping_id()
#' }
fetch_item_mapping_id <- function(version = "current") {
  fetch_metadata_table("item_mapping_id:tma7", version = version)
}

#' Get metadata for corpus items
#'
#' @export
#' @examples
#' \dontrun{
#' corpus_items <- fetch_corpus_items()
#' }
fetch_corpus_items <- function(version = "current") {
  fetch_metadata_table("corpus_items:ezfc", version = version)
}

#' Get metadata for survey items
#'
#' @export
#' @examples
#' \dontrun{
#' survey_items <- fetch_survey_items()
#' }
fetch_survey_items <- function(version = "current") {
  fetch_metadata_table("survey_items:tfw0", version = version) |>
    arrange(.data$survey_type, .data$variable_order)
}
