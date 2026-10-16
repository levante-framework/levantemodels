#' Get a table from a Redivis dataset, cached on disk across sessions
#'
#' If the option `levantemodels.cache_dir` is set, tables from released
#' dataset versions are saved there and read back on later calls instead of
#' being downloaded again. Released versions don't change, so a cached table
#' is always current for its version; unreleased versions are never cached.
#'
#' @param ds Redivis dataset object
#' @param table_name name of table in dataset
#' @return tibble
#' @keywords internal
fetch_cached_table <- function(ds, table_name) {
  cache_dir <- getOption("levantemodels.cache_dir")
  if (is.null(cache_dir) || !isTRUE(ds$get()$properties$version$isReleased)) {
    return(ds$table(table_name)$to_tibble())
  }

  # one file per dataset version + table
  cache_name <- gsub("[^A-Za-z0-9_.-]", "_", paste(ds$scoped_reference, table_name, sep = "-"))
  cache_file <- file.path(cache_dir, paste0(cache_name, ".rds"))
  if (file.exists(cache_file)) {
    message(glue::glue("--Using cached copy of {table_name}"))
    return(readRDS(cache_file))
  }

  tbl <- ds$table(table_name)$to_tibble()
  # write to temporary file and then rename, so that concurrent sessions
  # never read a partially written file
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  tmp_file <- tempfile(tmpdir = cache_dir, fileext = ".rds")
  saveRDS(tbl, tmp_file)
  file.rename(tmp_file, cache_file)
  tbl
}

fetch_metadata_table <- function(table_name, version = "current") {
  metadata <- redivis::redivis$organization("levante")$dataset("levante_metadata_items:czjv", version = version)
  message(glue::glue("Fetching item metadata from {table_name}"))
  suppressWarnings(
    fetch_cached_table(metadata, table_name)
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
#' @inheritParams fetch_item_mapping_trial
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
#' @inheritParams fetch_item_mapping_trial
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
#' @inheritParams fetch_item_mapping_trial
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
#' @inheritParams fetch_item_mapping_trial
#' @export
#' @examples
#' \dontrun{
#' survey_items <- fetch_survey_items()
#' }
fetch_survey_items <- function(version = "current") {
  fetch_metadata_table("survey_items:tfw0", version = version) |>
    arrange(.data$survey_type, .data$variable_order)
}
