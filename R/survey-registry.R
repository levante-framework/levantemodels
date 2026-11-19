# Survey scoring table and model registry, structured like the IRT ones
# (irt-registry.R):
#
#   IRT                               survey
#   fetch_scoring_table()             fetch_survey_scoring_table()
#   fetch_registry_dir()              fetch_survey_registry_dir()
#   get_model_spec(task, dataset)     get_survey_model_spec(survey_type, survey_model, dataset)
#   model_spec_filename(spec)         survey_model_spec_filename(spec)
#   get_model_record(spec, dir)       get_survey_model_record(spec, dir)   (session cache)
#
# The scoring table has one row per survey_type x survey_model x dataset,
# naming the model to use and the model group to score that dataset with. Both
# live in levante_metadata_scoring on Redivis, next to the IRT tables:
#   survey_scoring_models (table)  <- IRT scoring_models
#   survey_models (file index)     <- IRT model_registry

survey_scoring_dataset <- "levante_metadata_scoring:e97h"

#' Fetch the survey scoring table
#'
#' The survey counterpart of [fetch_scoring_table()]: one row per
#' `survey_type` x `survey_model` x `dataset`, naming the CFA model that scores
#' that dataset and the model group (`scoring_group`) to score it with.
#'
#' @param version Version tag of the `levante_metadata_scoring` dataset
#'   (`"current"` = latest release, `"next"` = the unreleased draft).
#' @param path Optional path to a local csv copy of the table (for offline
#'   checks). If given, `version` is ignored and no `redivis_source` column is
#'   added.
#' @return tibble with columns `survey_type`, `survey_model`, `dataset`, `team`,
#'   `model_type`, `invariance`, `scoring_group`, `new_site` (logical), and
#'   `redivis_source` when read from Redivis.
#' @export
#' @examples
#' \dontrun{
#' survey_scoring_table <- fetch_survey_scoring_table()
#' }
fetch_survey_scoring_table <- function(version = "current", path = NULL) {
  if (!is.null(path))
    return(readr::read_csv(path, show_col_types = FALSE,
                           col_types = readr::cols(new_site = readr::col_logical(), .default = readr::col_character())))
  ds <- redivis::redivis$organization("levante")$dataset(survey_scoring_dataset, version = version)
  fetch_cached_table(ds, "survey_scoring_models:3f4p") |>
    mutate(across(-"new_site", as.character), new_site = as.logical(.data$new_site)) |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' Fetch the survey model registry directory
#'
#' The survey counterpart of [fetch_registry_dir()]: the file index of survey
#' model records (`{survey_type}/{survey_model}_{model_type}_{invariance}.rds`).
#'
#' @inheritParams fetch_survey_scoring_table
#' @param path Optional path to a local folder with the same layout (for
#'   offline checks). If given, `version` is ignored.
#' @return A Redivis Directory object, or the normalized local path (character).
#' @export
fetch_survey_registry_dir <- function(version = "current", path = NULL) {
  if (!is.null(path)) return(normalizePath(path, mustWork = TRUE))
  ds <- redivis::redivis$organization("levante")$dataset(name = survey_scoring_dataset, version = version)
  ds$table("survey_models:nyzt")$to_directory()
}

# does the registry hold this record file? A Redivis Directory returns NULL for
# a missing file; any other error (network, credentials) is passed on as is
survey_registry_has <- function(registry_dir, f) {
  if (is.character(registry_dir)) return(file.exists(file.path(registry_dir, f)))
  !is.null(registry_dir$get(f))
}

# read one record file: local readRDS, or get_registry_file() (download, read,
# delete) for a Redivis Directory
survey_read_record <- function(registry_dir, f) {
  if (is.character(registry_dir)) return(readRDS(file.path(registry_dir, f)))
  get_registry_file(f, registry_dir)
}

#' Get the survey scoring specification for one model and dataset
#'
#' The survey counterpart of [get_model_spec()]. Unlike it, a duplicated table
#' row is an error (instead of silently not scoring).
#'
#' @param survey_type Survey type (`"child"` or `"caregiver"`).
#' @param survey_model Survey model name (e.g. `"parenting"`).
#' @param dataset Dataset name.
#' @param scoring_table Tibble returned by [fetch_survey_scoring_table()].
#' @return The table row as a list, or `NULL` if the table has no row.
#' @export
get_survey_model_spec <- function(survey_type, survey_model, dataset, scoring_table) {
  spec <- scoring_table |>
    filter(.data$survey_type == !!survey_type, .data$survey_model == !!survey_model, .data$dataset == !!dataset)
  # a duplicated row is a table error, so stop instead of silently not scoring
  if (nrow(spec) > 1) stop(sprintf("scoring table has %d rows for %s/%s x %s", nrow(spec), survey_type, survey_model, dataset))
  if (nrow(spec) == 0) return(NULL)
  as.list(spec)
}

# Table checks run before scoring (by score_survey() and score_surveys()): the
# required columns are present, there is one row per survey_type x
# survey_model x dataset, the model record file of every row exists, and the
# scoring group is the group of the row's team (calibration site), or "pooled"
# for a new site. Returns TRUE invisibly; stops on the first problem.
check_survey_scoring_table <- function(scoring_table, registry_dir) {
  need <- c("survey_type", "survey_model", "dataset", "team", "model_type", "invariance", "scoring_group", "new_site")
  miss <- setdiff(need, names(scoring_table))
  if (length(miss)) stop("scoring table lacks columns: ", paste(miss, collapse = ", "))
  dup <- scoring_table |> count(.data$survey_type, .data$survey_model, .data$dataset) |> filter(.data$n > 1)
  if (nrow(dup)) stop("duplicated scoring-table rows: ",
                      paste(dup$survey_type, dup$survey_model, dup$dataset, sep = "/", collapse = "; "))
  for (i in seq_len(nrow(scoring_table))) {
    spec <- as.list(scoring_table[i, ])
    f <- survey_model_spec_filename(spec)
    if (!survey_registry_has(registry_dir, f)) stop("no model record for table row ", i, ": ", f)
    meta <- get_survey_model_record(spec, registry_dir)$meta
    if (isTRUE(spec$new_site) && spec$scoring_group != "pooled")
      stop(sprintf("table row %d: a new site must have scoring group \"pooled\"", i))
    if (!isTRUE(spec$new_site) && !spec$scoring_group %in% meta$group_labels)
      stop(sprintf("table row %d: scoring group %s is not a group of %s", i, spec$scoring_group, meta$model))
    if (!isTRUE(spec$new_site) && !identical(unname(survey_site_map[spec$team]), spec$scoring_group))
      stop(sprintf("table row %d: team %s is site %s, but the scoring group is %s", i, spec$team,
                   unname(survey_site_map[spec$team]), spec$scoring_group))
    if (isTRUE(spec$new_site) && spec$team %in% names(survey_site_map))
      stop(sprintf("table row %d: team %s is a calibration site, not a new site", i, spec$team))
  }
  invisible(TRUE)
}

#' Convert a survey model spec to its registry filename
#'
#' The survey counterpart of [model_spec_filename()]. Two levels,
#' `{survey_type}/{survey_model}_{model_type}_{invariance}.rds`: unlike IRT,
#' each survey model has a single version, so there are no model_set / subset
#' folders.
#'
#' @inheritParams get_survey_model_record
#' @return Filename (string) relative to the registry directory.
#' @export
survey_model_spec_filename <- function(spec) {
  base <- paste(spec$survey_model, spec$model_type, spec$invariance, sep = "_")
  file.path(spec$survey_type, paste0(base, ".rds"))
}

# session cache of survey model records fetched from one registry directory
survey_model_cache <- new.env(parent = emptyenv())
survey_model_cache$registry_dir <- NULL
survey_model_cache$records <- list()

#' Get the survey model record for a scoring specification
#'
#' The survey counterpart of [get_model_record()]. Each record is read once
#' per session and then reused; passing a different `registry_dir` clears the
#' cache.
#'
#' @param spec Scoring specification (list with names `survey_type`,
#'   `survey_model`, `model_type`, `invariance`), e.g. from
#'   [get_survey_model_spec()].
#' @param registry_dir Registry directory returned by
#'   [fetch_survey_registry_dir()].
#' @return Model record: a list with `scheme`, `meta` and (scheme B)
#'   `partable`.
#' @export
get_survey_model_record <- function(spec, registry_dir) {
  f <- survey_model_spec_filename(spec)
  if (!identical(survey_model_cache$registry_dir, registry_dir)) {
    survey_model_cache$registry_dir <- registry_dir
    survey_model_cache$records <- list()
  }
  if (is.null(survey_model_cache$records[[f]])) {
    survey_model_cache$records[[f]] <- survey_read_record(registry_dir, f)
  }
  survey_model_cache$records[[f]]
}

# Sites (the `team` column) that are calibration groups of the survey models,
# used by check_survey_scoring_table(): a dataset of a calibration team must be
# scored with that team's site group; a dataset of any other team is a new
# site, scored under the pooled model of all groups (scoring_group "pooled").
survey_site_map <- c(pilot_uniandes_co = "uniandes-co",
                     pilot_mpieva_de   = "mpieva-de",
                     pilot_western_ca  = "western-ca")
