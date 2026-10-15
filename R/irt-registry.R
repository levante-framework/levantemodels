#' Fetch model registry file index table
#' @export
#' @param version Version tag of the `levante_metadata_scoring` dataset.
fetch_registry_dir <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset(name = "levante_metadata_scoring:e97h", version = version)
  ds$table("model_registry:rqwv")$to_directory()
}

#' Fetch scoring specification table
#' @export
#' @inheritParams fetch_registry_dir
fetch_scoring_table <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  ds$table("scoring_models:t416")$to_tibble() |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' Fetch scoring specification table
#' @export
#' @inheritParams fetch_registry_dir
fetch_scoring_parameters <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  ds$table("item_parameters:4cvk")$to_tibble() |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' get the scoring specification in scoring_table for a given task + dataset
#' @export
#' @param score_task string indicating task
#' @param score_dataset string indicating dataset
#' @param scoring_table tibble returned by get_scoring_table()
#' @return list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
get_model_spec <- \(score_task, score_dataset, scoring_table) {
  mod_spec <- scoring_table |> filter(.data$item_task == score_task, .data$dataset == score_dataset)
  if (nrow(mod_spec) != 1) return()
  mod_spec |> as.list()
}

#' convert model spec to filename
#' @export
#' @inheritParams get_model_record
model_spec_filename <- \(spec) {
  mod_basename <- spec[c("item_task", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
  mod_filename <- glue::glue("{spec$item_task}/{spec$model_set}/{spec$subset}/{mod_basename}.rds")
}

#' fetch file from model registry
#' @export
#' @param mod_filename Filename (string) of model record to fetch.
#' @inheritParams get_model_record
get_registry_file <- \(mod_filename, registry_dir) {

  # download file
  mod_path <- registry_dir$get(mod_filename)$download(overwrite = TRUE)

  # read in file
  mod_rec <- readr::read_rds(mod_path)

  # delete file
  file.remove(mod_path)

  mod_rec
}

# session cache of model records fetched from one registry directory, and of
# mirt models reconstructed from model records (scoring many datasets reuses
# the same few models)
model_cache <- new.env(parent = emptyenv())
model_cache$registry_dir <- NULL
model_cache$records <- list()
model_cache$models <- list()

#' get the model record indexed in registry_dir for a given scoring specification
#'
#' Each model record is downloaded once per session and then reused. The cache
#' holds one registry directory at a time: passing a different `registry_dir`
#' (e.g. from a new call to `fetch_registry_dir()`) clears it.
#'
#' @export
#' @param spec Model specification (list with names item_task, model_set, subset, itemtype, nfact, invariance).
#' @param registry_dir Model registry directory as returned by `fetch_registry_dir()`.
#' @return ModelRecord object
get_model_record <- \(spec, registry_dir) {

  # construct filename from specification
  mod_filename <- as.character(model_spec_filename(spec))

  # clear cache if registry directory has changed
  if (!identical(model_cache$registry_dir, registry_dir)) {
    model_cache$registry_dir <- registry_dir
    model_cache$records <- list()
    model_cache$models <- list()
  }

  # fetch corresponding file from model registry if not already cached
  if (is.null(model_cache$records[[mod_filename]])) {
    model_cache$records[[mod_filename]] <- get_registry_file(mod_filename, registry_dir)
  }
  model_cache$records[[mod_filename]]
}

# invariance as recorded in mirt
# (subset of c("free_means", "free_var", "intercepts", "slopes"))
# to shorthand name
# (one of c("configural", "metric", "scalar_intercepts", "scalar_slopes_and_intercepts", "full")
translate_invariance <- \(invariance_terms) {
  invariances <- list(
    configural = "",
    metric = c("slopes"),
    scalar = c("free_means", "free_var", "intercepts", "slopes")
  )

  invariances |>
    purrr::map(\(inv) setequal(inv, invariance_terms)) |>
    purrr::keep(identity) |>
    names()
}
