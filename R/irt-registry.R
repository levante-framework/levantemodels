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
  fetch_cached_table(ds, "scoring_models:t416") |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' Fetch scoring parameters table
#' @export
#' @inheritParams fetch_registry_dir
fetch_scoring_parameters <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  fetch_cached_table(ds, "item_parameters:4cvk") |>
    mutate(redivis_source = ds$get()$scoped_reference, .before = everything())
}

#' Get the scoring specification in scoring_table for a given task + dataset
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

#' Convert model spec to filename
#' @export
#' @inheritParams get_model_record
model_spec_filename <- \(spec) {
  mod_basename <- spec[c("item_task", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
  mod_filename <- glue::glue("{spec$item_task}/{spec$model_set}/{spec$subset}/{mod_basename}.rds")
}

#' Fetch file from model registry
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

# session cache of registry directories by version, of model records fetched
# from registry directories, and of mirt models reconstructed from model
# records (scoring many datasets reuses the same few models)
model_cache <- new.env(parent = emptyenv())
model_cache$registry_dirs <- list()
model_cache$records <- list()
model_cache$models <- list()

#' Get the model record indexed in the registry for a given scoring specification
#'
#' If `spec` has a non-missing `registry_version` (a column of the scoring
#' table), the model record is fetched from that version of the registry;
#' otherwise it is fetched from `registry_dir`. Each model record is
#' downloaded once per session per registry directory and then reused.
#'
#' @export
#' @param spec Model specification (list with names item_task, model_set, subset, itemtype, nfact, invariance, and optionally registry_version).
#' @param registry_dir Model registry directory as returned by `fetch_registry_dir()`, used if `spec` has no `registry_version`.
#' @return ModelRecord object
get_model_record <- \(spec, registry_dir = NULL) {

  # construct filename from specification
  mod_filename <- as.character(model_spec_filename(spec))

  # use the registry version given by the specification, if any
  version <- spec$registry_version
  if (!is.null(version) && !is.na(version)) {
    if (is.null(model_cache$registry_dirs[[version]])) {
      model_cache$registry_dirs[[version]] <- fetch_registry_dir(version)
    }
    registry_dir <- model_cache$registry_dirs[[version]]
  }
  stopifnot("registry_dir is needed for a spec with no registry_version" = !is.null(registry_dir))

  # fetch corresponding file from model registry if not already cached
  cached <- purrr::detect(model_cache$records, \(r) identical(r$registry_dir, registry_dir) &&
                            r$filename == mod_filename)
  if (!is.null(cached)) return(cached$record)
  mod_rec <- get_registry_file(mod_filename, registry_dir)
  model_cache$records <- c(model_cache$records,
                           list(list(registry_dir = registry_dir, filename = mod_filename, record = mod_rec)))
  mod_rec
}

# invariance as recorded in mirt
# (subset of c("free_means", "free_var", "intercepts", "slopes"))
# to shorthand name
# (one of c("configural", "metric", "scalar_intercepts", "scalar_slopes_and_intercepts", "full")
translate_invariance <- \(invariance_terms) {
  invariances |>
    purrr::map(\(inv) setequal(inv, invariance_terms)) |>
    purrr::keep(identity) |>
    names()
}
