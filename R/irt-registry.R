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

#' Fetch scoring parameters table
#' @export
#' @inheritParams fetch_registry_dir
fetch_scoring_parameters <- \(version = "current") {
  ds <- redivis::redivis$organization("levante")$dataset("levante_metadata_scoring:e97h", version = version)
  ds$table("item_parameters:4cvk")$to_tibble() |>
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

#' Get the model record indexed in registry_dir for a given scoring specification
#' @export
#' @param spec Model specification (list with names item_task, model_set, subset, itemtype, nfact, invariance).
#' @param registry_dir Model registry directory as returned by `fetch_registry_dir()`.
#' @return ModelRecord object
get_model_record <- \(spec, registry_dir) {

  # construct filename from specification
  mod_filename <- model_spec_filename(spec)

  # fetch corresponding file from model registry
  get_registry_file(mod_filename, registry_dir)
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
