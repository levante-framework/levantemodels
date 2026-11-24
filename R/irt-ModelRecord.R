#' ModelRecord class definition
#'
#' A fitted LEVANTE IRT model and its metadata, as stored in the model
#' registry. Access its contents with [items()], [model_class()],
#' [model_vals()], [scores()], [response_counts()], and [run_counts()]; see
#' `vignette("scoring-and-model-registry")` for details.
#'
#' @export
ModelRecord <- setClass(
  "ModelRecord",
  slots = c(
    model_class   = "character",  # "SingleGroupClass", "MultipleGroupClass"
    model_vals    = "data.frame", # mod2vals() output
    itemtype      = "character",  # "Rasch", "2PL", "3PL"
    # model         = "mirt.model",
    method        = "character",
    data          = "matrix",
    factors       = "character",
    nfact         = "numeric",
    nestpars      = "numeric",
    invariance    = "character",
    items         = "character",
    runs          = "character",
    group_names   = "character",
    groups        = "character",
    scores        = "tbl_df",
    response_counts = "tbl_df", # non-NA responses per group and item
    run_counts    = "tbl_df",   # runs per group
    fit           = "list"
  )
)

#' Constructor helper
#'
#' @param mod mirt model object
#' @param row_ids character vector
#'
#' @rdname ModelRecord
#' @importFrom methods new
#' @export
modelrecord <- \(mod, row_ids) {
  new("ModelRecord",
      model_class = as.character(class(mod)),
      model_vals = as.data.frame(mirt::mod2values(mod)), # actually mirt_df but S4 doesn't like that
      itemtype = unique(mirt::extract.mirt(mod, "itemtype")),
      # model = mirt::extract.mirt(mod, "model"),
      method = unique(mirt::extract.mirt(mod, "method")),
      data = mirt::extract.mirt(mod, "data"),
      factors = mirt::extract.mirt(mod, "factorNames"),
      nfact = mirt::extract.mirt(mod, "nfact"),
      nestpars = mirt::extract.mirt(mod, "nestpars"),
      invariance = mirt::extract.mirt(mod, "invariance"),
      items = mirt::extract.mirt(mod, "itemnames"),
      runs = row_ids,
      group_names = mirt::extract.mirt(mod, "groupNames"),
      groups = as.character(mirt::extract.mirt(mod, "group")),
      scores = extract_scores(mod, row_ids),
      response_counts = count_responses(mirt::extract.mirt(mod, "data"),
                                        mirt::extract.mirt(mod, "group")),
      run_counts = count_runs(mirt::extract.mirt(mod, "group")),
      # fit statistics plus convergence of the estimation
      fit = c(mod@Fit, list(converged = mirt::extract.mirt(mod, "converged"),
                            iterations = mirt::extract.mirt(mod, "iterations")))
  )
}

#' Helper to extract and tidy scores
#' @rdname ModelRecord
extract_scores <- \(mod, row_ids) {
  mirt::fscores(mod, method = "EAP", full.scores.SE = TRUE) |>
    as_tibble() |>
    mutate(across(where(is.numeric), as.numeric)) |> # convert matrices to vectors
    rename(ability = "F1", se = "SE_F1") |>
    mutate(run_id = row_ids, .before = everything())
}

#' Helper to count non-NA responses per group and item
#' @rdname ModelRecord
count_responses <- \(data, groups) {
  bind_cols(group = as.character(groups), as_tibble(!is.na(data))) |>
    group_by(.data$group) |>
    summarise(across(everything(), sum)) |>
    tidyr::pivot_longer(cols = -"group", names_to = "item", values_to = "n_responses")
}

#' Helper to count runs per group
#' @rdname ModelRecord
count_runs <- \(groups) {
  tibble(group = as.character(groups)) |> count(.data$group, name = "n_runs")
}

#' Method for show generic
#' @rdname ModelRecord
setMethod("show", "ModelRecord", \(object) {
  cat(glue::glue("{is(object)[[1]]} of a {object@model_class} model ({object@nfact} factor {object@itemtype})"))
})

#' Method for AIC generic
#' @rdname ModelRecord
#' @export
setMethod("AIC", "ModelRecord", \(object) {
  object@fit$AIC
})

#' Method for BIC generic
#' @rdname ModelRecord
#' @export
setMethod("BIC", "ModelRecord", \(object) {
  object@fit$BIC
})

#' Method for logLik generic
#' @rdname ModelRecord
#' @export
setMethod("logLik", "ModelRecord", \(object) {
  object@fit$logLik
})

# accessor functions for slots

#' @rdname ModelRecord
#' @param object ModelRecord object
#' @export
model_class <- \(object) object@model_class

#' @rdname ModelRecord
#' @export
model_vals <- \(object) object@model_vals

#' @rdname ModelRecord
#' @export
items <- \(object) object@items

#' @rdname ModelRecord
#' @export
scores <- \(object) object@scores

#' @rdname ModelRecord
#' @returns `response_counts()`: tibble with `group`, `item`, and `n_responses`
#'   (number of non-NA calibration responses); for records created before these
#'   were stored, computed from the calibration data
#' @export
response_counts <- \(object) {
  if (methods::.hasSlot(object, "response_counts")) return(object@response_counts)
  count_responses(object@data, object@groups)
}

#' @rdname ModelRecord
#' @returns `run_counts()`: tibble with `group` and `n_runs` (number of
#'   calibration runs); for records created before these were stored, computed
#'   from the calibration groups
#' @export
run_counts <- \(object) {
  if (methods::.hasSlot(object, "run_counts")) return(object@run_counts)
  count_runs(object@groups)
}

#' @rdname ModelRecord
#' @export
tabdata <- \(object) object@tabdata

#' @rdname ModelRecord
#' @returns `convergence()`: one-row tibble with `converged` (whether
#'   estimation converged) and `iterations` (number of EM cycles); both `NA`
#'   for records created before these were stored
#' @export
convergence <- \(object) {
  fit <- object@fit
  dplyr::tibble(converged = if (is.null(fit$converged)) NA else fit$converged,
                iterations = if (is.null(fit$iterations)) NA_real_ else fit$iterations)
}

#' Remove calibration data from a model record
#'
#' For publishing a model record without the data it was calibrated on:
#' removes the calibration runs and scores, and replaces the response matrix
#' with a minimal placeholder. The placeholder has, for each group, one row per
#' response category, with each item taking each of the categories it had in
#' the calibration data, since [mirt::mirt()] needs data with each item's
#' categories to reconstruct the model from its parameter values. Scoring with
#' a sanitized record gives the same scores as with the original record.
#' Response and run counts are kept (see [response_counts()] and
#' [run_counts()]), but functions that use the calibration data itself, such
#' as [count_items()] and [empirical_reliability()], don't work on a sanitized
#' record.
#'
#' @param mod_rec ModelRecord object
#' @return ModelRecord object with `data`, `groups`, `runs`, and `scores`
#'   replaced
#' @export
sanitize_record <- \(mod_rec) {
  # store counts first, since records created before they were stored compute
  # them from the calibration data
  mod_rec@response_counts <- response_counts(mod_rec)
  mod_rec@run_counts <- run_counts(mod_rec)

  data <- mod_rec@data
  item_cats <- lapply(seq_len(ncol(data)), \(j) sort(unique(stats::na.omit(data[, j]))))
  n_cats <- max(lengths(item_cats))
  # each item cycles through its categories
  placeholder <- matrix(sapply(item_cats, \(cats) cats[(seq_len(n_cats) - 1) %% length(cats) + 1]),
                        nrow = n_cats, dimnames = list(NULL, colnames(data)))
  storage.mode(placeholder) <- storage.mode(data)

  group_names <- mod_rec@group_names
  mod_rec@data <- do.call(rbind, rep(list(placeholder), length(group_names)))
  mod_rec@groups <- rep(group_names, each = n_cats)
  mod_rec@runs <- character(0)
  mod_rec@scores <- mod_rec@scores[0, ]
  mod_rec
}
