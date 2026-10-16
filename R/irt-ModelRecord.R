#' ModelRecord class definition
#'
#' A fitted LEVANTE IRT model and its metadata, as stored in the model
#' registry. Access its contents with [items()], [model_class()],
#' [model_vals()], and [scores()]; see
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
      fit = mod@Fit
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
#' @export
tabdata <- \(object) object@tabdata
