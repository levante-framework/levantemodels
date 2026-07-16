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
    invariance    = "character",
    items         = "character",
    runs          = "character",
    group_names   = "character",
    groups        = "character",
    scores        = "tbl_df",
    fit           = "list"
  )
)

#' constructor helper
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
      invariance = mirt::extract.mirt(mod, "invariance"),
      items = mirt::extract.mirt(mod, "itemnames"),
      runs = row_ids,
      group_names = mirt::extract.mirt(mod, "groupNames"),
      groups = as.character(mirt::extract.mirt(mod, "group")),
      scores = extract_scores(mod, row_ids),
      fit = mod@Fit
  )
}

#' helper to extract and tidy scores
#' @rdname ModelRecord
extract_scores <- \(mod, row_ids) {
  mirt::fscores(mod, method = "EAP", full.scores.SE = TRUE) |>
    as_tibble() |>
    rename(ability = "F1", se = "SE_F1") |>
    mutate(run_id = row_ids, .before = everything())
}

#' method for show generic
#' @rdname ModelRecord
setMethod("show", "ModelRecord", \(object) {
  cat(glue::glue("{is(object)[[1]]} of a {object@model_class} model ({object@nfact} factor {object@itemtype})"))
})

#' method for AIC generic
#' @rdname ModelRecord
#' @export
setMethod("AIC", "ModelRecord", \(object) {
  object@fit$AIC
})

#' method for BIC generic
#' @rdname ModelRecord
#' @export
setMethod("BIC", "ModelRecord", \(object) {
  object@fit$BIC
})

#' method for logLik generic
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

#' given ModelRecord object, instantiate mirt model object
#' @param mod_rec ModelRecord object
#' @export
model_from_record <- \(mod_rec) {
  if (model_class(mod_rec) == "SingleGroupClass") {
    mirt::mirt(data = mod_rec@data, pars = model_vals(mod_rec), TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups,
                        pars = model_vals(mod_rec), TOL = NaN)
  }
}

#' count number of items with non-NA responses for each run
#' @inheritParams model_from_record
#' @export
count_items <- \(mod_rec) {
  counts <- mod_rec@data |> purrr::negate(is.na)() |> rowSums()
  props <- counts / ncol(mod_rec@data)
  props |>
    purrr::set_names(mod_rec@runs) |>
    tibble::enframe(name = "run_id", value = "prop_items")
}

#' extract item parameters from a model record
#' @inheritParams model_from_record
#' @export
mod_coefs <- \(mod_rec, item_sep = "-") {
  n_resp <- bind_cols(group = mod_rec@groups, mod_rec@data) |>
    tidyr::pivot_longer(cols = -"group", names_to = "item", values_to = "correct") |>
    filter(!is.na(.data$correct)) |>
    count(.data$item, .data$group, name = "n_responses")
  model_vals(mod_rec) |>
    as_tibble() |>
    filter(.data$group != "GROUP", .data$item != "GROUP") |>
    select("group", "item", "name", "value") |>
    tidyr::pivot_wider(names_from = "name", values_from = "value") |>
    left_join(n_resp, by = c("group", "item")) |>
    mutate(n_responses = .data$n_responses |> tidyr::replace_na(0),
           item = stringr::str_remove(.data$item, glue::glue("{item_sep}[0-9]+$"))) |>
    group_by(.data$group, .data$item, .data$a1, .data$d, .data$g, .data$u) |>
    summarise(n_responses = sum(.data$n_responses), n_inst = n(), .groups = "drop") |>
    mutate(difficulty = -.data$d / .data$a1) |>
    arrange(.data$difficulty)
}
