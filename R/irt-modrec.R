#' Given ModelRecord object, instantiate mirt model object
#'
#' Models are cached for the session (see [get_model_record()]), so scoring
#' several datasets with the same model record reconstructs it only once.
#'
#' @param mod_rec ModelRecord object
#' @export
model_from_record <- \(mod_rec) {
  # identical() is immediate for the same object, e.g. a cached model record
  cached <- purrr::detect(model_cache$models, \(m) identical(m$record, mod_rec))
  if (!is.null(cached)) return(cached$model)

  if (model_class(mod_rec) == "SingleGroupClass") {
    mod <- mirt::mirt(data = mod_rec@data, pars = model_vals(mod_rec), TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    mod <- mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups,
                               pars = model_vals(mod_rec), TOL = NaN)
  }
  model_cache$models <- c(model_cache$models, list(list(record = mod_rec, model = mod)))
  mod
}

#' Count number of items with non-NA responses for each run
#' @inheritParams model_from_record
#' @export
count_items <- \(mod_rec) {
  counts <- mod_rec@data |> purrr::negate(is.na)() |> rowSums()
  props <- counts / ncol(mod_rec@data)
  props |>
    purrr::set_names(mod_rec@runs) |>
    tibble::enframe(name = "run_id", value = "prop_items")
}

#' Extract item parameters from a model record
#' @inheritParams model_from_record
#' @param item_sep separator between item and instance index
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

#' Single group model with the pooled distribution of a multigroup model's groups
#'
#' For scoring data from a group that is not in a scalar multigroup model:
#' returns a single group model with the shared item parameters and the
#' pooled latent distribution of the model's groups, i.e. the mean and
#' variance of the equally weighted mixture of the groups' normal
#' distributions. Nothing is estimated.
#'
#' @param mod_rec ModelRecord object for a scalar multigroup model
#' @return mirt SingleGroupClass model
#' @keywords internal
pooled_group_model <- \(mod_rec) {
  mod_vals <- model_vals(mod_rec)
  group_names <- mod_rec@group_names
  group_par <- \(par) mod_vals$value[match(paste(group_names, par), paste(mod_vals$group, mod_vals$name))]
  means <- group_par("MEAN_1")
  vars <- group_par("COV_11")
  pooled_mean <- mean(means)
  pooled_var <- mean(vars) + mean((means - pooled_mean)^2)
  message(glue::glue("--Group not in model, scoring under pooled prior N({round(pooled_mean, 2)}, {round(pooled_var, 2)})"))

  # reference group's single group model with the pooled mean and variance
  ref_vals <- group_vals(mod_rec, group_names[[1]])
  ref_vals$value[ref_vals$name == "MEAN_1"] <- pooled_mean
  ref_vals$value[ref_vals$name == "COV_11"] <- pooled_var
  mirt::mirt(data = mod_rec@data, pars = ref_vals, TOL = NaN)
}

#' Parameter values for one group of a multigroup model, as a single group model
#'
#' Gives the same single group model as [mirt::extract.group()] on the
#' reconstructed multigroup model, without reconstructing it: the group's rows
#' of the model record's parameter values, relabelled as a single group's.
#' Build the model with
#' `mirt::mirt(data = mod_rec@data, pars = group_vals(mod_rec, group), TOL = NaN)`.
#' This uses all of the model's calibration data, not just the group's, so that
#' every item has the response categories the model was calibrated with (the
#' data doesn't affect scoring with `fscores(response.pattern = )`).
#'
#' @param mod_rec ModelRecord object for a multigroup model
#' @param group name of one of the model's groups
#' @return data frame of parameter values, as from `mirt::mod2values()`
#' @keywords internal
group_vals <- \(mod_rec, group) {
  stopifnot(group %in% mod_rec@group_names)
  vals <- model_vals(mod_rec)
  vals <- vals[vals$group == group, ]
  vals$group <- "all"
  # number parameters from 1, as in a single group model
  vals$parnum <- vals$parnum - vals$parnum[1] + 1L
  # drop labels of equality constraints across groups
  vals$const <- "none"
  rownames(vals) <- NULL
  vals
}
