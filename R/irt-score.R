#' given trial data and model record, score data from corresponding model
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param mod_spec list with entries item_task, dataset, model_set, subset, itemtype, nfact, invariance
#' @param mod_rec ModelRecord object
#' @param method fscores estimation method, e.g. "EAP" (default) or "ML"; see mirt::fscores()
#' @return tibble with scores
#' @export
score_irt <- \(trial_data_task, mod_spec, mod_rec, method = "EAP") {
  message(glue::glue('--Using IRT scoring'))

  # prep new data for model
  data_filtered <- trial_data_task |> rename(group = "dataset") |> dedupe_items()
  data_wide <- data_filtered |> to_mirt_shape_grouped()
  data_prepped <- data_wide |> select(-"group")
  groups <- data_wide |> pull("group")
  data_group <- unique(groups)

  # handle data having groups that aren't in model
  new_group <- FALSE
  if (any(!(data_group %in% mod_rec@group_names))) {
    # for metric or configural models, scoring not possible
    if (!is.na(mod_spec$invariance) & mod_spec$invariance %in% c("metric", "configural")) {
      message(glue::glue("For scoring with {mod_spec$invariance} models, all groups in data must be in specified model."))
      return()
      # for scalar models, item parameters are shared across groups, but each
      # group has its own mean and variance, so score under the pooled
      # distribution of the model's groups
    } else if (!is.na(mod_spec$invariance) & mod_spec$invariance == "scalar") {
      new_group <- TRUE
    }
  }

  # subset data to items present in model
  overlap_items <- intersect(colnames(data_prepped), items(mod_rec))
  if (length(overlap_items) == 0) {
    message("Can't score task data from given model record (no overlapping items)")
    stop("invalid_scoring_model")
  }
  data_aligned <- data_prepped |> select(!!overlap_items)
  # add columns with NA values for items present in model but not in data
  missing_items <- setdiff(items(mod_rec), colnames(data_prepped))
  data_aligned[,missing_items] <- NA

  # reorder columns to the model's item order: mirt::fscores() matches the
  # columns of response.pattern to the model's items by position, not by name,
  # so a mismatched column order silently scores each response against the
  # wrong items
  data_aligned <- data_aligned[, items(mod_rec)]
  stopifnot(identical(colnames(data_aligned), items(mod_rec)))

  # set up mirt model object for data using parameter values from model record
  mod_vals <- model_vals(mod_rec)
  if (model_class(mod_rec) == "SingleGroupClass") {
    # reconstruct single group model
    mod <- mirt::mirt(data = mod_rec@data, pars = mod_vals, TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass" && new_group) {
    # data's group not in model: score under the pooled distribution of the model's groups
    mod <- pooled_group_model(mod_rec)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    # reconstruct multiple group model
    mod_recon <- mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups, pars = mod_vals, TOL = NaN)
    # extract single group model for given group
    mod <- mirt::extract.group(mod_recon, group = data_group)
  }

  # get scores from model
  scores <- mirt::fscores(mod, method = method, response.pattern = data_aligned)

  # return scores tibble with better names and run_ids added back in
  scores |>
    as_tibble() |>
    mutate(across(everything(), as.numeric)) |>
    rename(score = "F1", score_se = "SE_F1") |>
    mutate(run_id = rownames(data_prepped), .before = everything()) |>
    mutate(score_type = "ability", scoring_model = mod_spec_str(mod_spec),
           registry_version = stringr::str_extract(mod_spec$redivis_source, "(?<=:)[^:]*$"))
}

#' single group model with the pooled distribution of a multigroup model's groups
#'
#' For scoring data from a group that is not in a scalar multigroup model:
#' returns a single group model with the shared item parameters and the
#' pooled latent distribution of the model's groups, i.e. the mean and
#' variance of the mixture of the groups' normal distributions, weighted by
#' each group's number of calibration runs. Nothing is estimated.
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
  weights <- as.numeric(table(factor(mod_rec@groups, levels = group_names))) / length(mod_rec@groups)
  pooled_mean <- sum(weights * means)
  pooled_var <- sum(weights * vars) + sum(weights * (means - pooled_mean)^2)
  message(glue::glue("--Group not in model, scoring under pooled prior N({round(pooled_mean, 2)}, {round(pooled_var, 2)})"))

  # reference group's single group model with the pooled mean and variance
  mod_recon <- mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups, pars = mod_vals, TOL = NaN)
  ref_vals <- mirt::mod2values(mirt::extract.group(mod_recon, group = group_names[[1]]))
  ref_vals$value[ref_vals$name == "MEAN_1"] <- pooled_mean
  ref_vals$value[ref_vals$name == "COV_11"] <- pooled_var
  mirt::mirt(data = mod_rec@data, pars = ref_vals, TOL = NaN)
}

#' scores from CAT
#'
#' @param runs run data from one task and one dataset
#' @export
score_cat <- \(runs) {
  message(glue::glue('--Using CAT scoring'))
  runs |>
    filter(!is.na(.data$test_comp_theta_estimate)) |>
    select("run_id", score = "test_comp_theta_estimate", score_se = "test_comp_theta_se") |>
    mutate(score_type = "ability_cat")
}

#' scores for PA -- deprecated
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
score_pa <- \(trial_data_task, dataset)  {
  message(glue::glue('--Using PA scoring'))

  pa_max_trials <- list(
    pilot_western_ca_main = 57,
    pilot_uniandes_co_bogota = 20,
    pilot_uniandes_co_rural = 20
  )
  if (!(dataset %in% names(pa_max_trials))) {
    message(glue::glue("Can't rescore task pa for dataset {dataset}, skipping"))
    return()
  }
  trial_data_task |>
    group_by(.data$run_id) |>
    filter(n() > 3) |>
    summarise(score = sum(.data$correct) / pa_max_trials[[dataset]]) |>
    mutate(score_type = "prop_correct")
}

#' scores for SRE
#'
#' @param trial_data_task trial data from one task and one dataset
#' @param dataset dataset
#' @export
score_sre <- \(trial_data_task, dataset) {
  message(glue::glue('--Using SRE scoring'))

  trial_data_task |>
    group_by(.data$dataset, .data$run_id) |>
    summarise(elapsed = difftime(max(timestamp), min(timestamp), units = "sec"),
              net = sum(.data$correct) - sum(!.data$correct),
              score = .data$net / as.numeric(.data$elapsed),
              .groups = "drop") |>
    filter(.data$elapsed >= 30) |>
    group_by(.data$dataset) |>
    mutate(score = scale(score)[, 1],
           score_type = "guessing_adjusted_rate_scaled") |>
    ungroup() |>
    select("run_id", "score", "score_type")
    # select(-"elapsed", -"net")
}

mod_spec_str <- \(spec) {
  spec[c("model_set", "subset", "itemtype", "nfact", "invariance")] |> purrr::discard(is.na) |> paste(collapse = "_")
}

#' score
#' @export
#'
#' @param task task
#' @param dataset dataset
#' @param trials trial data from one task and one dataset
#' @param runs run data from one task and one dataset
#' @param scoring_table tibble returned by fetch_scoring_table()
#' @param registry_dir redivis directory returned by fetch_registry_dir()
score <- \(task, dataset, trials, runs, scoring_table, registry_dir) {

  message(glue::glue('Scoring data for task "{task}" and dataset "{dataset}"'))

  cat_tasks <- c()
  # cat_tasks <- c("swr")
  custom_tasks <- list(sre = score_sre)
  # custom_tasks <- list(pa = score_pa, sre = score_sre)

  # if scoring_table has entry for task + dataset, use that model spec
  # TODO: make model depend on registry version in scoring table
  mod_spec <- get_model_spec(task, dataset, scoring_table)
  if (!is.null(mod_spec)) {
    mod_rec <- get_model_record(mod_spec, registry_dir)
    scores <- score_irt(trials, mod_spec, mod_rec)
  } else if (task %in% cat_tasks) {
    scores <- score_cat(runs)
  } else if (task %in% names(custom_tasks)) {
    scoring_fun <- custom_tasks[[task]]
    scores <- purrr::exec(scoring_fun, trials, dataset)
  } else {
    message(glue::glue('--No scoring method found'))
    scores <- NULL
  }

  scores
}
