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
      # group has its own mean and variance, so score the data as its own group
    } else if (!is.na(mod_spec$invariance) & mod_spec$invariance == "scalar") {
      # a group's variance can't be estimated from only a few runs (it
      # collapses toward zero), so small groups are scored from the first group
      min_group_runs <- 25
      if (nrow(data_prepped) >= min_group_runs) {
        new_group <- TRUE
      } else {
        message(glue::glue("--Fewer than {min_group_runs} runs for group not in model, scoring using group {mod_rec@group_names[[1]]}"))
        data_group <- mod_rec@group_names[[1]]
      }
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
    # data's group not in model: score under its own estimated mean and variance
    mod <- new_group_model(mod_rec, data_aligned, data_group)
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

#' single group model for a group not in a scalar multigroup model
#'
#' Estimates the new group's mean and variance by refitting the model on the
#' calibration data plus the new group's data with every item parameter and
#' every existing group's mean and variance fixed at their calibrated values
#' (the new group shares the reference group's item parameters). Returns a
#' single group model with the shared item parameters and the new group's
#' estimated mean and variance, for scoring.
#'
#' @param mod_rec ModelRecord object for a scalar multigroup model
#' @param data_new response matrix for the new group, columns in items(mod_rec) order
#' @param group name of the new group
#' @return mirt SingleGroupClass model
#' @keywords internal
new_group_model <- \(mod_rec, data_new, group) {
  stopifnot(length(group) == 1, identical(colnames(data_new), colnames(mod_rec@data)))
  message(glue::glue("--Group {group} not in model, estimating its mean and variance"))
  data_all <- rbind(mod_rec@data, as.matrix(data_new))
  groups_all <- c(mod_rec@groups, rep(group, nrow(data_new)))

  # parameter table for the combined data, filled with calibrated values
  vals <- mirt::multipleGroup(data = data_all, group = groups_all, pars = "values")
  mod_vals <- model_vals(mod_rec)
  source_group <- ifelse(vals$group == group, mod_rec@group_names[[1]], vals$group)
  val_match <- match(paste(source_group, vals$item, vals$name),
                     paste(mod_vals$group, mod_vals$item, mod_vals$name))
  stopifnot(!anyNA(val_match))
  vals$value <- mod_vals$value[val_match]

  # estimate only the new group's mean and variance
  vals$est <- vals$group == group & vals$name %in% c("MEAN_1", "COV_11")
  mod_new <- mirt::multipleGroup(data = data_all, group = groups_all, pars = vals, verbose = FALSE)
  group_pars <- mirt::coef(mod_new, simplify = TRUE)[[group]]

  # reference group's single group model with the new group's mean and variance
  # (not extract.group() on the new group, which fails for a one-run group)
  ref_vals <- mirt::mod2values(mirt::extract.group(mod_new, group = mod_rec@group_names[[1]]))
  ref_vals$value[ref_vals$name == "MEAN_1"] <- group_pars$means[1]
  ref_vals$value[ref_vals$name == "COV_11"] <- group_pars$cov[1, 1]
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
