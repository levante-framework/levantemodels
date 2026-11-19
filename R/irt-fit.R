#' Mapping from invariance names to corresponding mirt argument
#'
#' Named list giving, for each invariance level that can be used in the
#' `invariance` column of `models` in [fit_task_models_multigroup()], the value
#' passed to the `invariance` argument of [mirt::multipleGroup()].
#'
#' mirt fixes every group's latent distribution at N(0, 1) unless `free_means`
#' or `free_var` is given, and always fixes the reference (first) group's.
#' So:
#' - `configural`: item parameters free in every group; all groups N(0, 1).
#' - `metric`: slopes equal across groups; latent variances free (identified
#'   by the equal slopes), means fixed at 0.
#' - `scalar`: slopes and intercepts equal; means and variances free.
#'
#' Each level adds constraints to the one before, so the three are nested.
#' (Before 2026-11, `metric` was `"slopes"` alone, following an earlier mirt
#' example, which also held every group's variance at 1.)
#' @export
invariances <- list(
  configural = "",
  metric = c("slopes", "free_var"),
  scalar = c("free_means", "free_var", "intercepts", "slopes")
)

#' For a given task + subsetting variable/value, fit set of pooled models
#'
#' Fits each model in `models` as a single-group model to the trials for
#' `task` where `subset_var == subset_val`, and saves each one as a
#' [ModelRecord] at
#' `{registry_dir}/{task}/by_{subset_var}/{subset_val}/{task}_{itemtype}_f{nfact}.rds`
#' (with `itemtype` lowercased). See `vignette("fitting-irt-models")` for a
#' worked example.
#'
#' Before fitting, trials with `NA` `correct` are dropped, repeated
#' presentations of an item within a run are treated as separate instances
#' whose parameters are constrained to be equal, and item instances with no
#' variance in responses are dropped. Each item's `chance` is used as its fixed
#' lower asymptote (guessing parameter), except for items with
#' `free_guess = TRUE`, whose lower asymptote is estimated (see
#' [generate_model_str_numeric()]).
#'
#' @param task_data data frame nested by task and grouping variables, with
#'   columns `item_task`, the grouping variable(s) (e.g. `language`,
#'   `dataset`), and `data`, a list-column of trial-level data frames with
#'   (at least) columns `run_id`, `item_uid`, `correct` (logical or 0/1), and
#'   `chance` (chance level of the item)
#' @param models data frame of models to fit, one per row, with columns
#'   `nfact` (number of factors) and `itemtype` (`"Rasch"` or `"2PL"`), and
#'   no other columns
#' @param priors list of priors, where names are parameter names (e.g. d, a1)
#'   and values are vectors of length 3 (priorType, val1, val2), e.g.
#'   `list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))`
#' @param task one value in item_task
#' @param subset_var variable to subset by (string or bare variable); must be
#'   a column of `task_data` (not of the nested `data`)
#' @param subset_val value to subset to (string)
#' @param registry_dir string indicating directory in which to save models
#'
#' @returns Called for its side effect of writing model files.
#' @export
fit_task_models_pooled <- \(task_data, models, priors, task, subset_var, subset_val, registry_dir) {

  var_name <- rlang::as_name(rlang::enquo(subset_var))

  # filter task data to given task + language
  trials <- task_data |>
    filter(.data$item_task == task) |>
    filter(.data[[var_name]] == subset_val) |>
    tidyr::unnest("data") |>
    filter(!is.na(.data$correct))

  # prep data for modeling
  data_filtered <- trials |> dedupe_items() |> remove_no_var_items()
  data_prepped <- to_mirt_shape(data_filtered)

  # pull out chance values
  guess <- data_filtered |> distinct(.data$item_inst, .data$chance) |> pull(.data$chance)

  # construct output directory
  out_dir <- file.path(registry_dir, task, paste0("by_", var_name), subset_val)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # iterate over nfact + itemtype
  mods <- models |> purrr::pwalk(
    \(nfact, itemtype) {
      message(glue::glue("fitting {task} {subset_val} model ({nfact} factor {itemtype})"))

      # generate model string with item constraints + dimensionality
      model_str <- generate_model_str_numeric(data_filtered, data_prepped, itemtype, nfact, priors)

      # fit model!
      mod <- mirt::mirt(
        data = data_prepped,
        itemtype = itemtype,
        model = mirt::mirt.model(model_str),
        guess = guess,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )

      # construct model record out of model
      mod_rec <- modelrecord(mod, rownames(data_prepped))

      # save model record
      mod_file <- glue::glue("{task}_{stringr::str_to_lower(itemtype)}_f{nfact}.rds")
      readr::write_rds(mod_rec, file.path(out_dir, mod_file), compress = "gz")
    })
}

#' Wrapper: given task, fit by_language model for each language
#'
#' Calls [fit_task_models_pooled()] with `subset_var = "language"` for each
#' language present for `task`, so `task_data` must have a `language` column.
#' @returns Called for its side effect of writing model files.
#' @export
#' @inheritParams fit_task_models_pooled
fit_bylanguage_task <- \(task_data, models, priors, task, registry_dir) {
  task_data |> filter(.data$item_task == task) |> pull(.data$language) |> unique() |>
    purrr::walk(\(lang) fit_task_models_pooled(task_data, models, priors, task, "language", lang, registry_dir),
                .progress = TRUE)
}

#' Wrapper: given language, fit by_language model for each task
#'
#' Calls [fit_task_models_pooled()] with `subset_var = "language"` for each
#' task present for `lang`, so `task_data` must have a `language` column.
#' @returns Called for its side effect of writing model files.
#' @export
#' @inheritParams fit_task_models_pooled
#' @param lang string indicating language
fit_bylanguage_lang <- \(task_data, models, priors, lang, registry_dir) {
  task_data |> filter(.data$language == lang) |> pull(.data$item_task) |> unique() |>
    purrr::walk(\(task) fit_task_models_pooled(task_data, models, priors, task, "language", lang, registry_dir),
                .progress = TRUE)
}


#' For a given task and group variable, fit and record set of multigroup models
#'
#' For each model in `models`, fits a multigroup model to the trials for
#' `task`, with each level of `group` as a group, in up to two steps:
#'
#' 1. An `overlap_items` model fit to only the items that are present (and
#'    have variance) in every group.
#' 2. For models with metric or scalar invariance, an `all_items` model fit to
#'    all items, with the parameters of the overlapping items fixed at their
#'    values from the `overlap_items` model, so that these items anchor the
#'    scale for the remaining items.
#'
#' Each model is saved as a [ModelRecord] at
#' `{registry_dir}/{task}/multigroup_{group}/{overlap_items|all_items}/{task}_{itemtype}_f{nfact}_{invariance}.rds`
#' (with `itemtype` lowercased). Data preparation is as in
#' [fit_task_models_pooled()]. See `vignette("fitting-irt-models")` for a
#' worked example.
#'
#' @export
#' @inheritParams fit_task_models_pooled
#' @param models data frame of models to fit, one per row, with columns
#'   `nfact` (number of factors), `itemtype` (`"Rasch"` or `"2PL"`), and
#'   `invariance` (a name in [invariances]: `"configural"`, `"metric"`, or
#'   `"scalar"`), and no other columns
#' @param group variable to use as groups (string or bare variable); can be a
#'   column of either `task_data` or the nested `data`
#' @param reference_group group whose latent distribution is fixed at N(0, 1)
#'   when means and variances are freed (metric and scalar invariance); if
#'   `NULL` (default), the alphabetically first group, as in [mirt::multipleGroup()]
#' @returns Called for its side effect of writing model files (returns `NULL`
#'   without fitting anything if `task` isn't in `task_data` or no items are
#'   shared by all groups).
fit_task_models_multigroup <- \(task_data, models, priors, task, group,
                                registry_dir, reference_group = NULL) {

  group_name <- rlang::as_name(rlang::enquo(group))

  if (!(task %in% unique(task_data$item_task))) {
    message(glue::glue("Task {task} not present in task_data"))
    return()
  }

  # filter task data to given task
  trials <- task_data |>
    filter(.data$item_task == task) |>
    tidyr::unnest("data") |>
    rename(group = all_of(group_name)) |>
    mutate(item_uid = as.character(.data$item_uid)) |>
    filter(!is.na(.data$correct))

  # order groups so that the reference group is first (mirt fixes the first
  # group's distribution); as_group() drops groups absent from a subset
  group_levels <- sort(unique(trials$group))
  if (!is.null(reference_group)) {
    stopifnot("reference_group must be one of the groups" = reference_group %in% group_levels)
    group_levels <- c(reference_group, setdiff(group_levels, reference_group))
  }
  as_group <- \(g) factor(g, levels = intersect(group_levels, g))

  # prep data for modeling
  data_filtered_full <- trials |> dedupe_items() |> remove_no_var_items()
  data_wide_full <- data_filtered_full |> to_mirt_shape_grouped()
  data_prepped_full <- data_wide_full |> select(-"group")

  # pull out chance values
  guess_full <- data_filtered_full |> distinct(.data$item_inst, .data$chance) |> pull(.data$chance)

  data_filtered_overlap <- data_filtered_full |>
    remove_nonshared_items() |>
    remove_no_var_items_bygroup()
  data_wide_overlap <- data_filtered_overlap |> to_mirt_shape_grouped()
  data_prepped_overlap <- data_wide_overlap |> select(-"group")
  guess_overlap <- data_filtered_overlap |> distinct(.data$item_inst, .data$chance) |> pull(.data$chance)

  if (nrow(data_filtered_overlap) == 0) {
    message("No items in common to all groups, skipping multigroup models")
    return()
  }

  # construct output directory
  out_dir <- file.path(registry_dir, task, paste0("multigroup_", group_name))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  mods <- models |> purrr::pmap(
    \(nfact, itemtype, invariance) {
      message(glue::glue("fitting {task} multigroup model ({nfact} factor {itemtype} with {invariance} invariance)"))

      # generate model string with item constraints + dimensionality
      model_str_overlap <- generate_model_str_numeric(data_filtered_overlap, data_prepped_overlap, itemtype, nfact, priors,
                                                      constrain_guess_groups = invariance == "scalar")

      # fit overlap items model
      mod_overlap <- mirt::multipleGroup(
        data = data_prepped_overlap,
        itemtype = itemtype,
        model = mirt::mirt.model(model_str_overlap),
        group = as_group(data_wide_overlap$group),
        invariance = invariances[[invariance]],
        guess = guess_overlap,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )

      # construct model record out of model
      mod_rec_overlap <- modelrecord(mod_overlap, rownames(data_prepped_overlap))
      mod_file <- glue::glue("{task}_{stringr::str_to_lower(itemtype)}_f{nfact}_{invariance}.rds")
      out_dir_overlap <- file.path(out_dir, "overlap_items")
      dir.create(out_dir_overlap, recursive = TRUE, showWarnings = FALSE)
      readr::write_rds(mod_rec_overlap, file.path(out_dir_overlap, mod_file), compress = "gz")

      if (invariance == "configural") return()

      item_inst_map <- data_filtered_full |>
        distinct(.data$item_uid, .data$item_inst) |>
        rename(item = .data$item_inst) |>
        mutate(item = as.character(.data$item))

      overlap_pars <- mirt::mod2values(mod_overlap)
      # get item parameter values from overlap model
      overlap_vals <- overlap_pars |> #as_tibble() |>
        select("group", "item", "name", fixed_value = "value") |>
        # remove group mean/cov
        filter(.data$item != "GROUP") |>
        # mark params to not be estimated and ignore equality constraints
        mutate(fixed_est = FALSE, fixed_const = "none") |>
        # map item instances back to item UIDs
        left_join(item_inst_map, by = "item") |>
        select(-"item") |>
        distinct()

      model_str_full <- generate_model_str_numeric(data_filtered_full, data_prepped_full, itemtype, nfact, priors,
                                                   constrain_guess_groups = invariance == "scalar")
      # set up parameter structure of full model
      mod_pars <- mirt::multipleGroup(
        data = data_prepped_full,
        itemtype = itemtype,
        model = mirt::mirt.model(model_str_full),
        group = as_group(data_wide_full$group),
        invariance = invariances[[invariance]],
        pars = "values",
        guess = guess_full
      ) #|> as_tibble()

      # change values for overlapping items to values from overlap model
      mod_pars_fixed <- mod_pars |>
        # map item instances back to item UIDs
        left_join(item_inst_map, by = "item") |>
        # add in parameter values from overlap by item UID
        left_join(overlap_vals, by = c("group", "name", "item_uid")) |>
        # use fixes values from overlap when available, mark to not estimate
        mutate(value = if_else(!is.na(.data$fixed_value), .data$fixed_value, .data$value),
               est = if_else(!is.na(.data$fixed_est), .data$fixed_est, .data$est),
               const = if_else(!is.na(.data$fixed_const), .data$fixed_const, .data$const)) |>
        select(-"fixed_value", -"fixed_est", -"fixed_const", -"item_uid")
      attr(mod_pars_fixed, "itemtype") <- itemtype

      # fit full items model
      mod_full <- mirt::multipleGroup(
        data = data_prepped_full,
        itemtype = itemtype,
        group = as_group(data_wide_full$group),
        invariance = invariances[[invariance]],
        pars = mod_pars_fixed,
        guess = guess_full,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )

      # construct model record out of model
      mod_rec_full <- modelrecord(mod_full, rownames(data_prepped_full))

      # save model record
      mod_file <- glue::glue("{task}_{stringr::str_to_lower(itemtype)}_f{nfact}_{invariance}.rds")
      out_dir_full <- file.path(out_dir, "all_items")
      dir.create(out_dir_full, recursive = TRUE, showWarnings = FALSE)
      readr::write_rds(mod_rec_full, file.path(out_dir_full, mod_file), compress = "gz")

    })
}

#' Add new groups to a scalar multigroup model
#'
#' Estimates the latent mean and variance of each new group in `task_data`
#' with every item parameter and every existing group's mean and variance held
#' fixed at their values in `mod_rec` (fixed item parameter calibration), and
#' returns a [ModelRecord] of the model with the new groups added. Nothing is
#' written to disk.
#'
#' The model is fit to the record's calibration data together with the new
#' groups' data, so that every item has the response categories it was
#' calibrated with. Since everything else is fixed, each new group's estimates
#' depend only on its own data, and are the same whether groups are added
#' together or one at a time. The existing groups' parameters are unchanged,
#' so their scores are too.
#'
#' Trials are prepared as for scoring: `NA` `correct` is dropped and
#' repeated presentations are numbered by [dedupe_items()]. Items not in the
#' model are dropped (items with no variance are kept, since their parameters
#' are fixed), as are runs with no responses to the model's items. This
#' assumes scalar invariance holds for the new groups and does not test it.
#'
#' In the returned record, each new group's parameter values are those of the
#' model's second group with its own mean and variance, and the fit statistics
#' and convergence are those of the estimation of the new groups' means and
#' variances. `mod_rec@fit$added_groups` accumulates, over successive
#' additions, a tibble with each added group's number of runs, number of the
#' model's items with responses, and estimated mean and variance with their
#' standard errors.
#'
#' @inheritParams fit_task_models_multigroup
#' @param mod_rec ModelRecord object for a scalar multigroup model
#' @param group variable whose values are the new groups (string or bare
#'   variable); can be a column of either `task_data` or the nested `data`.
#'   None of its values can already be groups in `mod_rec`.
#' @param min_runs minimum number of runs for a new group
#' @returns ModelRecord object
#' @export
add_groups_multigroup <- \(mod_rec, task_data, task, group, min_runs = 100) {

  group_name <- rlang::as_name(rlang::enquo(group))

  stopifnot("mod_rec must be a scalar multigroup model" =
              model_class(mod_rec) == "MultipleGroupClass" &&
              setequal(mod_rec@invariance, invariances$scalar))

  trials <- task_data |>
    filter(.data$item_task == task) |>
    tidyr::unnest("data") |>
    rename(group = all_of(group_name)) |>
    mutate(item_uid = as.character(.data$item_uid)) |>
    filter(!is.na(.data$correct))

  # align new data to the model's items, dropping runs with none of them
  data_wide <- trials |> dedupe_items() |> to_mirt_shape_grouped()
  resp <- data_wide |> select(-"group") |> as.matrix()
  data_new <- matrix(NA_real_, nrow(resp), length(items(mod_rec)),
                     dimnames = list(rownames(resp), items(mod_rec)))
  shared_items <- intersect(items(mod_rec), colnames(resp))
  data_new[, shared_items] <- resp[, shared_items]
  has_resp <- rowSums(!is.na(data_new)) > 0
  if (any(!has_resp)) message(glue::glue("Dropping {sum(!has_resp)} runs with no responses to the model's items"))
  data_new <- data_new[has_resp, , drop = FALSE]
  groups_new <- data_wide$group[has_resp]

  new_groups <- sort(unique(groups_new))
  existing <- intersect(new_groups, mod_rec@group_names)
  if (length(existing) > 0) stop(glue::glue("Groups already in the model: {paste(existing, collapse = ', ')}"))
  n_runs <- table(groups_new)[new_groups]
  too_small <- new_groups[n_runs < min_runs]
  if (length(too_small) > 0) stop(glue::glue("Groups with fewer than {min_runs} runs: {paste(too_small, collapse = ', ')}"))

  # new groups' parameter values: the second group's (the first is the
  # reference group, whose mean is fixed), with new parameter numbers
  base_vals <- model_vals(mod_rec)
  template <- base_vals[base_vals$group == mod_rec@group_names[[2]], ]
  new_vals <- purrr::imap(new_groups, \(g, i) {
    vals <- template
    vals$group <- g
    vals$parnum <- max(base_vals$parnum) + (i - 1L) * nrow(template) + seq_len(nrow(template))
    vals
  }) |> purrr::list_rbind()
  ext_vals <- rbind(base_vals, new_vals)
  rownames(ext_vals) <- NULL

  # estimate only the new groups' means and variances (mirt only allows
  # equality constraints on estimated parameters)
  est_vals <- ext_vals
  est_vals$est <- est_vals$group %in% new_groups & est_vals$name %in% c("MEAN_1", "COV_11")
  est_vals$const <- "none"

  group_levels <- c(mod_rec@group_names, new_groups)
  mod <- mirt::multipleGroup(
    data = rbind(mod_rec@data, data_new),
    group = factor(c(mod_rec@groups, groups_new), levels = group_levels),
    pars = est_vals,
    SE = TRUE,
    verbose = FALSE,
    technical = list(NCYCLES = 5000)
  )

  # check that the fixed parameters didn't move (mirt can change them in the
  # last bit; the returned record keeps the original values)
  fitted_vals <- mirt::mod2values(mod)
  stopifnot(isTRUE(all.equal(fitted_vals$value[seq_len(nrow(base_vals))], base_vals$value,
                             tolerance = 1e-10)))

  # new groups' estimates and standard errors
  group_pars <- mirt::coef(mod, printSE = TRUE)
  added <- purrr::map(new_groups, \(g) {
    gp <- group_pars[[g]]$GroupPars
    dplyr::tibble(group = g, n_runs = as.numeric(n_runs[[g]]),
                  n_items = sum(colSums(!is.na(data_new[groups_new == g, , drop = FALSE])) > 0),
                  mean = gp["par", "MEAN_1"], mean_se = gp["SE", "MEAN_1"],
                  var = gp["par", "COV_11"], var_se = gp["SE", "COV_11"])
  }) |> purrr::list_rbind()
  for (g in new_groups) {
    for (p in c("MEAN_1", "COV_11")) {
      ext_vals$value[ext_vals$group == g & ext_vals$name == p] <-
        fitted_vals$value[fitted_vals$group == g & fitted_vals$name == p]
    }
  }

  ext_rec <- modelrecord(mod, c(mod_rec@runs, rownames(data_new)))
  ext_rec@model_vals <- ext_vals
  ext_rec@invariance <- mod_rec@invariance
  ext_rec@fit$added_groups <- dplyr::bind_rows(mod_rec@fit$added_groups, added)
  ext_rec
}

# example calls: pooled models

# fit one task + one language
# fit_task_models_pooled(task_data = task_data_irt, models = models_pooled,
#                        priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
#                        task = "hf", subset_var = language, subset_val = "en")

# fit one task + all languages
# fit_bylanguage_task(task_data = task_data_irt, models = models_pooled,
#                        priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
#                 task = "mrot2d", registry_dir = regdir)

# fit all tasks + one language
# fit_bylanguage_lang(task_data = task_data_irt, models = models_pooled,
#                     priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
#                     lang = "en_us", registry_dir = regdir)

# fit all irt tasks + all languages
# irt_tasks |>
#   walk(\(task) fit_bylanguage_task(task_data = task_data_irt,
#                                 models = models_pooled,
#                                 priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
#                                 task = task,
#                                 registry_dir = regdir))


# example calls: multigroup models

# fit one task, grouped by site
# fit_task_models_multigroup(task_data = task_data_irt, models = models_multigroup,
#                            task = "mrot2d", group = site, registry_dir = regdir)

# fit all irt tasks, grouped by site
# irt_tasks |>
#   walk(\(task) fit_task_models_multigroup(task_data = task_data_irt,
#                                           models = models_multigroup,
#                                           priors = list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3)),
#                                           task = task,
#                                           group = site,
#                                           registry_dir = regdir))
