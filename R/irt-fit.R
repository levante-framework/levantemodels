#' mapping from invariance names to corresponding mirt argument
#'
#' Named list giving, for each invariance level that can be used in the
#' `invariance` column of `models` in [fit_task_models_multigroup()], the value
#' passed to the `invariance` argument of [mirt::multipleGroup()].
#' @export
invariances <- list(
  configural = "",
  metric = c("slopes"),
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
#' lower asymptote (guessing parameter).
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
#' @returns Called for its side effect of writing model files (returns `NULL`
#'   without fitting anything if `task` isn't in `task_data` or no items are
#'   shared by all groups).
fit_task_models_multigroup <- \(task_data, models, priors, task, group,
                                registry_dir) {

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
      model_str_overlap <- generate_model_str_numeric(data_filtered_overlap, data_prepped_overlap, itemtype, nfact, priors)

      # fit overlap items model
      mod_overlap <- mirt::multipleGroup(
        data = data_prepped_overlap,
        itemtype = itemtype,
        model = mirt::mirt.model(model_str_overlap),
        group = data_wide_overlap$group,
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

      model_str_full <- generate_model_str_numeric(data_filtered_full, data_prepped_full, itemtype, nfact, priors)
      # set up parameter structure of full model
      mod_pars <- mirt::multipleGroup(
        data = data_prepped_full,
        itemtype = itemtype,
        model = mirt::mirt.model(model_str_full),
        group = data_wide_full$group,
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
        group = data_wide_full$group,
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
