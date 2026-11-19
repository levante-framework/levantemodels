# Survey (CFA) scoring, structured like the IRT scoring (irt-score.R):
#
#   IRT                               survey
#   score_irt(trials, spec, rec)      score_survey_cfa(surveys, spec, rec, survey_items)
#   score(task, dataset, ...)         score_survey(survey_type, survey_model, dataset, ...)
#   process_dataset: nest task x      score_surveys(): nest survey_type x dataset,
#     dataset, pmap(score)              cross with the table's models, pmap(score_survey)
#
# Input: long survey data as produced by process_surveys() + link_surveys()
# (one row per survey x question [x linked child]). Scoring does not filter by
# age or validity; apply any such filter before scoring.
# Output: one row per scoring unit x factor. Caregiver household (`general`)
# factors have one row per respondent survey and are linked to children only
# by link_survey_scores().

# Long -> wide for one model, applying the reverse override and the frozen
# category map. Returns list(wide, n_items_in_data, info) where `wide` has id
# columns + items.
survey_prepare <- function(surveys, meta, survey_items) {
  stype <- meta$survey_type
  if (!is.numeric(surveys$value)) stop("`value` must be numeric (as returned by process_surveys())")
  d <- surveys |>
    filter(.data$survey_type == stype, .data$question %in% meta$items, !is.na(.data$value)) |>
    mutate(question = as.character(.data$question))
  # Reverse coding. The category map is keyed on values in CALIBRATION
  # orientation: process_surveys() reversal under the calibration-time
  # metadata flags (meta$reverse_flags), then the model's reverse overrides
  # (meta$reverse_overrides: items whose metadata flag the model does not
  # follow; they are flipped once more). `survey_items` must be the metadata
  # the data were processed with (process_surveys() uses the current version).
  # If an item's flag has changed since calibration, flip it back to
  # calibration orientation first.
  q_type <- if (stype == "child") "student" else stype
  flip <- function(it) {
    opts <- sort(unique(meta$category_map$raw_value[meta$category_map$item == it]))
    # min + max - value equals process_surveys()' reversal by position only for
    # equally spaced options
    if (length(unique(round(diff(opts), 8))) > 1)
      stop("cannot flip ", it, ": its response options are not equally spaced")
    d$value[d$question == it] <<- min(opts) + max(opts) - d$value[d$question == it]
  }
  # flags read exactly as process_surveys() reads them (NA -> FALSE)
  live <- survey_items |> filter(.data$survey_type == q_type, .data$variable %in% meta$items) |>
    transmute(item = .data$variable, live = suppressWarnings(as.logical(.data$reverse_coded)) |> tidyr::replace_na(FALSE)) |>
    distinct()
  fl <- meta$reverse_flags |> left_join(live, by = "item")
  if (anyNA(fl$live)) stop("items missing from survey metadata: ", paste(fl$item[is.na(fl$live)], collapse = ", "))
  changed <- fl$item[fl$live != fl$reverse_coded]
  for (it in changed) { message("reverse_coded flag of ", it, " changed since calibration: flipping back"); flip(it) }
  for (it in intersect(meta$reverse_overrides$item, meta$items)) flip(it)
  keys <- if (meta$unit == "respondent") c("team", "dataset", "survey_id", "respondent_id")
          else c("team", "dataset", "survey_id", "respondent_id", "child_id")
  # household answers are copied once per linked child: keep one copy
  d <- d |> distinct(across(all_of(c(keys, "question", "value"))))
  # model items present in the data (before conflicting answers are removed)
  n_items_in_data <- n_distinct(d$question)
  # frozen category map: raw value -> model category. The map covers every
  # response option of the metadata, so a value outside it is not a valid
  # option: stop instead of dropping it silently. Checked before conflicting
  # answers are removed, so an invalid value also stops when it conflicts.
  # (process_surveys() already sets values outside the options of reverse-coded
  # items to NA; those cannot be detected here.)
  cm <- meta$category_map |> select(question = "item", value = "raw_value", model_value = "model_value")
  d <- d |> left_join(cm, by = c("question", "value"))
  n_unmapped <- sum(is.na(d$model_value))
  if (n_unmapped > 0) {
    bad <- d |> filter(is.na(.data$model_value)) |> distinct(.data$question, .data$value)
    stop(sprintf("%s / %s: %d answer(s) with values not among the item's response options: %s",
                 stype, meta$model, n_unmapped, paste0(bad$question, "=", bad$value, collapse = ", ")))
  }
  # two different answers to the same question within one unit: drop that answer
  conflicts <- d |> count(across(all_of(c(keys, "question")))) |> filter(.data$n > 1)
  d <- d |> anti_join(conflicts, by = c(keys, "question"))
  wide <- d |>
    select(all_of(c(keys, "question", "model_value"))) |>
    tidyr::pivot_wider(names_from = "question", values_from = "model_value")
  # every unit of this survey type x part in the data keeps a row, also when
  # it answered none of the model's items (-> exclusion "no_items"), as IRT
  # keeps every run
  part <- if (meta$unit == "respondent_child") "specific" else "general"
  units <- surveys |> filter(.data$survey_type == stype, .data$survey_part == part)
  # child-part surveys with no linked child cannot form a unit. (link_surveys()
  # gives such surveys team and dataset NA, so they normally never reach this
  # point: score_surveys() lists them under `unscored` with dataset NA.)
  if (meta$unit == "respondent_child") units <- units |> filter(!is.na(.data$child_id))
  units <- units |> distinct(across(all_of(keys)))
  wide <- units |> left_join(wide, by = keys)
  for (it in setdiff(meta$items, names(wide))) wide[[it]] <- NA_real_
  wide <- wide |> select(all_of(c(keys, meta$items)))
  # unit "respondent": child survey -> the respondent is the child;
  # caregiver household part -> no single child (linked later)
  if (meta$unit == "respondent") wide$child_id <- if (stype == "child") wide$respondent_id else NA_character_
  list(wide = wide, n_items_in_data = n_items_in_data,
       info = tibble(survey_type = stype, survey_model = meta$model,
                     dataset = paste(unique(surveys$dataset), collapse = ","), n_units = nrow(wide),
                     n_conflicting_answers = nrow(conflicts), n_unmapped_values = n_unmapped,
                     reverse_flags_changed = paste(changed, collapse = ",")))
}

#' Score survey data from one dataset with one survey model record
#'
#' The survey counterpart of [score_irt()]: EBM factor scores
#' (`lavaan::lavPredict(method = "EBM")`) from a single-group model rebuilt
#' from the record's parameter table, for the scoring group named in `spec`
#' (or, for a new site, the pooled model of all groups: latent mean and
#' covariance of the equally weighted mixture of the groups, as
#' [pooled_group_model()] for IRT, and every other parameter averaged over the
#' groups, in the theta parameterization, so that the item residual variances
#' are averaged). The scorer is chosen from `rec$scheme` (`scorer_<scheme>()`).
#'
#' Every unit (survey, or survey x child for child-specific caregiver parts)
#' of the model's survey part gets one row per factor. `exclusion` is
#' `"no_items"` (no item of the model answered), `"incomplete_items"` (listwise
#' models: any item missing) or `"no_factor_items"` (none of this factor's own
#' items answered); `score` is `NA` for excluded rows. A value that is not one
#' of the item's response options stops scoring.
#'
#' @param surveys Long survey data from one dataset (and one survey type), as
#'   returned by [process_surveys()] + [link_surveys()]. Must have columns
#'   `team, dataset, survey_type, survey_part, survey_id, respondent_id,
#'   child_id, age, survey_timestamp, question, value` (`value` numeric).
#' @param spec Scoring specification from [get_survey_model_spec()].
#' @param rec Model record from [get_survey_model_record()].
#' @param survey_items Survey item metadata the data were processed with
#'   (e.g. [fetch_survey_items()]); used to check reverse-coding flags.
#' @return tibble with one row per unit x factor (columns `team, dataset,
#'   scoring_group, new_site, survey_type, survey_part, survey_id,
#'   respondent_id, child_id, age, survey_timestamp, survey_model, lv, factor,
#'   score, n_items_answered, n_factor_items_answered, n_items, exclusion,
#'   score_type, scoring_model, registry_version, scheme`); the data-prep
#'   summary is attached as attribute `"info"`. When nothing is scored, an
#'   empty tibble with attribute `"skip_reason"`.
#' @export
score_survey_cfa <- function(surveys, spec, rec, survey_items) {
  message("--Using CFA scoring (scheme ", rec$scheme, ")")
  meta <- rec$meta
  need <- c("team", "dataset", "survey_type", "survey_part", "survey_id", "respondent_id", "child_id",
            "age", "survey_timestamp", "question", "value")
  miss <- setdiff(need, names(surveys))
  if (length(miss)) stop("survey data lack columns: ", paste(miss, collapse = ", "))
  surveys <- ungroup(surveys)
  stopifnot("survey data must be from one dataset" = n_distinct(surveys$dataset) <= 1)
  skip <- \(reason) { message("--", reason); structure(tibble(), info = NULL, skip_reason = reason) }
  if (!nrow(surveys)) return(skip("no survey data"))
  if (is.null(spec$team) || is.na(spec$team)) stop("scoring-table row has no team for dataset ", spec$dataset)
  if (anyNA(surveys$team)) stop("survey data have NA in `team` for dataset ", spec$dataset)
  if (!all(surveys$team == spec$team))
    stop(sprintf("data team (%s) differs from the scoring-table team (%s) for %s",
                 paste(unique(surveys$team), collapse = ","), spec$team, spec$dataset))
  if (!isTRUE(spec$new_site) && !spec$scoring_group %in% meta$group_labels)
    stop("scoring group ", spec$scoring_group, " is not a group of model ", meta$model)
  # as in score_irt(): only a scalar-invariant model can score a group it was
  # not fitted on; otherwise skip
  if (isTRUE(spec$new_site) && !identical(meta$invariance, "scalar"))
    return(skip(paste0("new site needs a scalar-invariant model (", meta$model, " is ", meta$invariance, ")")))
  if (isTRUE(spec$new_site) && spec$scoring_group != "pooled")
    stop("a new site must have scoring group \"pooled\"")
  # scorer_<scheme>, looked up only where this function is defined (the package
  # namespace), never in the caller's or the global environment
  scheme_ok <- is.character(rec$scheme) && length(rec$scheme) == 1 && !is.na(rec$scheme)
  scorer <- if (scheme_ok) get0(paste0("scorer_", rec$scheme), envir = topenv(environment()),
                                mode = "function", inherits = FALSE)
  if (is.null(scorer))
    stop(sprintf("unknown record scheme \"%s\" (no scorer_%s)", format(rec$scheme), format(rec$scheme)))
  prep <- survey_prepare(surveys, meta, survey_items)
  w <- prep$wide
  # none of the model's items in this dataset's data: the questionnaire was
  # not given (IRT: the task is not in the data), so no rows
  if (prep$n_items_in_data == 0) return(structure(skip("none of the model's items in the data"), info = prep$info))
  if (!nrow(w)) return(structure(skip("no units"), info = prep$info))
  w$n_items_answered <- rowSums(!is.na(w[meta$items]))
  # items answered per factor: a factor with none of its own items answered
  # gets score NA / exclusion "no_factor_items" (below)
  nf <- purrr::map_dfc(purrr::set_names(meta$lv), \(l) rowSums(!is.na(w[meta$items_by_lv[[l]]])))
  w$scoring_group <- spec$scoring_group
  w$new_site <- isTRUE(spec$new_site)
  w$exclusion <- case_when(
    w$n_items_answered == 0 ~ "no_items",
    meta$lavaan_options$missing == "listwise" & w$n_items_answered < length(meta$items) ~ "incomplete_items",
    TRUE ~ NA_character_)
  fs <- matrix(NA_real_, nrow(w), length(meta$lv), dimnames = list(NULL, meta$lv))
  rows <- which(is.na(w$exclusion))
  if (length(rows)) {
    x <- scorer(rec, group = spec$scoring_group, new_site = isTRUE(spec$new_site),
                newdata = as.data.frame(w[rows, meta$items]))
    stopifnot(nrow(x) == length(rows))
    fs[rows, ] <- x[, meta$lv, drop = FALSE]
  }
  nf_long <- nf |> mutate(.row = row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "lv", values_to = "n_factor_items_answered")
  # survey part / timestamp / age of each unit
  first_age <- \(a) if (all(is.na(a))) NA_real_ else first(stats::na.omit(a))
  unit_info <- surveys |>
    group_by(.data$survey_id) |>
    summarise(survey_part = first(.data$survey_part), survey_timestamp = first(.data$survey_timestamp),
              age_survey = first_age(.data$age), .groups = "drop")
  # a caregiver child-part survey can cover several children: take the age of
  # the unit's own child where the data have it
  if ("child_id" %in% names(surveys)) {
    age_child <- surveys |> filter(!is.na(.data$child_id)) |> group_by(.data$survey_id, .data$child_id) |>
      summarise(age_child = first_age(.data$age), .groups = "drop")
    w <- w |> left_join(age_child, by = c("survey_id", "child_id"))
  } else w$age_child <- NA_real_
  unit_info <- unit_info |> mutate(age_unit = .data$age_survey) |> select(-"age_survey")
  scores <- bind_cols(w |> select(-all_of(meta$items)), as_tibble(fs)) |> mutate(.row = row_number()) |>
    tidyr::pivot_longer(all_of(meta$lv), names_to = "lv", values_to = "score") |>
    left_join(nf_long, by = c(".row", "lv")) |> select(-".row") |>
    # a factor with none of its own items answered gets no score (its EBM value
    # would come only from the other factors via the latent correlations).
    # The unit's other factors keep their scores.
    mutate(no_own = is.na(.data$exclusion) & .data$n_factor_items_answered == 0,
           score = if_else(.data$no_own, NA_real_, .data$score),
           exclusion = if_else(.data$no_own, "no_factor_items", .data$exclusion)) |>
    select(-"no_own") |>
    left_join(meta$factors, by = "lv") |>
    left_join(unit_info, by = "survey_id") |>
    # age is the child's age: undefined for household (unlinked) caregiver rows
    mutate(age = if_else(is.na(.data$child_id), NA_real_, coalesce(.data$age_child, .data$age_unit)),
           survey_type = meta$survey_type, survey_model = meta$model,
           n_items = unname(meta$n_items_by_lv[.data$lv]),
           score_type = "factor_score_ebm",
           scoring_model = paste(spec$model_type, spec$invariance, sep = "_"),
           registry_version = survey_spec_registry_version(spec),
           scheme = rec$scheme) |>
    select("team", "dataset", "scoring_group", "new_site", "survey_type", "survey_part", "survey_id",
           "respondent_id", "child_id", "age", "survey_timestamp", "survey_model", "lv", "factor", "score",
           "n_items_answered", "n_factor_items_answered", "n_items", "exclusion",
           "score_type", "scoring_model", "registry_version", "scheme")
  structure(scores, info = prep$info)
}

# Version label of the scoring table a spec came from. A table read from
# Redivis carries redivis_source (dataset scoped reference) and, as in
# score_irt(), the label is the version part of that reference; this wins over
# a registry_version column. A local table may carry registry_version. NA when
# neither is present.
survey_spec_registry_version <- function(spec) {
  has <- \(x) !is.null(x) && !is.na(x)
  if (has(spec$redivis_source)) return(stringr::str_extract(spec$redivis_source, "(?<=:)[^:]*$"))
  if (has(spec$registry_version)) return(spec$registry_version)
  NA_character_
}

#' Score one survey model for one dataset
#'
#' The survey counterpart of [score()]: looks up the scoring-table row, checks
#' the row (the same row checks as [score_surveys()]), gets the model record and
#' scores the dataset's surveys of that type with [score_survey_cfa()]. Unlike
#' [score_surveys()], it cannot check that no survey belongs to more than one
#' dataset (it sees one dataset only); use [score_surveys()] for a whole
#' notebook's data.
#'
#' @inheritParams get_survey_model_spec
#' @param surveys Long survey data (may contain other survey types and
#'   datasets; they are filtered out).
#' @inheritParams get_survey_model_record
#' @inheritParams score_survey_cfa
#' @return Score tibble as from [score_survey_cfa()], or `NULL` if the table
#'   has no row for this model and dataset.
#' @export
#' @examples
#' \dontrun{
#' scoring_table <- fetch_survey_scoring_table()
#' registry_dir <- fetch_survey_registry_dir()
#' survey_items <- fetch_survey_items()
#' scores <- score_survey("caregiver", "parenting", "pilot_mpieva_de_main",
#'                        surveys, scoring_table, registry_dir, survey_items)
#' }
score_survey <- function(survey_type, survey_model, dataset, surveys, scoring_table, registry_dir, survey_items) {
  message(sprintf('Scoring survey model "%s/%s" for dataset "%s"', survey_type, survey_model, dataset))
  spec <- get_survey_model_spec(survey_type, survey_model, dataset, scoring_table)
  if (is.null(spec)) {
    message("--No scoring model found")
    return(NULL)
  }
  # same row checks as score_surveys() (group belongs to the row's team, ...)
  check_survey_scoring_table(as_tibble(spec), registry_dir)
  rec <- get_survey_model_record(spec, registry_dir)
  d <- surveys |> ungroup() |> filter(.data$survey_type == !!survey_type, .data$dataset == !!dataset)
  score_survey_cfa(d, spec, rec, survey_items)
}

#' Score every survey model for every dataset in the data
#'
#' Nests the surveys by `survey_type` x `dataset`, crosses them with the models
#' the scoring table lists for that `survey_type` x `dataset`, and scores each
#' pair with [score_survey()]. The table rows that will be used are checked
#' first: required columns, one row per `survey_type` x `survey_model` x
#' `dataset`, record files present, and the scoring group matching the row's
#' team (or `"pooled"` for a new site); a problem stops scoring.
#'
#' Child-specific caregiver surveys without a linked child get `dataset` `NA`
#' from [link_surveys()]; they appear in `unscored` with `dataset` `NA`.
#'
#' @param surveys Long survey data of any survey types and datasets, as
#'   returned by [process_surveys()] + [link_surveys()]; every `survey_type` x
#'   `dataset` in it is scored. Must have the columns listed in
#'   [score_survey_cfa()].
#' @inheritParams score_survey
#' @return list with
#'   - `survey_scores`: one row per unit x factor (see [score_survey_cfa()]);
#'   - `info`: data-prep summary per model x dataset;
#'   - `unscored`: what was not scored and why (survey_type x dataset pairs
#'     with no table row, and table models with none of their items in that
#'     dataset's data).
#' @export
#' @examples
#' \dontrun{
#' res <- score_surveys(surveys, fetch_survey_scoring_table(),
#'                      fetch_survey_registry_dir(), fetch_survey_items())
#' res$survey_scores
#' }
score_surveys <- function(surveys, scoring_table, registry_dir, survey_items) {
  miss <- setdiff(c("survey_type", "dataset"), names(surveys))
  if (length(miss)) stop("survey data lack columns: ", paste(miss, collapse = ", "))
  # a survey (e.g. a caregiver general survey linked to children of two
  # datasets) must belong to one dataset, otherwise it would be scored twice
  multi <- surveys |> ungroup() |> distinct(.data$survey_type, .data$survey_id, .data$dataset) |>
    count(.data$survey_type, .data$survey_id) |> filter(.data$n > 1)
  if (nrow(multi)) stop(sprintf("%d survey(s) appear in more than one dataset; each survey must belong to one dataset", nrow(multi)))
  # check the table rows this run will use (IRT never looks at unused rows)
  check_survey_scoring_table(scoring_table |> semi_join(distinct(ungroup(surveys), .data$survey_type, .data$dataset),
                                                        by = c("survey_type", "dataset")), registry_dir)
  survey_data <- surveys |> ungroup() |>
    mutate(survey_type2 = .data$survey_type, dataset2 = .data$dataset) |>
    tidyr::nest(surveys = -c("survey_type2", "dataset2")) |>
    rename(survey_type = "survey_type2", dataset = "dataset2")
  jobs <- survey_data |>
    inner_join(scoring_table |> distinct(.data$survey_type, .data$survey_model, .data$dataset),
               by = c("survey_type", "dataset"), relationship = "one-to-many")
  no_row <- survey_data |> anti_join(scoring_table, by = c("survey_type", "dataset")) |>
    transmute(.data$survey_type, .data$dataset, survey_model = NA_character_,
              n_surveys = purrr::map_int(.data$surveys, \(s) n_distinct(s$survey_id)), reason = "no scoring-table row")
  jobs <- jobs |>
    mutate(scores = purrr::pmap(list(.data$survey_type, .data$survey_model, .data$dataset, .data$surveys),
                                \(t, m, ds, s) score_survey(t, m, ds, s, scoring_table, registry_dir, survey_items)),
           reason = purrr::map_chr(.data$scores, \(s) {
             r <- attr(s, "skip_reason")
             if (is.null(r)) NA_character_ else r
           }))
  skipped <- jobs |> filter(!is.na(.data$reason)) |>
    transmute(.data$survey_type, .data$dataset, .data$survey_model,
              n_surveys = purrr::map_int(.data$surveys, \(s) n_distinct(s$survey_id)), .data$reason)
  list(survey_scores = bind_rows(jobs$scores),
       info = purrr::map(jobs$scores, \(s) attr(s, "info")) |> bind_rows(),
       unscored = bind_rows(no_row, skipped))
}

#' Link household caregiver survey scores to children
#'
#' Household (`general`) caregiver factors are scored once per respondent
#' survey (`child_id` is `NA`); this copies each such score to every child that
#' survey is linked to, with that child's `age`. Use the unlinked scores for
#' anything about caregivers; use the linked ones only to join caregiver scores
#' onto child-level data.
#'
#' @param survey_scores `survey_scores` from [score_surveys()].
#' @param surveys The long survey data that were scored (with `child_id`
#'   from [link_surveys()]).
#' @return `survey_scores` in which each household row is replaced by one copy
#'   per linked child (column `linked_from_household` = `TRUE`); household rows
#'   whose survey has no linked child are kept as they are (`child_id` `NA`,
#'   `linked_from_household` = `FALSE`). Rows that already have a `child_id`
#'   are kept unchanged.
#' @export
link_survey_scores <- function(survey_scores, surveys) {
  # one row per survey x linked child, with that child's age
  links <- surveys |> ungroup() |> filter(!is.na(.data$child_id)) |>
    group_by(.data$survey_id, child_id_linked = .data$child_id) |>
    summarise(age_linked = if (all(is.na(.data$age))) NA_real_ else first(stats::na.omit(.data$age)), .groups = "drop")
  hh <- survey_scores |> filter(is.na(.data$child_id)) |> inner_join(links, by = "survey_id", relationship = "many-to-many") |>
    mutate(child_id = .data$child_id_linked, age = .data$age_linked, linked_from_household = TRUE) |>
    select(-"child_id_linked", -"age_linked")
  # household rows of surveys with no linked child: kept, not dropped
  unlinked <- survey_scores |> filter(is.na(.data$child_id)) |> anti_join(links, by = "survey_id") |>
    mutate(linked_from_household = FALSE)
  bind_rows(survey_scores |> filter(!is.na(.data$child_id)) |> mutate(linked_from_household = FALSE), hh, unlinked)
}
