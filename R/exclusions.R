#' Remove excluded items from trial data
#'
#' Removes the trials matched by any applicable row of the item exclusion
#' table ([fetch_exclusions()]). Each row documents one decision to exclude an
#' item, optionally only for some datasets, languages, or dates. Released trial
#' data keep all trials; exclusions should be applied before fitting models
#' (e.g. in the calibration notebook) and can be applied when scoring via
#' [score()].
#'
#' | column | type | required | meaning | example |
#' |---|---|---|---|---|
#' | `item_uid` | text | yes | item uid as produced by [recode_trials()]; for Theory of Mind, a uid without a story matches that question in every story | `tom_reference_reference`, `tom_story4_deception_reality_check_2` |
#' | `dataset` | text | no | dataset the row applies to; `NA` = all datasets | `pilot_uniandes_co_bogota` |
#' | `language` | text | no | language the row applies to, exactly as in the trial `language` column; `NA` = all languages | `es-CO` |
#' | `start_date` | date | no | first date excluded (inclusive); `NA` = unbounded | `2024-10-24` |
#' | `end_date` | date | no | date the exclusion ends (exclusive); `NA` = unbounded | `2024-07-01` |
#' | `exclude` | logical | no | only `TRUE` rows apply; if the column is absent, every row applies | `TRUE` |
#' | `reason` | text | no | documentation only | `below chance since image 4g` |
#'
#' Multi-valued Airtable fields (`dataset`, `language`) are synced as one row
#' per value. Absent optional columns are treated as `NA`; a blank `dataset` or
#' `language` must be `NA`, and `""` or `"NULL"` is an error. POSIXct
#' timestamps are compared as UTC dates. Character timestamps are compared by
#' their written date, so they must be UTC as stored on Redivis.
#'
#' @param trials trial data after [recode_trials()], with columns `item_uid`
#'   and whichever of `dataset`, `language`, `timestamp` the exclusions use
#' @param exclusions exclusion table, e.g. from [fetch_exclusions()]
#' @returns `trials` without the excluded trials.
#' @export
apply_exclusions <- \(trials, exclusions) {
  scope_cols <- c(dataset = "dataset", language = "language",
                  start_date = "timestamp", end_date = "timestamp")
  exclusions[setdiff(names(scope_cols), names(exclusions))] <- NA
  if ("exclude" %in% names(exclusions)) {
    exclusions <- exclusions |> filter(as.logical(.data$exclude) %in% TRUE)
  }
  if (any(c(exclusions$dataset, exclusions$language) %in% c("", "NULL"))) {
    stop('blank dataset or language in exclusions must be NA, not "" or "NULL"', call. = FALSE)
  }

  # every trial column that some exclusion is scoped by must be present
  used <- names(scope_cols) |> purrr::keep(\(col) any(!is.na(exclusions[[col]])))
  missing_cols <- setdiff(scope_cols[used], names(trials))
  if (length(missing_cols) > 0) {
    stop("exclusions are scoped by trial column(s) missing from trials: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  rules <- exclusions |>
    transmute(.uid = as.character(.data$item_uid),
              ex_dataset = .data$dataset, ex_language = .data$language,
              ex_start = as.Date(.data$start_date), ex_end = as.Date(.data$end_date))

  # match each trial on its item_uid and, for ToM, also on its story-less uid
  keys <- trials |>
    ungroup() |>
    mutate(.row = row_number(), .uid = as.character(.data$item_uid)) |>
    select(".row", ".uid", any_of(unique(scope_cols)))
  keys[setdiff(unique(scope_cols), names(keys))] <- NA
  keys <- bind_rows(
    keys,
    keys |> mutate(.uid = stringr::str_replace(.data$.uid, "^tom_story[0-9]+_", "tom_"))
  )

  excluded <- keys |>
    inner_join(rules, by = ".uid", relationship = "many-to-many", na_matches = "never") |>
    mutate(.date = as.Date(.data$timestamp)) |>
    filter(is.na(.data$ex_dataset) | .data$dataset == .data$ex_dataset,
           is.na(.data$ex_language) | .data$language == .data$ex_language,
           is.na(.data$ex_start) | .data$.date >= .data$ex_start,
           is.na(.data$ex_end) | .data$.date < .data$ex_end) |>
    pull(".row")

  drop <- seq_len(nrow(trials)) %in% excluded
  message(glue::glue("Excluding {sum(drop)} of {nrow(trials)} trials"))
  trials[!drop, , drop = FALSE]
}
