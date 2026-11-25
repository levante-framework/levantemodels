#' Slope classes of Same and Different items
#'
#' Classes of SDS items that share one slope in a `"2PLclass"` model (see
#' [fit_task_models_multigroup()]): the dimensions block, the same block, and
#' three classes of match-block picks. A match item is named
#' `sds_<k>match_choice<j>_<m>m<n>n`, the `j`-th pick of a `k`-pick trial after
#' `m` new matches and `n` new non-matches (see [recode_sds()]). Its class is:
#' - `match_first`: the trial's first pick;
#' - `match_after_nonmatch`: a later pick with an earlier non-match in the
#'   trial;
#' - `match_later`: any other later pick (earlier repeats, which count as
#'   neither match nor non-match, don't change its class).
#'
#' These classes were chosen in the 2026-11 recalibration
#' (levante-pilots `03_summaries/calibration/sds_slopes.R`): of 112 partitions
#' of the match items by pick position, earlier errors, set size and earlier
#' matches, they fit best by BIC in each half of the data and on the earlier
#' data. First picks and picks after a non-match discriminate about half as
#' well as other later picks.
#'
#' @param item_uids character vector of item UIDs
#' @returns character vector of slope classes, one per item; an item that
#'   isn't an SDS item of a known block gets its UID, i.e. its own slope
#' @export
#' @examples
#' sds_slope_classes(c("sds_dimensions_color", "sds_same_shape",
#'                     "sds_2match_choice1_0m0n", "sds_3match_choice3_1m1n",
#'                     "sds_3match_choice3_1m0n"))
sds_slope_classes <- \(item_uids) {
  match <- stringr::str_match(item_uids, "^sds_\\d+match_choice(\\d+)_(\\d+)m(\\d+)n$")
  pick <- as.integer(match[, 2])
  nonmatches <- as.integer(match[, 4])
  dplyr::case_when(
    stringr::str_starts(item_uids, "sds_dimensions_") ~ "dimensions",
    stringr::str_starts(item_uids, "sds_same_") ~ "same",
    pick == 1 ~ "match_first",
    nonmatches > 0 ~ "match_after_nonmatch",
    !is.na(pick) ~ "match_later",
    .default = item_uids
  )
}
