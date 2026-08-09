### functions to prep data for modeling

#' add identifiers for each instance of each item
#' @export
#'
#' @inheritParams generate_model_str_numeric
dedupe_items <- function(df, item_sep = "-") {
  df |>
    group_by(.data$run_id, .data$item_uid) |>
    mutate(instance = seq_along(.data$item_uid)) |> # i
    ungroup() |>
    mutate(item_inst = glue::glue("{item_uid}{item_sep}{instance}")) # item_i
}

#' remove items with no variance
#' @export
#'
#' @inheritParams generate_model_str_numeric
remove_no_var_items <- function(df) {
  df |>
    group_by(.data$item_inst) |>
    filter(n_distinct(.data$correct) > 1) |>
    ungroup()
}

#' remove items that aren't shared across all groups
#' @export
#'
#' @inheritParams generate_model_str_numeric
remove_nonshared_items <- function(df) {
  n_groups_total <- length(unique(df$group))

  df |>
    group_by(.data$item_uid) |>
    mutate(n_groups = n_distinct(.data$group)) |>
    ungroup() |>
    filter(.data$n_groups == n_groups_total) # need to be in all groups
}

#' remove items with no invariance in any single group
#' @export
#'
#' @inheritParams generate_model_str_numeric
remove_no_var_items_bygroup <- function(df) {
  df |>
    group_by(.data$item_inst, .data$group) |>
    mutate(n_cat_group = n_distinct(.data$correct)) |>
    group_by(.data$item_inst) |>
    # all groups need to have more than one response category AND
    # the number of response categories needs to be the same for all groups
    filter(all(.data$n_cat_group > 1), n_distinct(.data$n_cat_group) == 1) |>
    ungroup()
}

#' format data for mirt
#' @export
#'
#' @inheritParams generate_model_str_numeric
to_mirt_shape <- function(df) {
  df |>
    mutate(correct = as.numeric(.data$correct)) |> # values to numeric
    select("run_id", "item_inst", "correct") |>
    tidyr::pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    tibble::column_to_rownames("run_id") # user_id to rownames
}

#' format data for mirt
#' @export
#'
#' @inheritParams generate_model_str_numeric
to_mirt_shape_grouped <- function(df) {
  df |>
    mutate(correct = as.numeric(.data$correct)) |> # values to numeric
    select("run_id", "group", "item_inst", "correct") |>
    tidyr::pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    tibble::column_to_rownames("run_id") # user_id to rownames
}

paste_c <- \(...) paste(..., collapse = ",")

#' generates mirt model strings
#'
#' @param df trial data
#' @param df_prepped trial data in wide mirt shape (runs x items)
#' @param item_type "Rasch", "2PL", etc
#' @param f number of factors
#' @param priors list of priors
#' @param item_sep string to put in between item_uid and instance index (defaults to "-")
#'
#' @returns string that can be passed to `mirt.model()` as `input`
#' @export
generate_model_str_numeric <- function(df, df_prepped, item_type, f, # f = num factors
                                       priors = NULL, item_sep = "-") {

  items <- df |> pull(.data$item_uid) |> unique() # item ids

  # F[i] = 1-K statement for each factor
  factors <- purrr::map_chr(1:f, \(i) glue::glue("F{i} = 1-{ncol(df_prepped)}"))

  params <- "d" # always have difficulty
  if (item_type != "Rasch") {
    params <- c(params, "a1")
  }

  item_params <- df |>
    group_by(.data$item_uid) |>
    summarise(n_cat = n_distinct(.data$correct)) |>
    mutate(params = purrr::map(.data$n_cat, \(nc) if (nc == 2) params else paste0(params, 1:(nc-1)))) |>
    select("item_uid", "params") |>
    tibble::deframe()

  constraints <- items |> purrr::map(\(item_uid) {
    # get columns with item's instances by checking for columns that start with
    # item_uid + item_sep
    prefixes <- stringr::str_sub(colnames(df_prepped), start = 1, end = stringr::str_length(item_uid) + 1)
    matched_idx <- which(prefixes == paste0(item_uid, item_sep))

    if (length(matched_idx) > 1) {
      # constraint for item instance: (item_1, item_2, param)
      purrr::map_chr(item_params[[item_uid]], \(p) glue::glue("({paste_c(matched_idx)},{p})")) |> paste_c()
    }
  }) |> purrr::compact() |> paste_c() # combine into CONSTRAIN statement
  constraint <- if (stringr::str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""

  # e.g. PRIOR = (2-3, 5, d, norm, 0, 1), (4, d, norm, 0, 0.5)')
  # prior_terms <- priors |> imap(\(pr, param) glue("(1-{length(items)},{paste(c(param, pr),collapse = ',')})"))
  # prior <- if (length(prior_terms) > 0) glue("PRIOR={paste(prior_terms, collapse = ',')}") else ""
  prior_terms <- priors |>
    purrr::imap(\(pr, param) glue::glue("(1-{ncol(df_prepped)},{paste(c(param, pr),collapse = ',')})"))
  prior <- if (length(prior_terms) > 0) glue::glue("PRIOR={paste(prior_terms, collapse = ',')}") else ""

  # combine statements
  paste(c(factors, constraint, prior), collapse = "\n")
}
