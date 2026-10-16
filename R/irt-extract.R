#' Get item parameters of fitted mirt model
#' @export
#' @param mod fit mirt model (SingleGroupClass or MultiGroupClass object)
#' @param IRTpars logical; convert slope intercept parameters into traditional IRT parameters?
mirt_coefs <- function(mod, IRTpars = TRUE) {
  stats::coef(mod, simplify = TRUE, IRTpars = IRTpars)$items |> as_tibble(rownames = "item")
}

#' Get participant scores of fitted mirt model
#' @export
#' @inheritParams generate_model_str_numeric
#' @inheritParams mirt_coefs
mirt_scores <- function(mod, df, df_prepped) {
  scores <- mirt::fscores(mod, method = "EAP", verbose = FALSE)
  user_scores <- tibble(run_id = rownames(df_prepped),
                        ability = scores[,1])
  df |> distinct(.data$user_id, .data$run_id) |> left_join(user_scores)
}

#' Get AIC of fitted mirt model
#' @export
#' @inheritParams mirt_coefs
mirt_aic <- function(mod) mod@Fit$AIC

#' Get BIC of fitted mirt model
#' @export
#' @inheritParams mirt_coefs
mirt_bic <- function(mod) mod@Fit$BIC

#' Get coefficients for multigroup model
#' @export
#' @inheritParams mirt_coefs
multigroup_coefs <- \(mod) {
  purrr::transpose(stats::coef(mod, simplify = TRUE))$items |>
    purrr::map(purrr::partial(as_tibble, rownames = "item")) |>
    purrr::list_rbind(names_to = "site")
}

#' Get groups out of multigroup model
#' @export
#' @inheritParams mirt_coefs
multigroup_extract_groups <- \(mod) {
  mod@Data$groupNames |> purrr::set_names() |> purrr::map(\(gr) mirt::extract.group(mod, group = gr))
}

#' Get item fits out of multigroup model
#' @export
#' @param submods list of SingleGroupClass mirt models
#' @param fit_stats character vector indicating which fit statistics should be computed
multigroup_itemfit <- \(submods, fit_stats) {
  submods |>
    purrr::map(\(submod) mirt::itemfit(submod, fit_stats = fit_stats) |> as_tibble()) |>
    purrr::list_rbind(names_to = "group")
}
