# turn json-ish response/distractors string into character vector
# e.g. "{'0': 'sm-blue-triangle', '1': 'lg-yellow-triangle'}" -> c("sm-blue-triangle", "lg-yellow-triangle")
parse_response <- \(resp) {
  if (is.na(resp) || stringr::str_trim(resp) == "") return(character(0))

  # convert single quotes to double quotes to parse as JSON
  json_str <- stringr::str_replace_all(resp, "'", '"')
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
  parsed |> as.character() |> unname()
}

# infer dimensions for one stimulus
# e.g. med-red-star / med-red-star-2 / med-red-star-striped /  med-red-star-2-striped
code_stim <- \(stim) {

  # extract number if present, other use default "1"
  num <- stringr::str_extract(stim, "\\d") |> purrr::discard(is.na)
  if (length(num) == 0) num <- "1"
  # extract background color if present, otherwise use default "white"
  bg <- stringr::str_extract(stim, "gray|black|striped") |> purrr::discard(is.na)
  if (length(bg) == 0) bg <- "white"

  # first 3 parts are always size/color/shape, then number and background from above
  att <- purrr::set_names(c(stim[1:3], num, bg), c("size", "color", "shape", "number", "background"))

}

# split vector of stimuli (e.g. med-green-circle-2-black) into parts and infer parts' dimensions
code_dims <- \(resp) {
  resp |> stringr::str_split("-") |> purrr::map(code_stim)
}

# given list of character vectors of named dimensions,
# return how many pairs of items match on each dimension,
# excluding dimensions that all items match on
match_opts_dims <- \(opts) {
  opts |> purrr::transpose() |> purrr::map(unlist) |> purrr::map(base::table) |>
    purrr::discard(\(x) length(x) == 1) |> purrr::map(\(x) sum(choose(x, 2))) |>
    unlist()
}

match_resp_dims <- \(resp, opts_dims) {
  resp_t <- resp |> purrr::transpose() |> purrr::map(unlist)
  resp_t[names(opts_dims)] |> purrr::map(n_distinct) |>
    purrr::keep(\(x) x < length(resp)) |> names() |> sort()
}

# given list of character vectors of named dimensions,
# return which dimension(s) match for all items (out of size/color/shape)
same_dim <- \(opts) {
  opts |> purrr::transpose() |> purrr::keep_at(c("size", "color", "shape")) |>
    purrr::map(unlist) |> purrr::map(base::table) |>
    purrr::keep(\(x) length(x) == 1) |> names()
}

code_misses <- \(opts_dims, resp_dims, k) {
  trial_dims <- resp_dims |> unlist() |> unique()
  if (n_distinct(trial_dims) == k & all(trial_dims %in% opts_dims)) return(character())
  setdiff(opts_dims, trial_dims)
}
