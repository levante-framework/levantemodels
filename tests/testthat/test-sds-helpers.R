# Unit tests for the Same/Different Selection (SDS) parsing and dimension-coding
# helpers. These are intricate combinatorial functions with no other coverage;
# expected values are derived by hand from the function logic.

test_that("parse_response() turns a JSON-ish string into a character vector", {
  expect_equal(
    parse_response("{'0': 'sm-blue-triangle', '1': 'lg-yellow-triangle'}"),
    c("sm-blue-triangle", "lg-yellow-triangle")
  )
  expect_equal(parse_response(NA), character(0))
  expect_equal(parse_response("  "), character(0))
})

test_that("code_stim() infers size/color/shape with number and background defaults", {
  expect_equal(
    code_stim(c("med", "red", "star")),
    c(size = "med", color = "red", shape = "star",
      number = "1", background = "white")
  )
  # number and background are read from the trailing parts when present
  expect_equal(
    code_stim(c("med", "green", "circle", "2", "black")),
    c(size = "med", color = "green", shape = "circle",
      number = "2", background = "black")
  )
})

test_that("code_dims() codes each stimulus in a vector", {
  out <- code_dims(c("sm-red-star", "lg-red-circle"))
  expect_length(out, 2)
  expect_equal(out[[1]][["color"]], "red")
  expect_equal(out[[2]][["shape"]], "circle")
})

test_that("match_opts_dims() counts matching pairs on non-constant dimensions", {
  # three stimuli: two share color (red); size and shape all distinct; number
  # and background are constant (and so dropped)
  opts <- code_dims(c("sm-red-star", "lg-red-circle", "md-blue-square"))
  out <- match_opts_dims(opts)

  expect_equal(out[["color"]], 1)  # one red/red pair
  expect_equal(out[["size"]], 0)   # all sizes distinct
  expect_equal(out[["shape"]], 0)
  expect_false("number" %in% names(out))      # constant -> dropped
  expect_false("background" %in% names(out))
})

test_that("match_resp_dims() names dimensions the response stimuli share", {
  resp <- code_dims(c("sm-red-star", "lg-red-circle"))
  opts_dims <- c(size = 0, color = 1, shape = 0)  # as from match_opts_dims

  # the two response stimuli share only color, so that's the matched dimension
  expect_equal(match_resp_dims(resp, opts_dims), "color")
})

test_that("code_misses() returns option dimensions not covered by the response", {
  # response covers fewer than k distinct dimensions -> the uncovered ones
  expect_equal(code_misses(c("color", "size"), list("color"), k = 2), "size")
  # response covers exactly k dimensions, all present in options -> no misses
  expect_equal(code_misses(c("color"), list("color"), k = 1), character(0))
})
