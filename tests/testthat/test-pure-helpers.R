# Unit tests for small pure helpers used across the package.

test_that("compute_age() returns age in years from birth month/year", {
  # born June 2018 (assumed day 15), tested exactly six years later
  expect_equal(compute_age(6, 2018, as.Date("2024-06-15")), 6.0)
  # half a year later than birth midpoint
  expect_equal(compute_age(1, 2020, as.Date("2020-07-15")), 0.5)
  # missing inputs propagate to NA (parsed quietly)
  expect_true(is.na(compute_age(NA, 2018, as.Date("2024-06-15"))))
})

test_that("reverse_value() reverses within an ordered scale", {
  scale <- 1:5
  expect_equal(reverse_value(2, scale), 4)
  expect_equal(reverse_value(1, scale), 5)
  expect_equal(reverse_value(3, scale), 3)
  # value not on the scale, or a scale with NA -> NA
  expect_true(is.na(reverse_value(9, scale)))
  expect_true(is.na(reverse_value(2, c(1, NA, 3))))
})

test_that("is_only_source() matches comparing the source list to a string", {
  sources <- list("item_uid_roar", c("item_uid_roar", "item_uid"),
                  c("item_uid", "item_uid_roar"), "item_uid", character(0),
                  NULL, NA_character_)

  expect_identical(is_only_source(sources, "item_uid_roar"),
                   suppressWarnings(sources == "item_uid_roar"))
  expect_identical(is_only_source(list(), "item_uid_roar"), logical(0))
})
