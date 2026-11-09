# Tests for fetch_cached_table(), using a stand-in for a Redivis dataset object
# that counts table downloads.

fake_dataset <- function(version = "v1_0", released = TRUE) {
  ds <- new.env()
  ds$downloads <- 0
  ds$scoped_reference <- paste0("test_metadata:abcd:", version)
  ds$properties <- list(version = list(isReleased = released))
  ds$get <- function() ds
  ds$table <- function(table_name) {
    list(to_tibble = function() {
      ds$downloads <- ds$downloads + 1
      tibble::tibble(table = table_name, version = version, x = 1:3)
    })
  }
  ds
}

test_that("fetch_cached_table() downloads without a cache directory set", {
  withr::local_options(levantemodels.cache_dir = NULL)
  ds <- fake_dataset()

  fetch_cached_table(ds, "tbl:1234")
  fetch_cached_table(ds, "tbl:1234")
  expect_equal(ds$downloads, 2)
})

test_that("fetch_cached_table() downloads each table version once", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(levantemodels.cache_dir = cache_dir)
  ds <- fake_dataset()

  first <- fetch_cached_table(ds, "tbl:1234")
  second <- suppressMessages(fetch_cached_table(ds, "tbl:1234"))
  expect_equal(ds$downloads, 1)
  expect_identical(first, second)

  # other tables and other versions are cached separately
  fetch_cached_table(ds, "other:5678")
  expect_equal(ds$downloads, 2)
  ds_new <- fake_dataset(version = "v2_0")
  new_version <- fetch_cached_table(ds_new, "tbl:1234")
  expect_equal(ds_new$downloads, 1)
  expect_equal(unique(new_version$version), "v2_0")
  expect_length(list.files(cache_dir), 3)
})

test_that("fetch_cached_table() doesn't cache unreleased versions", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(levantemodels.cache_dir = cache_dir)
  ds <- fake_dataset(released = FALSE)

  fetch_cached_table(ds, "tbl:1234")
  fetch_cached_table(ds, "tbl:1234")
  expect_equal(ds$downloads, 2)
  expect_length(list.files(cache_dir), 0)
})
