# Unit tests for the data-shaping helpers that turn long trial data into the
# wide item-by-run matrices mirt expects. These run on every IRT scoring call.

test_that("dedupe_items() numbers repeated items per run", {
  df <- tibble(
    run_id = c("r1", "r1", "r1", "r2"),
    item_uid = c("a", "a", "b", "a")
  )

  out <- dedupe_items(df)

  expect_equal(out$instance, c(1, 2, 1, 1))
  expect_equal(as.character(out$item_inst), c("a-1", "a-2", "b-1", "a-1"))
})

test_that("dedupe_items() matches a grouped seq_along() numbering", {
  # interleaved repeats, NA item_uids, data frame and grouped input
  df <- data.frame(
    run_id = c("r2", "r1", "r1", "r2", "r1", "r1", "r2", "r1"),
    item_uid = c("a", "b", "a", "a", NA, "b", NA, NA),
    other = 1:8
  )
  grouped_seq <- \(d) d |>
    group_by(run_id, item_uid) |>
    mutate(instance = seq_along(item_uid)) |>
    ungroup() |>
    mutate(item_inst = glue::glue("{item_uid}-{instance}"))

  expect_identical(dedupe_items(df), grouped_seq(df))
  expect_identical(dedupe_items(group_by(df, other)), grouped_seq(group_by(df, other)))
  expect_identical(dedupe_items(df[0, ]), grouped_seq(df[0, ]))
})

test_that("to_mirt_shape() pivots to a run-by-item matrix with run_id rownames", {
  df <- tibble(
    run_id = c("r1", "r1", "r2"),
    item_inst = c("a-1", "b-1", "a-1"),
    correct = c(TRUE, FALSE, TRUE)
  )

  out <- to_mirt_shape(df)

  expect_equal(rownames(out), c("r1", "r2"))
  expect_setequal(colnames(out), c("a-1", "b-1"))
  expect_equal(out["r1", "a-1"], 1)
  expect_equal(out["r1", "b-1"], 0)
  expect_true(is.na(out["r2", "b-1"]))  # r2 never saw b-1
})

test_that("to_mirt_shape_grouped() keeps the group column", {
  df <- tibble(
    run_id = c("r1", "r2"),
    group = c("g1", "g2"),
    item_inst = c("a-1", "a-1"),
    correct = c(TRUE, FALSE)
  )

  out <- to_mirt_shape_grouped(df)

  expect_equal(out$group, c("g1", "g2"))
  expect_equal(out[["a-1"]], c(1, 0))
})

test_that("remove_no_var_items() drops items with no response variance", {
  df <- tibble(
    item_inst = c("novar", "novar", "var", "var"),
    correct = c(TRUE, TRUE, TRUE, FALSE)
  )

  out <- remove_no_var_items(df)

  expect_setequal(unique(out$item_inst), "var")
})

test_that("remove_nonshared_items() keeps only items present in all groups", {
  df <- tibble(
    item_uid = c("shared", "shared", "only_g1"),
    group = c("g1", "g2", "g1")
  )

  out <- remove_nonshared_items(df)

  expect_setequal(unique(out$item_uid), "shared")
})
