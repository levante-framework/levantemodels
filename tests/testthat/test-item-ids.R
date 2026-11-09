# Unit tests for item UID fixes in add_item_ids(), with mocked item maps.

local_no_item_maps <- \(env = parent.frame()) {
  local_mocked_bindings(
    fetch_item_mapping_trial = \(...) tibble(item_uid = "x_uid", trials = '["t_other"]'),
    fetch_item_mapping_fields = \(...) tibble(item_uid = "y_uid", corpus_trial_type = "a",
                                              item = "b", answer = "c", distractors = "d"),
    fetch_item_mapping_id = \(...) tibble(item_uid = "z_uid", item_id = "id_other"),
    .env = env
  )
}

test_that("add_item_ids() maps downward extension matrix UIDs to their corpus UIDs", {
  local_no_item_maps()
  trials <- tibble(trial_id = paste0("t", 1:3), run_id = "r1", task_id = "matrix-reasoning",
                   item = c("downex_item_3", "downex_item_12", "mat5"),
                   item_uid = c("matrix_set0_md_mat3", "matrix_set0_md_mat12", "matrix_set1_md_mat5"),
                   item_id = NA_character_, corpus_trial_type = "Matrix Reasoning",
                   answer = "a", distractors = "{}",
                   server_timestamp = as.POSIXct("2026-06-01 10:00:00", tz = "UTC"))

  out <- add_item_ids(trials)

  # other matrix sets keep their UIDs
  expect_equal(out$item_uid, c("matrix_downex_item3", "matrix_downex_item12", "matrix_set1_md_mat5"))
})
