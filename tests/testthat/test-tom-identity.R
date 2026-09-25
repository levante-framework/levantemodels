# Unit tests for the ToM item-identity fixes and the ToM guessing floor
# (2026-09 Stories audit). Inputs are hand-built trials shaped like
# add_item_ids() output; the rules under test are the package's own table.

tom_trials <- \(item, answer, item_uid, ts, run_id = "r1", task_id = "theory-of-mind") {
  tibble(trial_id = paste0("t", seq_along(ts)), run_id = run_id, task_id = task_id,
         item = item, answer = answer, item_uid = item_uid,
         server_timestamp = as.POSIXct(ts, tz = "UTC"))
}

test_that("fix_tom_item_uids() assigns identical-content questions by their order in the run", {
  # item-bank epoch: q3/q4 of story 2 were logged as fb_1/fb_2 (should be fb_2/fb_3);
  # rows are out of time order to show that order comes from the timestamp
  ib <- tom_trials(item = c("2f_new2", "2f_new2"), answer = "no",
                   item_uid = c("tom_moral_reasoning_false_belief_2", "tom_moral_reasoning_false_belief_1"),
                   ts = c("2025-11-04 10:05:00", "2025-11-04 10:04:00"))
  expect_equal(fix_tom_item_uids(ib)$item_uid,
               c("tom_moral_reasoning_false_belief_3", "tom_moral_reasoning_false_belief_2"))

  # first Bogota corpus: three identical 4c questions retro-mapped to fb_1
  v0 <- tom_trials(item = "4c", answer = "no", item_uid = rep("tom_deception_false_belief_1", 3),
                   ts = c("2024-05-07 10:00:00", "2024-05-07 10:01:00", "2024-05-07 10:02:00"))
  expect_equal(fix_tom_item_uids(v0)$item_uid,
               paste0("tom_deception_false_belief_", 1:3))

  # order is counted within each run (r2's only trial is its first, whatever UID it logged)
  two_runs <- tom_trials(item = "8g", answer = "no",
                         item_uid = paste0("tom_moral_reasoning_false_belief_", c(2, 2, 3)),
                         ts = c("2025-03-25 10:00:00", "2025-03-25 10:01:00", "2025-03-25 11:00:00"),
                         run_id = c("r1", "r1", "r2"))
  expect_equal(fix_tom_item_uids(two_runs)$item_uid,
               paste0("tom_moral_reasoning_false_belief_", c(2, 3, 2)))
})

test_that("fix_tom_item_uids() restores trials with an orphan or missing item UID", {
  # tom_moral_reasoning_false_belief is not in corpus_items, so this trial is dropped today
  orphan <- tom_trials(item = "2d", answer = "shelf", item_uid = "tom_moral_reasoning_false_belief",
                       ts = "2025-06-01 10:00:00")
  expect_equal(fix_tom_item_uids(orphan)$item_uid, "tom_moral_reasoning_false_belief_1")

  unmapped <- tom_trials(item = c("6c", "6d"), answer = "no", item_uid = NA_character_,
                         ts = c("2024-06-01 10:00:00", "2024-06-01 10:01:00"))
  expect_equal(fix_tom_item_uids(unmapped)$item_uid,
               c("tom_second_order_false_belief_1", "tom_second_order_false_belief_3"))
})

test_that("fix_tom_item_uids() leaves other epochs and other tasks untouched", {
  # correctly logged q3/q4 after the item-bank epoch keep their UIDs
  later <- tom_trials(item = c("2f_new2", "2f_new2"), answer = "no",
                      item_uid = c("tom_moral_reasoning_false_belief_2", "tom_moral_reasoning_false_belief_3"),
                      ts = c("2026-08-26 10:00:00", "2026-08-26 10:01:00"))
  expect_equal(fix_tom_item_uids(later), later)

  # outside the epochs, a rule's content does not override the logged UID
  lone <- tom_trials(item = "2f_new2", answer = "no", item_uid = "tom_moral_reasoning_false_belief_3",
                     ts = "2026-08-26 10:01:00")
  expect_equal(fix_tom_item_uids(lone), lone)

  # a non-ToM trial with a matching item code and answer inside an epoch
  other <- tom_trials(item = "2d", answer = "shelf", item_uid = "not_tom",
                      ts = "2025-06-01 10:00:00", task_id = "egma-math")
  expect_equal(fix_tom_item_uids(other), other)
})

test_that("add_item_ids() applies the ToM item UID fixes", {
  local_mocked_bindings(
    fetch_item_mapping_trial = \(...) tibble(item_uid = "x_uid", trials = '["t_other"]'),
    fetch_item_mapping_fields = \(...) tibble(item_uid = "y_uid", corpus_trial_type = "a",
                                              item = "b", answer = "c", distractors = "d"),
    fetch_item_mapping_id = \(...) tibble(item_uid = "z_uid", item_id = "id_other")
  )
  trials <- tom_trials(item = "2d", answer = "shelf", item_uid = "tom_moral_reasoning_false_belief",
                       ts = "2025-06-01 10:00:00") |>
    mutate(item_id = NA_character_, corpus_trial_type = "false_belief_question",
           distractors = "{'0': 'coats'}")

  out <- add_item_ids(trials)

  expect_equal(nrow(out), 1)
  expect_equal(out$item_uid, "tom_moral_reasoning_false_belief_1")
})

test_that("add_item_metadata() sets one ToM chance per story question", {
  local_mocked_bindings(
    fetch_corpus_items = \(...) tibble(
      item_uid = c(paste0("tom_second_order_false_belief_", 1:3), "math_x"),
      item_task = c("tom", "tom", "tom", "math"), group = "g", entry = "e",
      chance = c(0.5, 0.33, 0.33, 0.25))
  )
  trials <- tibble(
    trial_id = paste0("t", 1:6),
    task_id = c(rep("theory-of-mind", 5), "egma-math"),
    item_uid = c(paste0("tom_second_order_false_belief_", c(2, 2, 3, 1, 1)), "math_x"),
    item_uid_source = list("item_uid"),
    item = c("6c", "18d", "6d_new2", "18c", "18c", "1+1"),
    distractors = c("{'0': 'shirts', '1': 'socks'}", "{'0': 'yes'}", "{'0': 'yes'}",
                    "{'0': 'crayons'}", "{'0': 'crayons', '1': 'storybook'}", "{'0': 3}")
  )

  out <- add_item_metadata(trials)

  # yes/no questions get .5 where their generic UID is 3AFC (.33) in another story;
  # a trial logged with an extra option keeps its question's chance; non-ToM keeps corpus chance
  expect_equal(out$chance, c(0.33, 0.5, 0.5, 0.5, 0.5, 0.25))
})
