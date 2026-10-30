# Two 2-match trials (four responses) from a real Western run, in the
# pre-recode trials schema. Second choices repeat the first pair in the
# opposite order, so they are not new matches.
sds_block <- function(prompt_first, prompt_later) {
  tibble::tibble(
    dataset = "pilot_western_ca_main",
    run_id = "B50Y0PwxyDht9Tj0QVsA",
    trial_id = c("fHeIQ1cNEdya2twxhrZl", "yh2aYmDEqsp0uLnkQ4oi",
                 "QtleYCnuozHT2RlUip1U", "O8CyM04mD3umvN5uQsjv"),
    item_task = "sds",
    item_group = "2match",
    item = c("choice1", "choice2", "choice1", "choice2"),
    item_original = c(prompt_first, prompt_later, prompt_first, prompt_later),
    item_uid = c("sds_2match_choice1", "sds_2match_choice2",
                 "sds_2match_choice1", "sds_2match_choice2"),
    answer = NA_character_,
    response = c("{'0': 'lg-red-triangle', '1': 'sm-red-circle'}",
                 "{'0': 'sm-red-circle', '1': 'lg-red-triangle'}",
                 "{'0': 'med-blue-triangle', '1': 'sm-yellow-triangle'}",
                 "{'0': 'sm-yellow-triangle', '1': 'med-blue-triangle'}"),
    distractors = c(rep("{'0': 'sm-yellow-square', '1': 'sm-red-circle', '2': 'lg-red-triangle'}", 2),
                    rep("{'0': 'med-blue-triangle', '1': 'sm-yellow-triangle', '2': 'med-red-circle'}", 2)),
    correct = c(TRUE, FALSE, TRUE, FALSE),
    original_correct = c(TRUE, FALSE, TRUE, FALSE),
    chance = 0,
    timestamp = as.POSIXct("2026-08-08 09:16:56", tz = "UTC") + c(0, 6.8, 12, 19))
}

test_that("recode_sds() keeps match trials with the original prompt wording", {
  out <- recode_sds(sds_block(
    "Touch two pictures that are the same in one way to make a match. ",
    "Touch two pictures that are the same in a different way to make a match. "))
  expect_equal(nrow(out), 4)
  expect_setequal(out$item_uid, c("sds_2match_choice1_0m0n", "sds_2match_choice2_1m0n"))
  expect_equal(out$correct[order(out$timestamp)], c(TRUE, FALSE, TRUE, FALSE))
})

test_that("recode_sds() keeps match trials with the 2026-08 prompt wording", {
  # from 2026-08 the later-choice prompt says "in a new way" instead of "in a
  # different way"; trial boundaries must still be found
  old <- recode_sds(sds_block(
    "Touch two pictures that are the same in one way to make a match. ",
    "Touch two pictures that are the same in a different way to make a match. "))
  new <- recode_sds(sds_block(
    "Choose two cards that are the same in one way to make a match.",
    "Choose two cards that are the same in a new way to make a match."))
  expect_equal(nrow(new), 4)
  expect_equal(new[order(new$timestamp), c("trial_id", "item_uid", "correct")],
               old[order(old$timestamp), c("trial_id", "item_uid", "correct")])
})
