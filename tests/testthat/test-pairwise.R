library(prefio)

# Shared fixtures ------------------------------------------------------------

winner_wide <- data.frame(
  A = c(1, 1, 1, 2, 2),
  B = c(2, 2, 3, 1, 3),
  C = c(3, 3, 2, 3, 1)
) |>
  wide_preferences(col = vote, ranking_cols = A:C)

cycle_wide <- data.frame(
  A = c(1, 3, 2),
  B = c(2, 1, 3),
  C = c(3, 2, 1)
) |>
  wide_preferences(col = vote, ranking_cols = A:C)

result_winner <- pairwise_calculator(winner_wide, preferences_col = vote)
result_cycle  <- pairwise_calculator(cycle_wide,  preferences_col = vote)

# 1. Return class ------------------------------------------------------------

test_that("pairwise_calculator returns a pairwise object", {
  expect_s3_class(result_winner, "pairwise")
  expect_s3_class(result_cycle,  "pairwise")
})

# 7. tcp_a + tcp_b == 1 for every pair ---------------------------------------

test_that("tcp_a and tcp_b sum to 1 for every pair", {
  tcp_sums_winner <- with(result_winner$two_candidate_preferred, tcp_a + tcp_b)
  expect_equal(tcp_sums_winner, rep(1, nrow(result_winner$two_candidate_preferred)))

  tcp_sums_cycle <- with(result_cycle$two_candidate_preferred, tcp_a + tcp_b)
  expect_equal(tcp_sums_cycle, rep(1, nrow(result_cycle$two_candidate_preferred)))
})

# 8. wins_a + wins_b == total ------------------------------------------------

test_that("wins_a + wins_b equals total for every pair", {
  tcp_w <- result_winner$two_candidate_preferred
  expect_equal(tcp_w$wins_a + tcp_w$wins_b, tcp_w$total)

  tcp_c <- result_cycle$two_candidate_preferred
  expect_equal(tcp_c$wins_a + tcp_c$wins_b, tcp_c$total)
})

# 13 & 14. Condorcet winner and loser (winner_wide) --------------------------

test_that("winner_wide produces correct Condorcet winner and loser", {
  expect_equal(result_winner$condorcet_winner, "A")
  expect_equal(result_winner$condorcet_loser,  "C")
})

# 15 & 16. No Condorcet winner or loser (cycle_wide) -------------------------

test_that("cycle_wide produces no Condorcet winner or loser", {
  expect_true(is.na(result_cycle$condorcet_winner))
  expect_true(is.na(result_cycle$condorcet_loser))
})

# 17. h2h_winner is never NA (cycle_wide) ------------------------------------

test_that("h2h_winner is non-NA for every pair even in a cycle", {
  expect_true(all(!is.na(result_cycle$two_candidate_preferred$h2h_winner)))
})

# 24. Bare preferences object accepted ---------------------------------------

test_that("pairwise_calculator accepts a bare preferences object", {
  bare_prefs <- winner_wide$vote
  expect_s3_class(bare_prefs, "preferences")
  expect_no_error(pairwise_calculator(bare_prefs))
})

# 32. pairwise_heatmap rejects non-pairwise input ----------------------------

test_that("pairwise_heatmap errors when input is not a pairwise object", {
  expect_error(pairwise_heatmap(list()),       class = "simpleError")
  expect_error(pairwise_heatmap(winner_wide),  class = "simpleError")
  expect_error(pairwise_heatmap("not pairwise"), class = "simpleError")
})

# Extra. data frame with non-preferences column raises an error --------------

test_that("data frame with non-preferences column raises an error", {
  bad_df <- data.frame(x = 1:3, y = 4:6)
  expect_error(pairwise_calculator(bad_df, preferences_col = x))
})
