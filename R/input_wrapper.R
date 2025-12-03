#' Get full distribution of preferences in each instant runoff voting round as percentage
#'
#' @description
#' Compute the preference in each round of instant runoff voting from input data, 
#' transforming the results into a tidy format for visualization. Each row represents
#' one round, with columns for each candidate's preference percentage and the election winner.
#'
#' @param x Input data. Accepts the same formats as `prefio::pref_irv()`:
#'   * A preference vector where each element represents one ballot
#'   * A data frame with a column for preference
#' @param value_type Character string specifying the output format. Either:
#'   * `"percentage"` (default): Returns vote shares as proportions (0-1)
#'   * `"count"`: Returns raw vote counts
#' @param ... Additional arguments passed to `prefio::pref_irv()`, including:
#'   * `preferences_col`: Column name containing preference orderings
#'   * `frequency_col`: Column name containing vote frequencies
#'
#' @return A tibble with the following structure:
#'   * `round`: Integer, the round number (1 to n)
#'   * One column per candidate: Numeric, the percentage of votes (0-1) that 
#'     candidate received in that round. NA values are replaced with 0 for 
#'     eliminated candidates.
#'   * `winner`: Character, the name of the eventual IRV winner (same for all rows)
#'
#' @examples
#' \dontrun{
#' # Example 1: From preference vector
#' votes <- c("A > B > C", "B > A > C", "C > B > A", "A > B > C")
#' percent_df <- dop_irv(votes, value_type = "count")
#' 
#' # Example 2: From data frame with custom column names
#' vote_data <- tibble(
#'   prefs = c("A > B > C", "B > C > A", "C > A > B"),
#'   counts = c(100, 75, 25)
#' )
#' percent_df <- dop_irv(vote_data, value_type = "percentage",
#'                       preferences_col = prefs,
#'                       frequency_col = counts)
#' }
#'
#' @export
dop_irv <- function(x, value_type = c("percentage", "count"), ...) {
  
  value_type <- match.arg(value_type)
  
  # Implement IRV to get round results
  irv_result <- pref_irv(x, ...)
  
  percent_df <- tibble::tibble()
  
  # Process each round
  for (i in seq_along(irv_result$rounds)) {
    
    if (value_type == "percentage") {
      # Convert to percentages
      round_pref <- irv_result$rounds[[i]] |>
        dplyr::mutate(
          pref_value = value / sum(value),
          round = i
        ) |>
        dplyr::select(-value) |>
        tidyr::pivot_wider(
          names_from = candidate, 
          values_from = pref_value
        )
    } else {
      # Keep as counts
      round_pref <- irv_result$rounds[[i]] |>
        dplyr::mutate(round = i) |>
        tidyr::pivot_wider(
          names_from = candidate, 
          values_from = value
        )
    }
  
    percent_df <- dplyr::bind_rows(percent_df, round_pref) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0)))
  }

  # Add the final winner
  percent_df <- percent_df |>
    dplyr::mutate(winner = irv_result$winner)
  
  return(percent_df)
}