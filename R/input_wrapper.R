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

#' Transform AEC distribution of preferences from long to wide format
#' 
#' @description
#' Transform AEC distribution of preferences from long to wide format, with optional scaling and normalization.
#' This function is useful for converting all distribution of preference data with similar format
#' into format ready for ternary plots. 
#' 
#' @param data A data frame containing preference or vote distribution data, with format similar to
#'   \link[AEC Distribution of Preferences]{https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv}
#' @param key_cols Columns that identify unique observations, e.g., DivisionNm, CountNumber
#' @param value_col Numeric and non-negative. Column containing the numeric values to aggregate, 
#'  e.g., CalculationValue, Votes. 
#' @param alternative_col Column name containing the alternative of the election, 
#'  e.g., Party, Candidate. This column will become column names in the output wide format.
#' @param normalize Logical. If \code{TRUE} (default), normalizes values within
#'   each group to sum to 1. If \code{FALSE}, returns raw aggregated values.
#' @param scale Numeric. If \code{normalize = FALSE}, divides all values by this
#'   scale factor. Default is 1 (no scaling).
#' @param fill_value Numeric. Value to use for missing combinations after
#'   pivoting. Default is 0.
#' @param winner_col Optional character string specifying a column that indicates
#'   the winner/elected party. If provided, this column will be joined back to
#'   the output based on key columns. Useful for preserving election outcome
#'   information. Default is \code{NULL}.
#' @param winner_identifier Optional character string specifying the value in
#'   \code{winner_col} that identifies winning candidates (e.g., "Y", "Elected").
#'   Only used if \code{winner_col} is specified. Default is "Y".
#' 
#' @return A data frame in wide format with:
#'   \itemize{
#'     \item Key columns identifying each observation
#'     \item Columns for each alternative containing aggregated/normalized values
#'     \item Winner column (if \code{winner_col} was specified)
#'   }
#' @examples
#' # Convert AEC 2025 Distribution of Preference data to wide format
#' data(aec_dop_2025)
#' 
#' # We are interested in the preferences of Labor, Coalition, Greens and Independent. 
#' # The rest of the parties are aggregated as Other.
#' aec_dop_2025 <- aec_dop_2025 |>
#'  filter(CalculationType == "Preference Percent") |> 
#'   mutate(PartyAb = case_when(
#'     !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other",
#'     PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
#'    TRUE ~ PartyAb))
#' 
#' transform_preference(
#'   data = aec_dop_2025,
#'   key_cols = DivisionNm, CountNumber,
#'   value_col = CalculationValue,
#'   alternative_col = Party,
#'   winner_col = Elected
#' )
#'@export

dop_transform <- function(data,
                          key_cols,
                          value_col,
                          alternative_col,
                          normalize = TRUE,
                          scale = 1,
                          fill_value = 0,
                          winner_col = NULL,
                          winner_identifier = "Y") {
  
  # Input validation
  stopifnot("'data' must be a data frame" = is.data.frame(data))
  
  # Capture and convert column selections
  key_cols_sel <- tidyselect::eval_select(
    rlang::enquo(key_cols), 
    data
  )
  key_cols_chr <- names(key_cols_sel)
  
  value_col_chr <- rlang::as_label(rlang::ensym(value_col))
  alternative_col_chr <- rlang::as_label(rlang::ensym(alternative_col))
  
  if (!is.null(rlang::enexpr(winner_col))) {
    winner_col_chr <- rlang::as_label(rlang::ensym(winner_col))
  } else {
    winner_col_chr <- NULL
  }
  
  # Check if columns exist
  required_cols <- c(key_cols_chr, value_col_chr, alternative_col_chr)
  if (!is.null(winner_col_chr)) {
    required_cols <- c(required_cols, winner_col_chr)
  }
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if value column is numeric
  if (!is.numeric(data[[value_col_chr]])) {
    stop("'value_col' (", value_col_chr, ") must be numeric")
  }
  
  # Check for negative values
  if (any(data[[value_col_chr]] < 0, na.rm = TRUE)) {
    stop("'value_col' contains negative values. Compositional data must be non-negative.")
  }
  
  # Prepare grouping columns
  group_cols <- c(key_cols_chr, alternative_col_chr)
  
  # Aggregate data
  df_agg <- data |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      aggregated_value = sum(.data[[value_col_chr]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot wider
  df_wide <- df_agg |>
    pivot_wider(
      id_cols = all_of(key_cols_chr),
      names_from = all_of(alternative_col_chr),
      values_from = aggregated_value,
      values_fill = fill_value
    )
  
  # Get alternative column names
  alternative_names <- setdiff(names(df_wide), key_cols_chr)
  
  # Apply normalization or scaling
  if (normalize) {
    df_wide <- df_wide |>
      mutate(
        row_total = rowSums(across(all_of(alternative_names)), na.rm = TRUE),
        across(
          all_of(alternative_names),
          ~ .x / row_total
        )
      ) |>
      select(-row_total)
    
    # Handle division by zero
    df_wide <- df_wide |>
      mutate(
        across(
          all_of(alternative_names),
          ~ if_else(is.nan(.x) | is.infinite(.x), 0, .x)
        )
      )
    
  } else if (scale != 1) {
    df_wide <- df_wide |>
      mutate(
        across(all_of(alternative_names), ~ .x / scale)
      )
  }
  
  # Join winner information if requested
  if (!is.null(winner_col_chr)) {
    winner_data <- data |>
      filter(.data[[winner_col_chr]] == winner_identifier) |>
      select(all_of(c(key_cols_chr, alternative_col_chr))) |>
      distinct() |>
      rename(Winner = all_of(alternative_col_chr))
    
    df_wide <- df_wide |>
      left_join(winner_data, by = key_cols_chr)
  }
  
  return(df_wide)
}
