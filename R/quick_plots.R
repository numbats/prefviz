#' Bar chart of preference distribution for one contest
#'
#' @description
#' Draws a bar chart showing how votes or preferences are distributed across
#' items (candidates, parties, options) in a single contest or round. Bars are
#' ordered from highest to lowest value.
#'
#' @param data A data frame in wide or long format. See Details.
#' @param items <[`tidy-select`][dplyr::dplyr_tidy_select]>
#'   Columns to plot. Interpretation depends on format:
#'   * **Wide**: A tidy-select expression identifying the item columns to pivot,
#'     e.g. `ALP:Other` or `-c(round, winner)`.
#'   * **Long**: A single column containing item names, e.g. `PartyAb`.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]>
#'   **Long format only.** Column containing the numeric values to plot (vote
#'   shares or counts). When `NULL` (default), wide format is assumed and this
#'   argument is ignored.
#' @param round_col Character. Name of the column used to
#'   identify rounds. Default `"round"`, matching [dop_irv()] output. Override
#'   when your data uses a different column name (e.g., `"CountNumber"`).
#' @param at_round Integer. The round number to display. Default is `1`.
#'
#' @return A ggplot object.
#' 
#' @details
#' `dop_bar()` accepts data in two formats, detected automatically via
#' `value_col`:
#'
#' **Wide format** (`value_col = NULL`, the default)
#'
#' One row per round, one column per item. This is the direct output of
#' [dop_irv()]:
#' ```
#' round | ALP  | LNP  | Other | winner
#'     1 | 0.40 | 0.35 | 0.25  | ALP
#'     2 | 0.52 | 0.48 | 0.00  | ALP
#' ```
#' Supply the item columns via `items` (e.g., `ALP:Other` or
#' `-c(round, winner)`) and select the round to display with `at_round`.
#' Use `round_col` if your round column is named something other than
#' `"round"` (e.g., `round_col = "CountNumber"`).
#'
#' **Long format** (`value_col` provided)
#'
#' One row per item, with the item name and its value in separate columns.
#' This is the format of [aecdop_2022] and similar raw electoral datasets:
#' ```
#' DivisionNm | CountNumber | PartyAb | CalculationValue
#' Adelaide   |           0 | ALP     |            0.40
#' Adelaide   |           0 | LNP     |            0.35
#' Adelaide   |           0 | Other   |            0.25
#' ```
#' Supply the item name column via `items`, the value column via
#' `value_col`, and the round to display via `at_round`. 
#' Use `round_col` if your round column is named something other than
#' `"round"` (e.g., `round_col = "CountNumber"`).
#'
#' @examples
#' library(ggplot2)
#'
#' # Wide format: output of dop_irv()
#' votes <- prefio::preferences(c("A > B > C", "B > A > C", "C > B > A",
#'                                 "A > B > C", "A > C > B"))
#' irv_result <- dop_irv(votes)
#' dop_bar(irv_result, items = -c(round, winner), at_round = 1)
#'
#' # Long format: pre-filter to desired contest, then plot
#' long_df <- aecdop_2022 |>
#'   dplyr::filter(
#'     CalculationType == "Preference Percent",
#'     CountNumber == 0,
#'     DivisionNm == "Adelaide"
#'   )
#' dop_bar(long_df, items = PartyAb, value_col = CalculationValue)
#'
#' @seealso [dop_irv()] to generate wide-format input from raw ballot data.
#'
#' @export
dop_bar <- function(data,
                    items,
                    value_col = NULL,
                    round_col = "round",
                    at_round  = 1) {

  if (rlang::quo_is_null(rlang::enquo(value_col))) {
    df <- data |>
      dplyr::filter(.data[[round_col]] == at_round) |>
      tidyr::pivot_longer({{ items }}, names_to = "item", values_to = "value")
  } else {
    df <- data |>
      dplyr::select(item = {{ items }}, value = {{ value_col }})
  }

  ggplot2::ggplot(df, ggplot2::aes(x = reorder(item, -value), y = value)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                       vjust = -0.5, size = 3.5) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x = "Item",
      y = "Value",
      title = paste0("Distribution of Preferences, Round ", at_round)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}

#' Heatmap of pairwise results
#'
#' @description
#' Plots a full N×N heatmap of pairwise results from a [pairwise_calculator()]
#' object. Each cell shows how the row item performed against the column item.
#' Color always encodes the TCP ratio (green = win, red = lose, white = 50/50).
#'
#' @param x A `pairwise` object returned by [pairwise_calculator()].
#' @param value `"tcp"` (default) or `"count"`. Controls the tile annotation:
#'   * `"tcp"`: shows the TCP percentage, e.g. `"56.8%"`.
#'   * `"count"`: shows the raw vote count, e.g. `"2841"`.
#'
#' @return A ggplot object.
#'
#'
#' @examples
#' library(prefio)
#'
#' prefs <- data.frame(
#'   A = c(1, 1, 1, 2, 2),
#'   B = c(2, 2, 3, 1, 3),
#'   C = c(3, 3, 2, 3, 1)
#' ) |>
#'   wide_preferences(col = vote, ranking_cols = A:C)
#'
#' result <- pairwise_calculator(prefs, preferences_col = vote)
#' pairwise_heatmap(result, value = "tcp")
#' pairwise_heatmap(result, value = "count")
#'
#' @seealso [pairwise_calculator()] to compute the input object.
#'
#' @export
pairwise_heatmap <- function(x, value = c("tcp", "count")) {
  stopifnot(inherits(x, "pairwise"))
  value <- match.arg(value)

  tcp <- x$two_candidate_preferred

  dir_a <- data.frame(
    item = tcp$item_a,
    opponent = tcp$item_b,
    tcp_val = tcp$tcp_a,
    label_val = if (value == "tcp") {
      sprintf("%.1f%%", tcp$tcp_a * 100)
    } else {
      as.character(tcp$wins_a)
    }
  )
  dir_b <- data.frame(
    item = tcp$item_b,
    opponent = tcp$item_a,
    tcp_val = tcp$tcp_b,
    label_val = if (value == "tcp") {
      sprintf("%.1f%%", tcp$tcp_b * 100)
    } else {
      as.character(tcp$wins_b)
    }
  )
  plot_data <- rbind(dir_a, dir_b)

  # White text on dark tiles (far from 0.5), dark grey on light tiles (near 0.5)
  plot_data$text_color <- ifelse(
    plot_data$tcp_val < 0.25 | plot_data$tcp_val > 0.75,
    "white", "grey20"
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = opponent, y = item, fill = tcp_val)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_val, color = text_color),
      size = 5
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_gradient2(
      low = "firebrick", mid = "white", high = "forestgreen",
      midpoint = 0.5, limits = c(0, 1),
      labels = scales::percent,
      name = "TCP ratio"
    ) +
    ggplot2::labs(
      title = "Pairwise head-to-head",
      x = "Opponent",
      y = "Item"
    ) +
    ggplot2::theme_minimal()
}
