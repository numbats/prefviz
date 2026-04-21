#' Compute pairwise results from ranked preference data
#'
#' @description
#' Computes all pairwise comparisons from a set of ranked
#' preferences using [prefio::adjacency()]. For each pair of items, reports
#' the number of voters who preferred each item and the Two-Candidate Preferred
#' (TCP) ratio. Also identifies the Condorcet winner and loser where they exist.
#'
#' @param x A `preferences` object, or a data frame / tibble containing a
#'   `preferences`-typed column. 
#' @param preferences_col <[`tidy-select`][dplyr::dplyr_tidy_select]>
#'   When `x` is a data frame, the column containing the preferences.
#'   Passed directly to [prefio::adjacency()].
#' @param frequency_col <[`tidy-select`][dplyr::dplyr_tidy_select]>
#'   Optional column containing ballot frequencies.
#'   Passed directly to [prefio::adjacency()].
#'
#' @return An S3 object of class `"pairwise"` with four components:
#'   \describe{
#'     \item{`pairwise_matrix`}{N×N integer matrix from [prefio::adjacency()].}
#'     \item{`two_candidate_preferred`}{Tibble with one row per pair,
#'       columns: `item_a`, `item_b`, `wins_a`, `wins_b`, `total`, `tcp_a`,
#'       `tcp_b`, `h2h_winner`.}
#'     \item{`condorcet_winner`}{Name of the Condorcet winner, or `NA` if none.}
#'     \item{`condorcet_loser`}{Name of the Condorcet loser, or `NA` if none.}
#'   }
#'
#' @details
#' **TCP ratio** (`tcp_a`, `tcp_b`) is computed as `wins / (wins_a + wins_b)`.
#' The denominator is the number of voters who expressed a preference between
#' the specific pair, not the total number of voters. This correctly handles
#' partial rankings where some voters did not rank all items.
#'
#' **Condorcet winner**: the item that beats every other item head-to-head
#' (tcp > 0.5 in all pairs it appears in). At most one can exist.
#'
#' **Condorcet loser**: the item that loses to every other item head-to-head
#' (tcp < 0.5 in all pairs it appears in). At most one can exist. Neither
#' winner nor loser may exist when preference cycles are present.
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
#' print(result)
#'
#' @seealso [pairwise_heatmap()] to visualise the results.
#'
#' @export
pairwise_calculator <- function(x, preferences_col = NULL, frequency_col = NULL) {

  if (inherits(x, "preferences")) {
    x <- x
  } else if (is.data.frame(x)) {
    x <- tibble::as_tibble(x)
  } else {
    stop(
      "`x` must be a `preferences` object or a data frame with a ",
      "preferences-typed column."
    )
  }

  adj <- prefio::adjacency(
    x,
    preferences_col = {{ preferences_col }},
    frequency_col = {{ frequency_col }}
  )
  items <- rownames(adj)

  tcp <- pairwise_build_tcp(adj)
  winner <- condorcet_find(tcp, items, win = TRUE)
  loser <- condorcet_find(tcp, items, win = FALSE)

  structure(
    list(
      pairwise_matrix = adj,
      two_candidate_preferred = tcp,
      condorcet_winner = winner,
      condorcet_loser = loser
    ),
    class = "pairwise"
  )
}

#' Print a pairwise object
#'
#' @param x A `pairwise` object returned by [pairwise_calculator()].
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#' 
#' @keywords internal
#' @export
print.pairwise <- function(x, ...) {
  n_items <- nrow(x$pairwise_matrix)
  cat(sprintf("Pairwise analysis (%d items)\n\n", n_items))

  cat("Head-to-head results:\n")
  display <- x$two_candidate_preferred
  # Format as percentages for display only; underlying tibble keeps raw 0-1 values
  display$tcp_a <- sprintf("%.1f%%", display$tcp_a * 100)
  display$tcp_b <- sprintf("%.1f%%", display$tcp_b * 100)
  print(as.data.frame(display), row.names = FALSE)

  cat("\n")
  if (!is.na(x$condorcet_winner)) {
    cat(sprintf("Condorcet winner: %s\n", x$condorcet_winner))
  } else {
    cat("No Condorcet winner -- preference cycles exist.\n")
  }

  if (!is.na(x$condorcet_loser)) {
    cat(sprintf("Condorcet loser:  %s\n", x$condorcet_loser))
  } else {
    cat("No Condorcet loser.\n")
  }

  invisible(x)
}

#' Build TCP tibble from adjacency matrix
#'
#' Converts the raw N×N adjacency matrix from [prefio::adjacency()] into a
#' tidy tibble with one row per unordered pair \{A, B\}.
#'
#' @param adj N×N integer matrix from [prefio::adjacency()].
#'
#' @return A tibble with columns `item_a`, `item_b`, `wins_a`, `wins_b`,
#'   `total`, `tcp_a`, `tcp_b`, `h2h_winner`.
#'
#' @details
#' Matrix indexing note: [prefio::wide_preferences()] treats ranking values as
#' descending (higher value = more preferred), which reverses the usual
#' adjacency convention. As a result `adj[a, b]` counts b's wins over a, so
#' `wins_a` is read from `adj[b, a]`.
#'
#' @keywords internal
pairwise_build_tcp <- function(adj) {
  items <- rownames(adj)
  n_items <- length(items)

  # Pre-allocate to exact size — faster than growing with append()
  pairs <- vector("list", n_items * (n_items - 1L) / 2L)
  k <- 0L

  for (i in seq_len(n_items - 1L)) {
    for (j in seq(i + 1L, n_items)) {
      a <- items[i]
      b <- items[j]

      wins_a <- adj[b, a]
      wins_b <- adj[a, b]
      total <- wins_a + wins_b

      # NA when no voter expressed a preference between this specific pair
      tcp_a <- if (total > 0L) { wins_a / total } else { NA_real_ }
      tcp_b <- if (total > 0L) { wins_b / total } else { NA_real_ }

      k <- k + 1L
      pairs[[k]] <- list(
        item_a = a,
        item_b = b,
        wins_a = wins_a,
        wins_b = wins_b,
        total = total,
        tcp_a = tcp_a,
        tcp_b = tcp_b,
        h2h_winner = if (is.na(tcp_a)) {
          "tie"
        } else if (tcp_a > 0.5) {
          a
        } else if (tcp_b > 0.5) {
          b
        } else {
          "tie"
        }
      )
    }
  }

  dplyr::bind_rows(pairs)
}

#' Find the Condorcet winner or loser
#'
#' Scans the TCP tibble to identify the item that beats (or loses to) every
#' other item head-to-head.
#'
#' @param tcp Tibble returned by [pairwise_build_tcp()].
#' @param items Character vector of all item names.
#' @param win Logical. `TRUE` to find the Condorcet winner (tcp > 0.5 in every
#'   pair); `FALSE` to find the Condorcet loser (tcp < 0.5 in every pair).
#'
#' @return The item name as a character string, or `NA_character_` if none
#'   exists (e.g. when preference cycles are present).
#'
#' @keywords internal
condorcet_find <- function(tcp, items, win = TRUE) {
  result <- NA_character_

  for (item in items) {
    item_rows <- tcp[tcp$item_a == item | tcp$item_b == item, ]
    tcps <- ifelse(item_rows$item_a == item, item_rows$tcp_a, item_rows$tcp_b)

    # NAs (no shared voters for some pair) disqualify the item
    found <- if (win) {
      all(!is.na(tcps)) && all(tcps > 0.5)
    } else {
      all(!is.na(tcps)) && all(tcps < 0.5)
    }

    if (found) {
      result <- item
      break
    }
  }

  result
}
