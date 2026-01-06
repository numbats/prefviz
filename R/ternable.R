#' Create a ternable object
#' 
#' @description
#' Creates a ternable object, which contains observation coordinates, simplex vertices, and edges
#' necessary for building a ternary plot in both 2 and higher dimensions.
#' 
#' @param data A data frame containing the item (alternative) columns used to construct the ternary plot.
#' @param items <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns representing the 
#'   items to be plotted as vertices of the simplex. Default is [everything()],
#'   which selects all columns. Must select at least 3 columns. All columns must be
#'   non-negative and sum to 1. 
#' @param ... Additional arguments (currently unused, reserved for future extensions).
#' 
#' @return A ternable object (S3 class) containing:
#' \itemize{
#'   \item{data}{The validated and normalized data frame}
#'   \item{ternary_coord}{Transformed coordinates for all observations}
#'   \item{simplex_vertices}{Vertex coordinates and labels for the simplex}
#'   \item{simplex_edges}{Edge connections for drawing the simplex boundary}
#'   \item{vertex_labels}{Labels of the vertices, same as names of the selected item columns}
#' }
#'
#' @examples
#' 
#' ### Create a ternable object from the 2025 Australian election's Distribution of Preferences
#' 
#' # Load and transform the dataset
#' df <- aecdop_2025 |> 
#'  filter(CalculationType == "Preference Percent") |>
#'  mutate(Party = case_when(
#'    # all parties not in the main parties are grouped into "Other"
#'    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other", 
#'    # group all parties in the Coalition into "LNP"
#'    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
#'    TRUE ~ PartyAb
#'  ))
#' 
#' transform_df <- dop_transform(
#'  data = aecdop_2025,
#'  key_cols = c(DivisionNm, CountNumber),
#'  value_col = CalculationValue,
#'  item_col = Party,
#'  winner_col = Elected,
#'  winner_identifier = "Y")
#' head(transform_df)
#' 
#' # Create the ternable object
#' tern <- ternable(transform_df, ALP:Other)
#' tern
#'
#' @export
ternable <- function(data, items = everything(), ...) {
  stopifnot(is.data.frame(data))

  item_col_ind <- tidyselect::eval_select(
      rlang::enquo(items), 
      data)
  item_col_chr <- colnames(data)[item_col_ind]

  validate_df <- validate_ternable(data, item_col_chr)

  new_ternable(validate_df, item_col_chr)
}

#' Validate input for ternable
#' @description
#' Internal validation function that checks compositional data requirements
#' and normalizes if necessary.
#'
#' @param data A data frame
#' @param item_col_chr Character vector of item column names
#'
#' @return The validated (and possibly normalized) data frame, invisibly
#'
#' @keywords internal
#' @noRd
validate_ternable <- function(data, item_col_chr) {
  alt_data <- data[, item_col_chr, drop = FALSE]

  # At least 3 items
  if (ncol(alt_data) < 3) {
    stop(
      "At least 3 items are required.",
      call. = FALSE
    )
  }

  # All items are numeric
  if (!all(sapply(alt_data, is.numeric))) {
    stop(
      "All item columns must be numeric.",
      call. = FALSE
    )
  }
  
  # No negative values allowed
  if (any(alt_data < 0, na.rm = TRUE)) {
    stop(
      "Item values cannot be negative.",
      call. = FALSE
    )
  }

  # Normalize if rows don't sum to 1
  row_sums <- rowSums(alt_data, na.rm = TRUE)
  tolerance <- 1e-8
  
  if (!all(abs(row_sums - 1) < tolerance)) {
    warning(
      "Not all rows sum to 1. Normalizing items automatically.",
      call. = FALSE
    )
    data[, item_col_chr] <- alt_data / row_sums
  }

  invisible(data)
}

#' Low-level constructor for ternable objects
#'
#' @description
#' Constructor that builds the ternable object after validation. 
#' Users should use [ternable()] instead.
#'
#' @param data A validated data frame
#' @param item_col_chr Character vector of item column names
#' @param ... Additional arguments (unused for now)
#'
#' @return A ternable object
#'
#' @keywords internal
#' @noRd
new_ternable <- function(data, item_col_chr, ...) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(item_col_chr))

  # Get ternary coordinates of the data
  cart_df <- helmert_transform(data, items = item_col_chr)

  # Define the simplex
  simp <- geozoo::simplex(p = length(item_col_chr) - 1)
  simp_points <- data.frame(simp$points)
  colnames(simp_points) <- paste0("x", 1:ncol(simp_points))

  # Define the vertex labels
  simp_points$labels <- item_col_chr

  structure(
    list(
      data = data, # validated & normalized data
      ternary_coord = cart_df,
      simplex_vertices = simp_points,
      simplex_edges = as.matrix(simp$edges),
      vertex_labels = item_col_chr
    ),
    class = "ternable"
  )
}

#' Print method for ternable objects
#'
#' @param x A ternable object
#' @param ... Additional arguments passed to print methods
#'
#' @return The object, invisibly
#'
#' @export
print.ternable <- function(x, ...) {
  cat("Ternable object\n")
  cat("----------------\n")
  cat("Items:", paste(x$vertex_labels, collapse = ", "), "\n")
  cat("Vertices:", nrow(x$simplex_vertices), "\n")
  cat("Edges:", nrow(x$simplex_edges), "\n")
  invisible(x)
}
