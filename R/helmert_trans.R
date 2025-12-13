#' Transform compositional data using Helmert matrix
#'
#' @description 
#' Transform n-dimension compositional data (all values sum to 1) to its (n-1)-dimensional space
#' using the Helmert matrix. 
#' 
#' @param data A data frame or matrix containing the compositional data. 
#' @param alternatives (Optional) A character or numeric vector specifying the columns of 
#' compositional data to use. If `NULL` (default), all columns are used.
#' 
#' @return A data frame with the original columns and additional columns 
#' containing the Helmert-transformed coordinates, named `x1`, `x2`, ..., `x(n-1)` where `n` is 
#' the number of dimensions in the compositional data.
#' 
#' @examples
#' \dontrun{
#' # Example 1: Transform a matrix (all columns)
#' comp_mat <- matrix(c(0.5, 0.3, 0.2,
#'                      0.4, 0.4, 0.2,
#'                      0.6, 0.2, 0.2),
#'                    ncol = 3, byrow = TRUE)
#' helmert_transform(comp_mat)
#' 
#' # Example 2: Transform specific columns in a data frame
#' df <- data.frame(
#'   electorate = c("A", "B", "C"),
#'   ALP = c(0.5, 0.4, 0.6),
#'   LNP = c(0.3, 0.4, 0.2),
#'   Other = c(0.2, 0.2, 0.2)
#' )
#' helmert_transform(df, alternatives = c("ALP", "LNP", "Other"))
#'}
helmert_transform <- function(data, alternatives = NULL) {
  # Check input type
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Input must be a matrix or a data frame")
  }

  # Convert input to matrix
  if (is.null(alternatives)) {
    input_mat <- as.matrix(data)
  } else {
    if (is.character(alternatives)) {
      # Check if all alternatives are columns in the data
      missing_cols <- setdiff(alternatives, colnames(data))
      if (length(missing_cols) > 0) {
        stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      }

      input_mat <- as.matrix(data[, alternatives, drop = FALSE])
    } else if (is.numeric(alternatives)) {
      # Validate column indices
      if (any(alternatives < 1) || any(alternatives > ncol(data))) {
        stop("Alternatives column indices must be between 1 and ", ncol(data))
      }

      input_mat <- as.matrix(data[, alternatives, drop = FALSE])
    } else {
      stop("Alternatives must be a character vector or a numeric vector")
    }
  }

  # Check for valid compositional data (positive values)
  if (any(input_mat < 0)) {
    stop("Input contains negative values. Compositional data must be non-negative.")
  }

  # Check if the rows sum to 1, if not, normalize and give warning
  row_sums <- rowSums(input_mat)
  if (!all(dplyr::near(row_sums, 1))) {
    warning("Input rows do not sum to 1. Normalizing automatically.")
    input_mat <- input_mat / row_sums
  }

  # Helmert transformation
  cart_output <- geozoo::f_composition(input_mat)
  
  # Name the output columns
  colnames(cart_output) <- paste0("x", seq_len(ncol(cart_output)))
  
  # Combine with original data
  if (is.data.frame(data)) {
    res <- cbind(data, as.data.frame(cart_output))
  } else {
    res <- cbind(as.data.frame(data), as.data.frame(cart_output))
  }

  return(res)
}
