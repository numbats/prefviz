#' Transform Compositional Data using Helmert Transformation
#'
helmert_transform <- function(data, alternatives = NULL) {
  # Check input type
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Input must be a matrix or a data frame")
  }

  # Convert input to matrix
  if (is.null(alternatives)) {
    input_mat <- as.matrix(data)
  } else {
    # Validate that alternatives exist in data
    if (is.data.frame(data)) {
      missing_cols <- setdiff(alternatives, names(data))
      if (length(missing_cols) > 0) {
        stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      }
    }
    input_mat <- as.matrix(data[, alternatives, drop = FALSE])
  }

  # Check for valid compositional data (positive values)
  if (any(input_mat < 0)) {
    stop("Input contains negative values. Compositional data must be non-negative.")
  }
  
  if (any(input_mat == 0)) {
    warning("Input contains zeros. These will be replaced with a small value for log-ratio transformation.")
    input_mat[input_mat == 0] <- 1e-10
  }

  # Check if the rows sum to 1, if not, normalize and give warning
  row_sums <- rowSums(input_mat)
  if (!all(dplyr::near(row_sums, 1))) {
    warning("Input rows do not sum to 1. Normalizing automatically.")
    input_mat <- input_mat / row_sums
  }

  # Helmert transformation using compositions package
  # ilr() performs isometric log-ratio transformation with Helmert basis
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
