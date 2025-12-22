#' Create a ternable object
#' 
ternable <- function(data, alternatives = everything(), ...) {
  stopifnot(is.data.frame(data))

  alternative_col_ind <- tidyselect::eval_select(
      rlang::enquo(alternatives), 
      data)
  alternative_col_chr <- colnames(data)[alternative_col_ind]

  validate_df <- .validate_ternable(data, alternative_col_chr)

  new_ternable(validate_df, alternative_col_chr)
}

#' Validate input for ternable
#' 
.validate_ternable <- function(data, alternative_col_chr) {
  alt_data <- data[, alternative_col_chr, drop = FALSE]

  # At least 3 alternatives
  if (ncol(alt_data) < 3) {
    stop(
      "At least 3 alternatives are required.",
      call. = FALSE
    )
  }

  # All alternatives are numeric
  if (!all(sapply(alt_data, is.numeric))) {
    stop(
      "All alternative columns must be numeric.",
      call. = FALSE
    )
  }
  
  # No negative values allowed
  if (any(alt_data < 0, na.rm = TRUE)) {
    stop(
      "Alternative values cannot be negative.",
      call. = FALSE
    )
  }

  # Normalize if rows don't sum to 1
  row_sums <- rowSums(alt_data, na.rm = TRUE)
  tolerance <- 1e-6
  
  if (!all(abs(row_sums - 1) < tolerance)) {
    warning(
      "Not all rows sum to 1. Normalizing alternatives automatically.",
      call. = FALSE
    )
    data[, alternative_col_chr] <- alt_data / row_sums
  }

  invisible(data)
}

#' Low-level constructor of ternable object
#' 
new_ternable <- function(data, alternative_col_chr, ...) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(alternative_col_chr))

  # Transform data using helmert matrix
  cart_df <- helmert_transform(data, alternatives = alternative_col_chr)

  # Define the simplex
  simp <- geozoo::simplex(p = length(alternative_col_chr) - 1)
  simp_points <- data.frame(simp$points)
  colnames(simp_points) <- paste0("x", 1:ncol(simp_points))

  # Define the vertex labels
  labels <- c(alternative_col_chr, rep("", nrow(cart_df)))

  # Combine data
  simp_points$labels <- alternative_col_chr
  cart_df_simp <- dplyr::bind_rows(simp_points, cart_df) |> 
    dplyr::mutate(labels = labels)

  structure(
    list(
      data = cart_df_simp,
      ternary_coord = cart_df_simp |> dplyr::select(paste0("x", 1:ncol(simp_points))),
      simplex_edges = matrix(simp$edges),
      simplex_points = simp_points,
      vertex_labels = labels,
      alternative_names = alternative_col_chr
    ),
    class = "ternable"
  )
}

#' Print method
print.ternable <- function(x, ...) {
  cat("Ternable object\n")
  cat("----------------\n")
  cat("Alternatives:    ", paste(x$alternative_names, collapse = ", "), "\n")
  cat("Simplex vertices:", nrow(x$simplex_points), "\n")
  cat("Edges:           ", nrow(x$simplex_edges), "\n")
  invisible(x)
}
