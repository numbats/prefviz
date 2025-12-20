#' Create a ternable object
#' 
ternable <- function(data, alternatives, ...) {
  stopifnot(is.data.frame(data))

  if (is.null(alternatives)) {
    alternative_sel <- seq(ncol(data))
  } else {
    alternatives_sel <- tidyselect::eval_select(
      rlang::enquo(alternatives), 
      data
    )
  }

  alternative_col_chr <- colnames(data)[alternatives_sel]

  .validate_ternable(data, alternative_col_chr)

  new_ternable(data, alternative_col_chr)
}

#' Validate input for ternable
#' 
.validate_ternable <- function(data, alternative_col_chr) {
  alt_data <- data[, alternative_col_chr, drop = FALSE]

  # Check if all alternatives are numeric
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
  cart_df <- helmert_transform(data, alternatives = alternative_col_char)

  # Define the simplex
  simp <- geozoo::simplex(p = length(alternative_col_chr) - 1)
  simp_points <- data.frame(simp$points)
  colnames(simp_points) <- paste0("x", 1:ncol(simp_points))

  # Define the vertex labels
  labels <- c(alternative_col_chr, rep("", nrow(cart_df)))

  # Combine data
  simp_points$labels <- labels
  cart_df_simp <- dplyr::bind_rows(simp_points, cart_df) |> 
    dplyr::mutate(labels = label_vtr)

  # Build the object
  structure(
    list(
      data = cart_df_simp,
      ternary_coord = cart_df_simp |> dplyr::select(dplyr::starts_with("x")),
      simplex_edges = matrix(simp$edges),
      simplex_points = simp_points,
      vertex_labels = labels,
      n_alternatives = length(alternative_col_chr),
      alternative_names = alternative_col_chr,
      n_vertices = ncol(sp)
    )
  )
}