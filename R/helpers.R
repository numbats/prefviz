
#' Perpendicular projection of a point onto a line
#'
#' @description
#' Computes the orthogonal projection of point `P` onto the line through
#' points `A` and `B`. `A` and `B` are vertices of a simplex.
#' Used internally to construct ternary region boundaries for `geom_ternary_region()`.
#'
#' @param A Numeric vector of coordinates for the first line point
#' @param B Numeric vector of coordinates for the second line point  
#' @param P Numeric vector of coordinates for the point to project
#'
#' @return Numeric vector of the projected point coordinates
#'
#' @keywords internal
#' @noRd
#' 
perp_proj <- function (A, B, P) {
  AB <- B - A
  AP <- P - A
  t <- sum(AP * AB) / sum(AB * AB)
  intersection <- A + t * AB
  
  return(intersection)
}

#' Create 3 ternary region polygons
#' 
#' @description Internal function that generates polygon coordinates for
#' three regions based on a reference point within the simplex.
#' Used internally to construct ternary region boundaries for `geom_ternary_region()`.
#' 
#' @param x1,x2,x3 Barycentric coordinates of the reference point
#' @return A data frame with columns x, y and group defining vertices of 3 polygons
#' 
#' @keywords internal
#' @noRd
create_ternary_region <- function(x1, x2, x3) {
  # Validate input
  if(sum(x1, x2, x3) - 1 > 1e-8) {
    stop("x1, x2, x3 must sum to 1")
  }

  if(any(x1 < 0, x2 < 0, x3 < 0)) {
    stop("x1, x2, x3 must be non-negative")
  }

  # Vertices
  vert <- geozoo::simplex(p = 2)$points
  colnames(vert) <- paste0("x", seq_len(ncol(vert)))

  # Ref point
  ref_point <- matrix(c(x1, x2, x3), ncol = 3, byrow = TRUE)
  p4 <- geozoo::f_composition(ref_point)
  names(p4) <- c("x1", "x2")

  # Perp projection
  v1 <- vert[1,] 
  v2 <- vert[2,] 
  v3 <- vert[3,] 

  p5 <- perp_proj(v1, v2, p4)
  p6 <- perp_proj(v1, v3, p4)
  p7 <- perp_proj(v2, v3, p4)

  # Regions
  r1 <- tibble::tibble(
    x = c(v1[1], p5[1], p4[1], p6[1], v1[1]),
    y = c(v1[2], p5[2], p4[2], p6[2], v1[2]),
    id = c("1", "5", "4", "6", "1"),
    group = "1",
    vertex_labels = "Region 1"
  )

  r2 <- tibble::tibble(
    x = c(v2[1], p5[1], p4[1], p7[1], v2[1]),
    y = c(v2[2], p5[2], p4[2], p7[2], v2[2]),
    id = c("2", "5", "4", "7", "2"),
    group = "2",
    vertex_labels = "Region 2"
  )

  r3 <- tibble::tibble(
    x = c(v3[1], p6[1], p4[1], p7[1], v3[1]),
    y = c(v3[2], p6[2], p4[2], p7[2], v3[2]),
    id = c("3", "5", "4", "7", "3"),
    group = "3",
    vertex_labels = "Region 3"
  )

  polygon <- rbind(r1, r2, r3) |> 
    dplyr::mutate(y = y*-1)

  return(polygon)
}


#' Validate and order path data
#'
#' @description
#' Internal helper to validate and reorder data
#' within each group according to the `order_by` aesthetic. 
#' Used by `stat_ordered_path()`.
#'
#' @param data A data frame containing at least an `order_by` column (and
#'   optionally a `group` column created by ggplot2).
#' @param decreasing Logical. If `TRUE`, sort `order_by` in decreasing order;
#'   otherwise in increasing order.
#' @param na_method Character string indicating how to handle missing values in
#'   `order_by`. One of `"drop_na"` or `"drop_group"`.
#'
#' @return A data frame with the same columns as `data`, but potentially fewer
#'   rows (after dropping rows or groups) and with rows reordered within each
#'   group.
#'
#' @keywords internal
ordered_path_df <- function(data,
                          group,
                          order_by,
                          decreasing = FALSE,
                          na_method  = c("drop_na", "drop_group")) {

  group_chr <- rlang::as_label(rlang::ensym(group))
  order_chr <- rlang::as_label(rlang::ensym(order_by))
  na_method <- match.arg(na_method)

  process_one_group <- function(df) {
    # NA handling
    na_rows <- is.na(df[[order_chr]])
    if (any(na_rows)) {
      if (na_method == "drop_group") {
        return(df[0, , drop = FALSE])
      } else if (na_method == "drop_na") {
        df <- df[!na_rows, , drop = FALSE]
      }
    }

    # Duplicates handling
    dup_logical <- duplicated(df)
    n_dupes <- sum(dup_logical)
    
    if (n_dupes > 0L) {
      df <- df[!dup_logical, , drop = FALSE]
      warning(sprintf(
        "Dropped %d duplicate row(s).",
        n_dupes
      ), call. = FALSE)
    }

    # Ties handling
    if (nrow(df) > 1L) {
      ties_logical <- duplicated(df[[order_chr]]) | duplicated(df[[order_chr]], fromLast = TRUE)
      if (any(ties_logical)) {
        ties_sum <- sum(ties_logical)
        warning(
          sprintf(
            "%d ties detected for order_by values. ",
            ties_sum
          ),
          "Row order is preserved for tied values.",
          call. = FALSE
        )
      }
    }

    o <- order(df[[order_chr]], decreasing = decreasing, na.last = TRUE)
    return(df[o, , drop = FALSE])
  }

  if (!is.null(group_chr)) {
    res <- data |>
      dplyr::group_by(.data[[group_chr]]) |>
      dplyr::group_modify(~ process_one_group(.x)) |>
      dplyr::ungroup()
  } else {
    res <- process_one_group(data)
  }

  return(res)
}

#' Add data edges
#' 
#' @description
#' Internal helper to create paths/edges between observations in a ternary plot.
#' Used by `new_ternable()` to create the `data_edges` component.
#' 
#' @param data A data frame input from `new_ternable()`
#' @param group_col_chr Character vector of group column names
#' 
#' @keywords internal
add_data_edges <- function(data, group_col_chr) {
  stopifnot(is.character(group_col_chr))

  if (length(group_col_chr) == 0) {
    data_edges <- data |>
      dplyr::mutate(
        Var1 = dplyr::row_number(),
        Var2 = dplyr::lead(Var1, default = dplyr::last(Var1))
      ) |>
      dplyr::select(Var1, Var2)
  } else {
    data_edges <- data |> 
      dplyr::mutate(Var1 = dplyr::row_number()) |>
      group_by(dplyr::across(all_of(group_col_chr))) |>
      dplyr::mutate(Var2 = dplyr::lead(Var1, default = dplyr::last(Var1))) |> 
      dplyr::ungroup() |>
      dplyr::select(Var1, Var2)
  }

  return(data_edges)
}
