#' Getter functions to extract components from ternable object for ternary plots
#'
#' @description
#' Performs additional transformations on ternable object components, making it
#' ready for both 2D ternary plot with `ggplot2` and
#' high-dimensional ternary plots with `tourr`.
#'
#' @param ternable A ternable object created by [as_ternable()].
#' @param include_data Logical. Only in `get_tern_edges()`. If `TRUE`, return data edges, along with simplex edges.
#'   If `FALSE`, only return simplex edges.
#'
#' @return
#' - `get_tern_data2d()`: A data frame augmenting the original data with its
#'   ternary coordinates (`x1`, `x2`). Used as input data for 2D ternary plot with `ggplot2`.
#' - `get_tern_datahd()`: A data frame combining the simplex vertices with the
#'   original data and ternary coordinates. The `labels` column contains labels
#'   for the vertexes and `""` for data rows. Pass the coordinate columns
#'   (`dplyr::select(starts_with("x"))`) to `tourr` and use the `labels` column
#'   directly for `obs_labels`.
#' - `get_tern_edges()`: A matrix of simplex edge connections for drawing
#'   the simplex boundary.
#'   \itemize{
#'     \item If `include_data = FALSE`, the matrix contains only the simplex edges.
#'     Equivalent to `ternable$simplex_edges`.
#'     \item If `include_data = TRUE`, the matrix combines the simplex edges with
#'     the data edges. Used when you want to draw lines between the data points.
#'   }
#'
#' @details
#' These functions are designed to work together for creating animated tours
#' of high-dimensional ternary data:
#' - `get_tern_datahd()` provides both the point coordinates and observation labels
#' - `get_tern_edges()` provides the simplex structure
#'
#' @section Deprecated:
#' `get_tern_labels()` is deprecated as of version 0.1.2. Use
#' `get_tern_datahd(ternable)[["labels"]]` instead.
#'
#' @examples
#' library(ggplot2)
#' # Create a ternable object
#' tern <- as_ternable(aecdop22_transformed, ALP:Other)
#'
#' # Use with tourr (example)
#' \dontrun{
#' tourr_data <- get_tern_datahd(tern)
#' tourr::animate_xy(
#'   dplyr::select(tourr_data, starts_with("x")),
#'   edges = get_tern_edges(tern),
#'   obs_labels = tourr_data[["labels"]],
#'   axes = "bottomleft")
#' }
#'
#' # Use with ggplot2 (example)
#' ggplot(get_tern_data2d(tern), aes(x = x1, y = x2)) +
#'   add_ternary_base() +
#'   geom_point(aes(color = ElectedParty))
#'
#' @seealso [as_ternable()] for creating ternable objects
#'
#' @name ternary_getters
NULL

#' @rdname ternary_getters
#' @export
get_tern_data2d <- function(ternable) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  tern_coord <- ternable$ternary_coord |>
    dplyr::mutate(x2 = x2 * -1)
  dplyr::bind_cols(ternable$data, tern_coord)
}

#' @rdname ternary_getters
#' @export
get_tern_datahd <- function(ternable) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  tourr_data <- ternable$data |>
    dplyr::bind_cols(ternable$ternary_coord)
  ternable$simplex_vertices |>
    dplyr::bind_rows(tourr_data) |>
    dplyr::mutate(labels = ifelse(is.na(labels), "", labels))
}

#' @rdname ternary_getters
#' @export
get_tern_edges <- function(ternable, include_data = FALSE) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  n_vertices <- length(ternable$vertex_labels)
  se <- ternable$simplex_edges
  de <- data.frame(ternable$data_edges) |> 
    dplyr::mutate(
      Var1 = Var1 + n_vertices,
      Var2 = Var2 + n_vertices
    )

  if (include_data) {
    edges <- dplyr::bind_rows(data.frame(se), de) |> as.matrix()
    return(edges)
  } else {
    return(se)
  }
}

#' @rdname ternary_getters
#' @export
get_tern_labels <- function(ternable) {
  lifecycle::deprecate_warn(
    when = "0.1.2",
    what = "get_tern_labels()",
    details = 'Use `get_tern_datahd(ternable)[["labels"]]` instead.'
  )
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  vert_labels <- ternable$vertex_labels
  labels <- c(vert_labels, rep("", nrow(ternable$ternary_coord)))
  return(labels)
}