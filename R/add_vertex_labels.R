#' Add vertex labels to ternary plot
#'
#' @description
#' Adds text labels at the vertices of a ternary simplex with automatic
#' positioning adjustments.
#'
#' @param vertex_labels_df A data frame containing vertex coordinates and labels.
#'   Should have columns `x1`, `x2`, and `labels`. Can be specified manually or 
#'   obtained from a ternable object: `ternable_object$simplex_vertices`.
#' @param nudge_x Numeric vector of length 3 specifying horizontal nudges for each vertex label.
#' @param nudge_y Numeric vector of length 3 specifying vertical nudges for each vertex label.
#' @param ... Arguments passed to [ggplot2::geom_text()], such as
#'   `size`, `colour`, `fontface`, etc.
#'
#' @examples
#' library(ggplot2)
#' 
#' # Create a ternable object
#' aecdop22_transformed <- prefviz:::aecdop22_transformed
#' tern <- ternable(aecdop22_transformed, ALP:Other)
#' 
#' ggplot() +
#'   geom_ternary_cart() +
#'   add_vertex_labels(tern$simplex_vertices, size = 5, fontface = "bold")
#'
#' @export
add_vertex_labels <- function(vertex_labels_df, 
                              nudge_x = c(-0.02, 0.02, 0),
                              nudge_y = c(-0.05, -0.05, 0.05),...) {
  vertex_labels_df <- vertex_labels_df |>
    dplyr::mutate(x2 = x2*-1)
  
  ggplot2::geom_text(
    data = vertex_labels_df,
    mapping = ggplot2::aes(x = x1, y = x2, label = labels),
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    inherit.aes = FALSE,
    ...
  )
}
