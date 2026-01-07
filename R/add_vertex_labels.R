#' Add vertex labels to ternary plot
#'
#' @description
#' Adds text labels at the vertices of a ternary simplex with automatic
#' positioning adjustments.
#'
#' @param vertex_labels_df A data frame containing vertex coordinates and labels.
#'   Should have columns `x1`, `x2`, and `labels`. Can be specified manually or 
#'   obtained from a ternable object: `ternable_object$simplex_vertices`.
#' @param ... Arguments passed to [ggplot2::geom_text()], such as
#'   `size`, `colour`, `fontface`, etc.
#'
#' @examples
#' library(ggplot2)
#' 
#' # Create a ternable object
#' tern <- ternable(election_data, ALP:Other)
#' 
#' # Add vertex labels
#' ggplot() +
#'   geom_ternary_cart() +
#'   add_vertex_labels(tern$simplex_vertices)
#' 
#' # Customize label appearance
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
    mapping = ggplot2::aes(x = .data$x1, y = .data$x2, label = .data$labels),
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    inherit.aes = FALSE,
    ...
  )
}
