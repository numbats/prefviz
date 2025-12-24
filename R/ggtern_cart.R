#' Ternary plot in cartesian coordinates

ggtern_cart2d <- function(data, alternatives = NULL, label = TRUE, ...){
  tern_object <- ternable(data, alternatives)
  tern_data <- cbind(tern_object$data, tern_object$ternary_coord)
  vert <- tern_object$simplex_vertices

  p <- ggplot(tern_data, aes(x = x1, y = x2)) +
    geom_polygon(data = vert, aes(x = x1, y = x2), fill = NA, color = "black") +
    scale_y_reverse() +
    coord_fixed(ratio = 1) +
    theme_void()

  if(label){
    p <- p +
      geom_text(
        data = vert, 
        aes(x = x1, y = x2, label = labels),
        nudge_x=c(-0.02, 0.02, 0),
        nudge_y=c(-0.05, -0.05, 0.05))
  }

  return(p)
}
