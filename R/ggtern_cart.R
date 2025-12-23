#' Ternary plot in cartesian coordinates

ggtern_cart2d <- function(data, alternatives = NULL, label = TRUE, ...){
  
  tern_object <- ternable(data, alternatives)
  tern_data <- cbind(ternary_tour22$data, ternary_tour22$ternary_coord)
  vert <- ternary_tour22$simplex_vertices

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
