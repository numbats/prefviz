#' Ternary plot in cartesian coordinates

ggtern_cart2d <- function(data, alternatives = NULL, label = TRUE, ...){
  # There must be 3 alternatives to draw the plot
  if(is.null(alternatives)){
    if(ncol(data) != 3){
      stop("To draw a 2D ternary plot, there must be exactly 3 alternatives.")
    }
  } else if(length(alternatives) != 3){
    stop("To draw a 2D ternary plot, there must be exactly 3 alternatives")
  }
  
  if(is.null(alternatives)){
    alternatives <- colnames(data)
  }

  # Define the simplex
  simp <- geozoo::simplex(p = 2)
  sp <- data.frame(simp$points)
  colnames(sp) <- paste0("x", 1:length(sp))
  sp$alternatives <- alternatives

  # Transform compositional data to cartesian coordinates
  cart_data <- helmert_transform(data, alternatives = alternatives)

  # Plot
  p <- ggplot(data = cart_data, aes(x = x1, y = x2)) +
    geom_polygon(data = sp, aes(x = x1, y = x2), fill = NA, color = "black") +
    scale_y_reverse() +
    coord_fixed(ratio = 1) +
    theme_void()

  if(label){
    p <- p +
      geom_text(
        data = sp, 
        aes(x = x1, y = x2, label = alternatives),
        nudge_x=c(-0.06, 0.07, 0),
        nudge_y=c(-0.05, -0.05, 0.05))
  }

  return(p)
}
