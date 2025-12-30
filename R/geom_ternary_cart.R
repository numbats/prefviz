#' Geom version of ggtern_cart2d

geom_ternary_cart <- function() {
  vert <- geozoo::simplex(p = 2)$points
  colnames(vert) <- paste0("x", seq_len(ncol(vert)))
  
  ggplot2::geom_polyon(data = vert, aes(x = x1, y = x2), fill = NA, color = "black") +
    scale_y_reverse() +
    coord_fixed(ratio = 1) +
    theme_void()
}