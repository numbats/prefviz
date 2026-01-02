#' Draw the 2D ternary simplex
#'
#' @description
#' Draws the boundary of a 2D ternary simplex as an equilateral triangle
#'
#' @param ... Arguments passed to [ggplot2::geom_polygon()], such as
#'   `colour`, `fill`, `linewidth`, etc.
#'
#' @examples
#' library(ggplot2)
#' 
#' # Basic simplex
#' ggplot() + geom_ternary_cart()
#' 
#' # Customize appearance
#' ggplot() + geom_ternary_cart(colour = "blue", linewidth = 1.5)
#'
#' @export
geom_ternary_cart <- function(...) {
  vert <- geozoo::simplex(p = 2)$points
  vert_df <- as.data.frame(vert)
  colnames(vert_df) <- c("x", "y")
  vert_df$y <- vert_df$y * -1
  
  list(
    ggplot2::geom_polygon(
      data = vert_df,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      fill = NA,
      colour = "black",
      inherit.aes = FALSE,
      ...
    ),
    ggplot2::coord_fixed(ratio = 1),
    ggplot2::theme_void()
  )
}
