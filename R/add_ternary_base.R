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
#' ggplot() + add_ternary_base()
#' 
#' # Customize appearance
#' ggplot() + add_ternary_base(colour = "blue", linewidth = 1.5)
#'
#' @export
add_ternary_base <- function(...) {
  vert <- geozoo::simplex(p = 2)$points
  vert_df <- as.data.frame(vert)
  colnames(vert_df) <- c("x", "y")
  vert_df$y <- vert_df$y * -1
  
  defaults <- list(colour = "black", fill = NA)
  params <- utils::modifyList(defaults, rlang::list2(...))

  list(
    do.call(
      ggplot2::geom_polygon,
      c(
        list(
          data = vert_df,
          mapping = ggplot2::aes(x = .data$x, y = .data$y),
          inherit.aes = FALSE
        ),
        params
      )
    ),
    ggplot2::coord_fixed(ratio = 1),
    ggplot2::theme_void()
  )
}