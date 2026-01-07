#' Create polygonal regions in a ternary plot based on a reference point
#' 
#' @description
#' `geom_ternary_region()` and `stat_ternary_region()` divide the ternary triangle 
#' into three polygonal regions centered around a specific reference point. 
#' 
#' Geometrically, lines are drawn from the reference point perpendicular to the 
#' three edges of the triangle. These lines partition the simplex into three zones, 
#' where each zone is associated with the closest vertex (item). This is often 
#' used to visualize "winning regions" or catchment areas for each item.
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param x1,x2,x3 Numeric values defining the reference point in ternary coordinates 
#'   (proportions). Must sum to 1 (or will be normalized). Default is `c(1/3, 1/3, 1/3)` 
#'   (the centroid), which divides the space into three equal regions.
#' @param vertex_labels Character vector of length 3 providing names for the regions. 
#'   The order must correspond to the three vertices of the ternary plot. 
#'   If `NULL`, regions are labeled "Region 1", "Region 2", and "Region 3", starting from
#'   the rightmost vertex and moving clockwise.
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. 
#'   To map aesthetics to the computed region labels, use [ggplot2::after_stat()], 
#'   e.g., `aes(fill = after_stat(vertex_labels))`.
#' @param geom The geometric object to use to display the data. Default is `"polygon"`.
#' @param show.legend Logical. Should this layer be included in the legends?
#'   `NA` (default) includes it if aesthetics are mapped. `FALSE` never includes it;
#'   `TRUE` always includes it.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than 
#'   combining with them.
#' 
#' @section Computed variables:
#' `stat_ternary_region()` calculates the following variables which can be accessed 
#' with `after_stat()`:
#' \describe{
#'   \item{`x`, `y`}{Cartesian coordinates defining the polygon shapes.}
#'   \item{`id`}{Numeric identifier for the specific geometric points used to build the polygons:
#'     \itemize{
#'       \item 1-3: The main vertices of the ternary triangle.
#'       \item 4: The reference point (center).
#'       \item 5-7: The projection points on the edges.
#'     }
#'   }
#'   \item{`group`}{Integer (1, 2, or 3) identifying which region the polygon belongs to.}
#'   \item{`vertex_labels`}{The label assigned to the region (derived from the 
#'     `vertex_labels` parameter).}
#' }
#' 
#' @examples
#' aecdop22_transformed <- prefviz:::aecdop22_transformed
#' 
#' # Get ternable
#' tern22 <- ternable(aecdop22_transformed, ALP:Other)
#' 
#' # Draw the ternary plot
#' ggplot(get_tern_data(tern22, plot_type = "2D"), aes(x = x1, y = x2)) +
#'   geom_ternary_cart() +
#'   geom_ternary_region(
#'     vertex_labels = tern22$vertex_labels,
#'     aes(fill = after_stat(vertex_labels)), 
#'     alpha = 0.3, color = "grey50",
#'     show.legend = FALSE
#'   )
#' 
#' @name geom_ternary_region

#' @export
#' @rdname geom_ternary_region
StatTernaryRegion <- ggplot2::ggproto("StatTernaryRegion", ggplot2::Stat,
  required_aes = character(0),
  
  compute_panel = function(data, scales, 
                          x1, x2, x3,
                          vertex_labels = NULL) {
    
    res <- create_ternary_region(x1, x2, x3)

    if(!is.null(vertex_labels)){
      if(length(vertex_labels) != 3){
        stop("There must be 3 vertex labels.")
      }
      res$vertex_labels <- rep(vertex_labels, each = 5)
    }
    res
  }
)

#' @export
#' @rdname geom_ternary_region
stat_ternary_region <- function(mapping = NULL, data = NULL,
                                geom = "polygon", position = "identity", 
                                show.legend = NA, inherit.aes = FALSE, 
                                x1 = 1/3, x2 = 1/3, x3 = 1/3, 
                                vertex_labels = NULL, ...) {
  ggplot2::layer(
    data = NULL, 
    mapping = mapping, 
    geom = geom,
    stat = StatTernaryRegion, 
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      x1 = x1, x2 = x2, x3 = x3,
      vertex_labels = vertex_labels, ...)
  )
}

#' @export
#' @rdname geom_ternary_region
geom_ternary_region <- function(mapping = NULL, position = "identity", 
                                show.legend = NA, inherit.aes = FALSE, 
                                x1 = 1/3, x2 = 1/3, x3 = 1/3, vertex_labels = NULL, ...) {
  
  # Check if user is trying to map to columns without after_stat() or vertex_labels()
  if (!is.null(mapping)) {
    aes_list <- mapping
    aesthetics_to_check <- c("fill", "colour", "alpha", "group")
    
    for (aes_name in aesthetics_to_check) {
      if (!is.null(aes_list[[aes_name]])) {
        aes_expr <- rlang::quo_get_expr(aes_list[[aes_name]])
        
        # Check if it's a simple column reference (not after_stat())
        is_simple_column <- !rlang::is_call(aes_expr) || 
                           (rlang::is_call(aes_expr) && 
                            !identical(rlang::call_name(aes_expr), "after_stat"))
        
        if (is_simple_column) {
          # Warning for any column mapping
          warning(
            sprintf("aesthetic '%s' is mapped to column from input data. ", aes_name),
            "Note: geom_ternary_region generates its own data. ",
            "If you want to apply aesthetic mappings, use after_stat() with computed variables. ",
            "Use ?stat_ternary_region to learn more about columns that can be specified in after_stat().",
            call. = FALSE
          )
        }

        # Check if user has after_stat() but forgot vertex_labels
        has_after_stat_no_vertex_labels <- is.null(vertex_labels) &&
                                          rlang::is_call(aes_expr) && 
                                          identical(rlang::call_name(aes_expr), "after_stat")
        
        if (has_after_stat_no_vertex_labels) {
          message(
            "Note: You've mapped aesthetics using after_stat(), but 'vertex_labels' is NULL. \n",
            "vertex_labels is default to c('Region 1', 'Region 2', 'Region 3'). \n",
            "To create meaningful labels, provide the 'vertex_labels' argument:\n",
            "  geom_ternary_region(vertex_labels = c('A', 'B', 'C'), ...) \n",
            "If you have used object ternable, vertex_labels can be found via:\n",
            "  geom_ternary_region(vertex_labels = your_ternable_object$vertex_labels, ...) \n"
          )
          break
        }
      }
    }
  } 

  ggplot2::layer(
    data = NULL, 
    mapping = mapping, 
    geom = "polygon", 
    stat = StatTernaryRegion,
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      x1 = x1, x2 = x2, x3 = x3,
      vertex_labels = vertex_labels, ...)
  )
}