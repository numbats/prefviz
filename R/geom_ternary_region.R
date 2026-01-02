#' Create 3 polygonial regions in a ternary plot based on a reference point
#' 
#' @description
#' `geom_ternary_region()` and `stat_ternary_region()` create three polygonal
#' regions in a ternary plot via a reference point. Each region is defined by perpendicular 
#' projections from the reference point to the edges of the ternary triangle. 
#' Each region represents one of the traingular vertex that forms the corresponding region.
#' 
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param x1,x2,x3 Numeric values defining the reference point in ternary coordinates. 
#'   Must sum to 1 (or will be normalized). Default is `1/3, 1/3, 1/3` (centroid), 
#'   diving the ternary space into 3 equal regions.
#' @param vertex_labels Character vector of length 3 giving labels for the three 
#'   regions. The order corresponds to the three vertices of the ternary triangle 
#'   (see Details). If `NULL`, regions are labeled numerically. These labels can be 
#'   accessed in aesthetic mappings via `after_stat(vertex_labels)`.
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. 
#'   To map aesthetics to computed variables (like region labels), use 
#'   [ggplot2::after_stat()], e.g., `aes(fill = after_stat(vertex_labels))`.
#' @param geom The geometric object to use display the data for `stat_ternary_region()`.
#'   Default is `"polygon"`.
#' @param show.legend Logical. Determines whether this layer is included in the legends.
#'   `NA` (the default) includes if any aesthetics are mapped. `FALSE` never includes, 
#'   and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than 
#'   combining with them. This is most useful for helper functions that define both 
#'   data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#' 
#' #' @section Computed variables:
#' These are calculated by `stat_ternary_region()`:
#' \describe{
#'   \item{`x`, `y`}{Cartesian coordinates of polygon vertices}
#'   \item{`id`}{Numeric identifier for each points (
#'    - 1-3 are the vertices of the triangle
#'    - 4 is the reference point
#'    - 5-7 are the projection of the reference point to the edges of the triangle)}
#'   \item{`group`}{Numeric identifier for each region (1, 2, or 3)}
#'   \item{`vertex_labels`}{Label for the vertex, representing each region.
#'    If `vertex_labels` is `NULL`, the labels default to `Region 1, Region 2, Region 3`.}
#' }
#' 
#' @section Aesthetic mappings:
#' geom_ternary_region()` uses [ggplot2::geom_polygon()], so it understands the 
#' same aesthetics. The most commonly used are:
#' \itemize{
#'   \item `fill` - Fill color of regions
#'   \item `colour`/`color` - Border color of regions
#'   \item `alpha` - Transparency (0 = transparent, 1 = opaque)
#'   \item `linewidth` - Width of region borders
#'   \item `linetype` - Type of border lines
#' }
#' 
#' However, since this geom generates its own data based on the reference point parameters, `
#' `after_stat()` is needed to access the computed variables. 
#' The most common use case is to map aesthetics based on the three alternatives, 
#' representing by three ternary vertices. 
#' To do so, use `ggplot2::after_stat(vertex_labels)` in your `aes()` mappings. 
#' See Computed variables section for variables calculated by `stat_ternary_region()`.
#' 
#' TODO: Update with examples once geom_ternary_cart() is done
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
geom_ternary_region <- function(mapping = NULL, data = NULL, 
                                position = "identity", 
                                show.legend = NA, inherit.aes = TRUE, 
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
            "Note: You've mapped aesthetics using after_stat(), but 'vertex_labels' is NULL. ",
            "To create meaningful region labels for your aesthetic mapping, ",
            "provide the 'vertex_labels' argument:\n",
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
    geom = GeomPolygon, 
    stat = StatTernaryRegion,
    position = position,
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      x1 = x1, x2 = x2, x3 = x3,
      vertex_labels = vertex_labels, ...)
  )
}