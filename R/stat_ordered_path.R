#' Reorder observations for a path geom
#' 
#' @description
#' `stat_ordered_path()` reorders observations along each path using a user-supplied
#' ordering aesthetic (`order_by`) before drawing the path. The statistic can be
#' used to ensure that paths are drawn in a consistent order even when the input
#' data are not pre-sorted. This is equivalent to reordering the data before 
#' passing it to `geom_path()`.
#'
#' @details
#' The statistic expects an `order_by` aesthetic that supplies the variable used
#' to order observations within each group. 
#' 
#' Missing values `order_by` are handled according to `na_method`. 
#' If `na_method` is "drop_na", missing values are dropped from the data, and the path is 
#' still drawn, but might skipping steps due to missing values. If `na_method` is "drop_group", 
#' the entire group whose missing values belong is dropped from the data, and the path is not drawn.
#' 
#' Ties in `order_by` are allowed but trigger a warning and preserve the original 
#' row order for tied values.
#' 
#' Duplicates in `order_by` are dropped and a warning is issued.
#'
#' Grouping is controlled via the usual `group` aesthetic. If a `group` column
#' is present in the data, reordering is performed independently within each
#' group; otherwise, the entire data is treated as a single path.
#' 
#' @inheritParams ggplot2::layer
#' @param decreasing Logical. If `TRUE`, paths are ordered in decreasing order
#'   of `order_by`. If `FALSE` (default), ordering is increasing.
#' @param na_method Character string specifying how to handle missing values in
#'   `order_by`. One of:
#'   * `"drop_na"` (default): drop only rows where `order_by` is `NA`;
#'   * `"drop_group"`: drop entire groups that contain any `NA` in `order_by`.
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. Must
#'   at least supply `x`, `y`, and `order_by`. The `group` aesthetic can be used
#'   to define separate paths.
#' @param data Data frame to be used for this layer. If `NULL`, the default,
#'   the data is inherited from the plot.
#' @param geom The geometric object to use to draw the paths. Defaults to `"path"`.
#' @param show.legend Logical or `NA`. Should this layer be included in the
#'   legends? `NA`, the default, includes the layer if any aesthetics are
#'   mapped.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them.
#' @param ... Additional parameters passed on to the underlying geom.
#'
#' @section Aesthetics:
#' `stat_ordered_path()` understands the following aesthetics (required are in
#' bold).
#'
#' * **x**
#' * **y**
#' * **order_by**
#' * group
#' * alpha, colour, linewidth, linetype, etc. (inherited from [ggplot2::geom_path()])
#'
#' @examples
#' # Data prep
#' input_df <- prefviz:::aecdop22_widen |> 
#'    filter(DivisionNm %in% c("Higgins", "Monash"))
#' tern22 <- ternable(input_df, ALP:Other)
#' 
#' # Base plot
#' p <- get_tern_data(tern22, plot_type = "2D") |> 
#'   ggplot(aes(x = x1, y = x2)) +
#'   geom_ternary_cart() +
#'   geom_ternary_region(
#'     aes(fill = after_stat(vertex_labels)),
#'     vertex_labels = tern22$vertex_labels,
#'     alpha = 0.3, color = "grey50",
#'     show.legend = FALSE
#'   ) +
#'   geom_point(aes(color = ElectedParty)) +
#'   add_vertex_labels(tern22$simplex_vertices) +
#'   scale_color_manual(
#'     values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
#'     aesthetics = c("fill", "colour")
#'   )
#' 
#' # Add ordered paths
#' p + 
#'   stat_ordered_path(
#'     aes(group = DivisionNm, order_by = CountNumber, color = ElectedParty), 
#'     size = 0.5)
#' 
#' @return A ggplot2 layer that can be added to a plot object.
#' @name stat_ordered_path

#' @export
#' @rdname stat_ordered_path
stat_ordered_path <- function(mapping = NULL, data = NULL, geom = "path",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, 
                         decreasing = TRUE,
                         na_method = c("drop_na", "drop_group"),
                         ...) {
  
  na_method <- match.arg(na_method)
  
  ggplot2::layer(
    stat = StatOrderedPath, 
    geom = geom, 
    data = data, 
    mapping = mapping,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      decreasing = decreasing,
      na_method = na_method,
      ...
    )
  )
}

#' @export
#' @rdname stat_ordered_path
StatOrderedPath <- ggplot2::ggproto("StatOrdered", ggplot2::Stat,
  
  default_geom = "path",
  
  required_aes = c("x", "y", "order_by"),
  
  compute_group = function(data, scales, 
                           decreasing, 
                           na_method) {

    #group_chr <- "group"
    #order_chr <- "order_by"

    ordered_path_df(
      data = data,
      group = "group",
      order_by = "order_by",
      decreasing = decreasing,
      na_method = na_method
    )
  }
)