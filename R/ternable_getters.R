#' Getter functions to extract components from ternable object for ternary plots
#' 
#' @description
#' Performs additional transformations on ternable object components, making it
#' ready for both 2D ternary plot with `ggplot2` and 
#' high-dimensional ternary plots with `tourr`.
#' 
#' @param ternable A ternable object created by [ternable()].
#' @param plot_type Only in `get_tern_data()`. Character string specifying the type of plot to be drawn. 
#'   Either "2D" for a 2D ternary plot or "HD" for a high-dimensional ternary plot.
#'
#' @return 
#' - `get_tern_data()`: A data frame as input for `ggplot2` or `tourr`.
#'   \itemize{
#'     \item If `plot_type = "2D"`, the data frame augments the original data, with
#'     its ternary coordinates. Used as input data for `ggplot2`.
#'     \item If `plot_type = "HD"`, the data frame combines ternary coordinates of 
#'     original data with those of simplex vertices (without vertex labels). 
#'     Used as input data for `tourr`.
#'  }
#' - `get_tern_edges()`: A matrix of simplex edge connections for drawing 
#'   the simplex boundary. Equivalent to `ternable$simplex_edges`.
#' - `get_tern_labels()`: A character vector containing vertex labels. 
#'   Used as vertex labels for `tourr`, via argument `obs_labels`.
#' 
#' @details
#' These functions are designed to work together for creating animated tours
#' of high-dimensional ternary data:
#' - `get_tern_data()` provides the point coordinates
#' - `get_tern_edges()` provides the simplex structure
#' - `get_tern_labels()` provides labels that align with the data rows
#'
#' @examples
#' \dontrun{
#' # Create a ternable object
#' tern <- ternable(election_data, ALP:Other)
#' 
#' # Use with tourr (example)
#' tourr::animate_xy(
#'  get_tern_data(tern, plot_type = "HD"),
#'  edges = get_tern_edges(tern),
#'  obs_labels  = get_tern_labels(tern),
#'  axes = "bottomleft")
#'
#' # Use with ggplot2 (example)
#' ggplot(get_tern_data(tern, plot_type = "2D"), aes(x = x1, y = x2)) +
#'   geom_ternary_cart() +
#'   geom_point(aes(color = ElectedParty))
#'}
#' 
#' @seealso [ternable()] for creating ternable objects
#' 
#' @name ternary_getters
NULL

#' @rdname ternary_getters
#' @export
get_tern_data <- function(ternable, plot_type = c("2D", "HD")) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")
  plot_type <- match.arg(plot_type)
  
  if(plot_type == "2D"){
    tern_coord <- ternable$ternary_coord |> 
      mutate(x2 = x2*-1)
    data <- cbind(ternable$data, tern_coord)
  } 
  else if(plot_type == "HD"){
    sp <- ternable$simplex_vertices |> 
    select(-labels)
    data <- dplyr::bind_rows(ternable$ternary_coord, sp)
  }

  return(data)
}

#' @rdname ternary_getters
#' @export
get_tern_edges <- function(ternable) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  edges <- ternable$simplex_edges
  return(edges)
}

#' @rdname ternary_getters
#' @export
get_tern_labels <- function(ternable) {
  stopifnot("input should be of class `ternable`" = class(ternable) == "ternable")

  vert_labels <- ternable$simplex_vertices$labels
  labels <- c(vert_labels, rep("", nrow(ternable$ternary_coord)))
  return(labels)
}