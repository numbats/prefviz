#' Getter functions to extract components from ternable object for high-dimensional ternary plots
#' 
#' @description
#' Performs additional transformations on ternable object components, making it
#' ready for high-dimensional ternary plots with the `tourr` package.
#' 
#' @param ternable A ternable object created by [ternable()].
#'
#' @return 
#' - `get_tern_data()`: A data frame combining ternary coordinates of 
#'   observations with those of simplex vertices (without vertex labels). 
#'   Used as input data for `tourr`.
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
#'  get_tern_data(tern),
#'  edges = get_tern_edges(tern),
#'  obs_labels  = get_tern_labels(tern),
#'  axes = "bottomleft")
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