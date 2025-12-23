#' Getters for high-dimensional ternary plots
#' 
#' Performs additional transformations on ternable object to make it 
#' ready for visualication with `tourr`
get_tern_data <- function(ternable) {
  sp <- ternable$simplex_vertices |> 
    select(-labels)
  data <- dplyr::bind_rows(ternable$ternary_coord, sp)
  return(data)
}

get_tern_edges <- function(ternable) {
  edges <- ternable$simplex_edges
  return(edges)
}

get_tern_labels <- function(ternable) {
  vert_labels <- ternable$simplex_vertices$labels
  labels <- c(vert_labels, rep("", nrow(ternable$ternary_coord)))
  return(labels)
}