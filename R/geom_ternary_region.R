#' Helper to calculate the perpendicular projection of a point on a line
perp_proj <- function (A, B, P) {
  AB <- B - A
  AP <- P - A
  t <- sum(AP * AB) / sum(AB * AB)
  intersection <- A + t * AB
  
  return(intersection)
}

#' Helper to create a 3-region ternary polygon
create_ternary_region <- function(x1, x2, x3) {
  # Validate input
  if(sum(x1, x2, x3) - 1 > 1e-8) {
    stop("x1, x2, x3 must sum to 1")
  }

  if(any(x1 < 0, x2 < 0, x3 < 0)) {
    stop("x1, x2, x3 must be non-negative")
  }

  # Vertices
  vert <- geozoo::simplex(p = 2)$points
  colnames(vert) <- paste0("x", seq_len(ncol(vert)))

  # Midpoint
  mid_point <- matrix(c(x1, x2, x3), ncol = 3, byrow = TRUE)
  p4 <- geozoo::f_composition(mid_point)
  names(p4) <- c("x1", "x2")

  # Perp projection
  v1 <- vert[1,] 
  v2 <- vert[2,] 
  v3 <- vert[3,] 

  p5 <- perp_proj(v1, v2, p4)
  p6 <- perp_proj(v1, v3, p4)
  p7 <- perp_proj(v2, v3, p4)

  # Regions
  r1 <- tibble::tibble(
    x = c(v1[1], p5[1], p4[1], p6[1], v1[1]),
    y = c(v1[2], p5[2], p4[2], p6[2], v1[2]),
    id = c("1", "5", "4", "6", "1"),
    group = "1",
    vertex_labels = "Region 1"
  )

  r2 <- tibble::tibble(
    x = c(v2[1], p5[1], p4[1], p7[1], v2[1]),
    y = c(v2[2], p5[2], p4[2], p7[2], v2[2]),
    id = c("2", "5", "4", "7", "2"),
    group = "2",
    vertex_labels = "Region 2"
  )

  r3 <- tibble::tibble(
    x = c(v3[1], p6[1], p4[1], p7[1], v3[1]),
    y = c(v3[2], p6[2], p4[2], p7[2], v3[2]),
    id = c("3", "5", "4", "7", "3"),
    group = "3",
    vertex_labels = "Region 3"
  )

  polygon <- rbind(r1, r2, r3) |> 
    mutate(y = y*-1)

  return(polygon)
}

#' Stat to create a 3-region ternary polygon
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

stat_ternary_region <- function(mapping = NULL, data = NULL, geom = "polygon",
                                position = "identity", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE, 
                                x1 = 1/3, x2 = 1/3, x3 = 1/3, 
                                vertex_labels = NULL, ...) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    geom = geom,
    stat = StatTernaryRegion, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, x1 = x1, x2 = x2, x3 = x3, 
      vertex_labels = vertex_labels, ...)
  )
}

geom_ternary_region <- function(mapping = NULL, data = NULL, 
                                position = "identity", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE, 
                                x1 = 1/3, x2 = 1/3, x3 = 1/3, 
                                vertex_labels = NULL, ...) {
  
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
    data = data, 
    mapping = mapping, 
    geom = GeomPolygon, 
    stat = StatTernaryRegion,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      x1 = x1, x2 = x2, x3 = x3, 
      vertex_labels = vertex_labels, ...)
  )
  }