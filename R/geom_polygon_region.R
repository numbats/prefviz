#' Helper to calculate the perpendicular projection of a point on a line
perp_proj <- function (A, B, P) {
  AB <- B - A
  AP <- P - A
  t <- sum(AP * AB) / sum(AB * AB)
  intersection <- A + t * AB
  
  return(intersection)
}

#' Helper to create a 3-region ternary polygon
create_polygon_region <- function(x1, x2, x3) {
  # Validate input
  if(sum(x1, x2, x3) != 1) {
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
    x1 = c(v1[1], p5[1], p4[1], p6[1], v1[1]),
    x2 = c(v1[2], p5[2], p4[2], p6[2], v1[2]),
    group = "1",
    id = c("1", "5", "4", "6", "1")
  )

  r2 <- tibble::tibble(
    x1 = c(v2[1], p5[1], p4[1], p7[1], v2[1]),
    x2 = c(v2[2], p5[2], p4[2], p7[2], v2[2]),
    group = "2",
    id = c("2", "5", "4", "7", "2")
  )

  r3 <- tibble::tibble(
    x1 = c(v3[1], p6[1], p4[1], p7[1], v3[1]),
    x2 = c(v3[2], p6[2], p4[2], p7[2], v3[2]),
    group = "3",
    id = c("3", "5", "4", "7", "3")
  )

  polygon <- rbind(r1, r2, r3)

  return(polygon)
}

#' Stat to create a 3-region ternary polygon
StatPolygonRegion <- ggplot2::ggproto("StatPolygonRegion", Stat,
  required_aes = character(0),
  
  compute_panel = function(data, scales, x1, x2, x3) {
    res <- create_polygon_region(x1, x2, x3)
  }
)

stat_polygon_region <- function(mapping = NULL, data = NULL, geom = "polygon",
                                position = "identity", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE, 
                                x1 = 33.33, x2 = 33.33, x3 = 33.33, ...) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    geom = geom,
    stat = StatPolygonRegion, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, x1 = x1, x2 = x2, x3 = x3, ...)
  )
}

geom_polygon_region <- function(mapping = NULL, data = NULL, stat = "StatPolygonRegion",
                                position = "identity", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE, 
                                x1 = 33.33, x2 = 33.33, x3 = 33.33, ...) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    geom = GeomPolygon, 
    stat = stat,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, x1 = x1, x2 = x2, x3 = x3, ...)
  )
}