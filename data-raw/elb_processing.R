library(tidyverse)
library(sf)

## 2025 electoral boundaries
elb <- st_read(here::here("data-raw/2025_ELB/AUS_ELB_region.shp")) |> 
  rmapshaper::ms_simplify()

# Transform ELB map to data frame
elb_map <- elb |> 
  # Convert sf object to data frame
  st_cast("MULTIPOLYGON") |>
  st_coordinates() |> 
  as.data.frame() |> 
  # Extract polygon levels
  mutate(
    hole = L1 > 1,
    piece = paste0("P.", L2),
    group = paste0("G.", L3 - 1, ".", L2)
  ) |> 
  rename(long = X, lat = Y) |>
  # Group polygons
  group_by(group) |> 
  mutate(order = row_number()) |>
  ungroup() |> 
  # Get electoral division names
  left_join(
    elb |> 
      mutate(id = row_number()) |>
      st_drop_geometry() |> 
      select(id, Elect_div),
    by = c("L3" = "id"),
    keep = TRUE
  ) |> 
  mutate(id = as.character(id - 1)) |>
  rename(elect_div = Elect_div) |> 
  select(-L1, -L2, -L3)

# Centroids
elb_centroid <- st_centroid(elb, of_largest_polygon = TRUE) |> 
  mutate(
    long = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) |> 
  st_drop_geometry() |>
  mutate(id = as.character(row_number() - 1)) |>
  rename_with(tolower) |> 
  select(id, elect_div, area_sqkm, long, lat)

usethis::use_data(elb_map, overwrite = TRUE)
usethis::use_data(elb_centroid, overwrite = TRUE)
