library(tourr)

# get centroids of 3 clusters of the flea data
f <- apply(flea[,1:6], 2, function(x) (x-mean(x))/sd(x))
n <- nrow(f)
set.seed(1019)
flea_centroids <- stats::kmeans((f), 3)$centers
flea_aug <- rbind(f, flea_centroids)
col <- c(rep("black", n), rep("orange", 3))
flea_edges <- matrix(c(n+1, n+2, n+1, n+3, n+2, n+3), ncol=2, byrow = TRUE)
animate_xy(flea_aug, edges = flea_edges, 
           col = col, edges.col = c("orange", "blue", "red"), 
           edges.width = 3)

# Get data for Aston
pref_2025 <- read_csv("inst/dev/pref_2025.csv") |> 
  filter(DivisionNm %in% c("Aston", "Monash"))
tern25 <- ternable(pref_2025, ALP:IND)

data_edges <- pref_2025 |> 
  mutate(
    Var1 = row_number() + length(tern25$vertex_labels),
    Var2 = if_else(
      DivisionNm == lead(DivisionNm, default = DivisionNm[n()]),
      lead(Var1, default = Var1[n()]),
      Var1)
  ) |> 
  select(Var1, Var2)

combined_edges <- rbind(data_edges, tern25$simplex_edges) |> as.matrix()

party_colors <- c(
  "ALP" = "#E13940",    # Red
  "LNP" = "#1C4F9C",    # Blue
  "GRN" = "#10C25B",    # Green
  "IND" = "#F39C12",    # Orange
  "Other" = "#95A5A6"   # Gray
)

color_vector <- c(rep("black", 5),
  party_colors[pref_2025$Winner])

# Add data edges

add_data_edges <- function(data, group) {
  group_quo <- rlang::enquo(group)

  if (rlang::quo_is_null(group_quo)) {
    data_edges <- data |>
      dplyr::mutate(
        Var1 = dplyr::row_number(),
        Var2 = dplyr::lead(Var1, default = dplyr::last(Var1))
      ) |>
      dplyr::select(Var1, Var2)
  } else {
    data_edges <- data |>
      dplyr::mutate(
        Var1 = dplyr::row_number(),
        Var2 = dplyr::if_else(
          !!group_quo == dplyr::lead(!!group_quo, default = dplyr::last(!!group_quo)),
          dplyr::lead(Var1, default = dplyr::last(Var1)),
          Var1
        )
      ) |>
      dplyr::select(Var1, Var2)
  }

  return(data_edges)
}


# Animate the tour
animate_xy(
  get_tern_data(tern25, plot_type = "HD"), 
  edges = combined_edges,
  obs_labels  = get_tern_labels(tern25),
  # col = color_vector,
  axes = "bottomleft"
)

#----------------Test df
ordered_path_df(
  test_df,
  group = "cate",
  order_by = order_by
)

df <- get_tern_data(tern22, plot_type = "2D") |> 
  filter(DivisionNm %in% c("Higgins", "Monash"))

p <- ggplot(df, aes(x = x1, y = x2)) +
  geom_ternary_cart() +
  geom_point(aes(color = ElectedParty), size = 1, alpha = 0.8) +
  stat_ordered_path(aes(order_by = CountNumber, group = DivisionNm)) +
  add_vertex_labels(tern22$simplex_vertices)
layer_data(p, 3)

ternable(pref_2022, ALP:Other, group = DivisionNm)
