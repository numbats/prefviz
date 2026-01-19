library(tourr)
library(ggplot2)
library(tidyverse)

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

pref_2025 |> 
  mutate(Var1 = row_number()) |> 
  group_by(DivisionNm) |> 
  mutate(Var2 = lead(Var1, default = Var1[n()]))

tern25 <- ternable(pref_2025, ALP:IND, group = DivisionNm)
get_tern_edges(tern25, include_data = TRUE)

party_colors <- c(
  "ALP" = "#E13940",    # Red
  "LNP" = "#1C4F9C",    # Blue
  "GRN" = "#10C25B",    # Green
  "IND" = "#F39C12",    # Orange
  "Other" = "#95A5A6"   # Gray
)

color_vector <- c(rep("black", 5),
  party_colors[pref_2025$Winner])

# Animate the tour
animate_xy(
  get_tern_data(tern25, plot_type = "HD"), 
  edges = get_tern_edges(tern25, include_data = TRUE),
  obs_labels  = get_tern_labels(tern25),
  col = color_vector,
  axes = "bottomleft"
)

#----------------Test df
ordered_path_df(
  test_df,
  group = "cate",
  order_by = order_by
)

pref_2022 <- read_csv("inst/dev/pref_2022.csv") |> 
  filter(DivisionNm %in% c("Higgins", "Monash"))

tern22 <- ternable(pref_2022, ALP:Other, group = DivisionNm)
cbind(tern22$data, tern22$data_edges)

add_data_edges(pref_2022, "DivisionNm")

#----- Detour

# Color
colors <- c(
  "ALP" = "red",    # Red
  "LNP" = "blue",    # Blue
  "GRN" = "green",    # Green
  "IND" = "orange",    # Orange
  "Other" = "gray"   # Gray
)

# First preference scatter
first_pref_2025 <- read_csv("inst/dev/pref_2025.csv") |> 
  filter(CountNumber == 0)

tern_first_pref <- ternable(first_pref_2025, ALP:IND)

col_first_pref <- c(rep("black", 5),
  party_colors[first_pref_2025$Winner])

animate_xy(
  get_tern_data(tern_first_pref, plot_type = "HD"), 
  edges = get_tern_edges(tern_first_pref),
  obs_labels  = get_tern_labels(tern_first_pref),
  col = col_first_pref,
  axes = "bottomleft"
)

d_edges <- get_tern_edges(tern_first_pref)
dtour_data <- tern_first_pref$simplex_vertices |>
  mutate(
    Winner = labels
  ) |> 
  bind_rows(get_tern_data(tern_first_pref, plot_type = "2D")) |> 
  mutate(text = if_else(
    is.na(labels), 
    paste0(DivisionNm, "\n",
          "Elected Party: ", Winner, "\n",
          "ALP: ", round(ALP*100, 1), "%\n",
          "LNP: ", round(LNP*100, 1), "%\n",
          "GRN: ", round(GRN*100, 1), "%\n",
          "IND: ", round(IND*100, 1), "%\n",
          "Other: ", round(Other*100, 1), "%"),
    labels
  ))

de <- detour(
  dtour_data,
  tour_aes(projection = x1:x4, colour = Winner, label = text)
) |> 
  tour_path(grand_tour(3), fps = 60) |>
  show_scatter(
    axes = FALSE, 
    palette = party_colors,
    edges = get_tern_edges(tern_first_pref),
    size = 1.5
  )
de
