library(tidyverse)
library(ggplot2)

pref_2022 <- read_csv("inst/dev/pref_2022.csv") |> filter(CountNumber == 0)
pref_2025 <- read_csv("inst/dev/pref_2025.csv")

ternary_tour22 <- ternable(pref_2022, ALP:Other, group = CountNumber, order_by = ALP, decreasing = TRUE)
ternary_tour25 <- ternable(pref_2025, 3:7)

tern_data <- cbind(ternary_tour22$data, ternary_tour22$ternary_coord)
vert <- ternary_tour22$simplex_vertices

ggplot(
  get_tern_data(ternary_tour22, plot_type = "2D"),
  aes(x = x1, y = x2)
) +
  geom_ternary_cart() +
  geom_ternary_region(
    vertex_labels = ternary_tour22$alternatives,
    aes(fill = after_stat(vertex_labels)), 
    alpha = 0.5, color = "grey50",
    show.legend = FALSE
  ) +
  geom_point(aes(color = ElectedParty)) +
  add_vertex_labels(ternary_tour22$simplex_vertices) +
  labs(title = "1st preference in Australian Federal election 2022")

# HD
sp <- ternary_tour25$simplex_vertices |> 
  select(-labels)
tern_data25 <- rbind(sp, ternary_tour25$ternary_coord)
labels <- c(ternary_tour25$simplex_vertices$labels, rep("", nrow(ternary_tour25$ternary_coord)))

tourr::animate_xy(
  tern_data25,
  edges = ternary_tour25$simplex_edges,
  obs_labels = labels
)

# Different alphabetical order

pref_2025_trunc <- pref_2025 |>
  filter(CountNumber == 0, Winner %in% c("ALP", "LNP", "IND")) |>
  select(Winner, ALP, LNP, IND)

ttern_2025 <- ternable(pref_2025_trunc, ALP:IND)

ggtern_cart2d(ttern_2025$data, alternatives = c("ALP", "LNP", "IND")) +
  geom_ternary_region(
    vertex_labels = ttern_2025$alternatives,
    aes(fill = after_stat(vertex_labels)), 
    alpha = 0.5, color = "grey50"
  ) +
  geom_point(aes(color = Winner)) +
  scale_fill_manual(
    values = c("ALP" = "#009E73", "LNP" = "#D55E00", "IND" = "#CC79A7"),
    aesthetics = c("fill", "colour")) 
