library(tidyverse)
library(ggplot2)

pref_2022 <- read_csv("inst/dev/pref_2022.csv")
pref_2025 <- read_csv("inst/dev/pref_2025.csv")

ternary_tour22 <- ternable(pref_2022, c(LNP, ALP, Other))
ternary_tour25 <- ternable(pref_2025, 3:7)

tern_data <- cbind(ternary_tour22$data, ternary_tour22$ternary_coord)
vert <- ternary_tour22$simplex_vertices

ggplot(tern_data |> filter(CountNumber == 0), aes(x = x1, y = x2)) +
  geom_polygon(data = vert, aes(x = x1, y = x2), fill = NA, color = "black") +
  scale_y_reverse() +
  coord_fixed(ratio = 1) +
  theme_void() +
  geom_text(
        data = vert, 
        aes(x = x1, y = x2, label = labels),
        nudge_x=c(-0.02, 0.02, 0),
        nudge_y=c(-0.05, -0.05, 0.05)) +
  geom_point(aes(color = ElectedParty))

sp <- ternary_tour25$simplex_vertices |> 
  select(-labels)
tern_data25 <- rbind(sp, ternary_tour25$ternary_coord)
labels <- c(ternary_tour25$simplex_vertices$labels, rep("", nrow(ternary_tour25$ternary_coord)))

tourr::animate_xy(
  tern_data25,
  edges = ternary_tour25$simplex_edges,
  obs_labels = labels
)

# Reverse sp
sp <- ternary_tour22$simplex_vertices |> 
  mutate(x2 = x2*-1)

reverse_tern <- ternary_tour22$ternary_coord |> 
  mutate(x2 = x2*-1)

combined_df <- cbind(ternary_tour22$data, reverse_tern)

ggplot(combined_df |> filter(CountNumber == 0), aes(x = x1, y = x2)) +
  geom_polygon(data = sp, aes(x = x1, y = x2), fill = NA, color = "black") +
  coord_fixed(ratio = 1) +
  geom_point(aes(color = ElectedParty)) +
  theme_void()
