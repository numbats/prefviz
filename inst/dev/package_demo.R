library(tidyverse)
library(prefviz)
library(ggplot2)
library(tourr)
library(detourr)
library(ggiraph)

#### 2D

# 3 parts raw
pref25_2d <- aecdop_2025 |> 
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other", 
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))

# Transform
pref25_2d <- dop_transform(
  data = pref25_2d,
  key_cols = c(DivisionNm, CountNumber),
  value_col = CalculationValue,
  item_col = Party,
  winner_col = Elected,
  winner_identifier = "Y"
)

# Ternable
tern_2d <- ternable(pref25_2d, items = ALP:Other)

# Plot
input_data <- get_tern_data(tern_2d, plot_type = "2D") |> 
  mutate(text = paste0(DivisionNm, "\n",
                "ALP: ", round(ALP*100, 1), "%\n",
                "LNP: ", round(LNP*100, 1), "%\n",
                "Other: ", round(Other*100, 1), "%"),
        text = gsub("'", "", text),
        DivisionNm = gsub("'", "", DivisionNm)
      )

p2d <- ggplot(input_data |> filter(CountNumber == 0), aes(x = x1, y = x2)) +
  geom_ternary_cart() + 
  geom_ternary_region(
    x1 = 1/3, x2 = 1/3, x3 = 1/3,
    aes(fill = after_stat(vertex_labels)), 
    vertex_labels = tern_2d$vertex_labels,
    alpha = 0.3, color = NA, show.legend = FALSE
  ) +
  add_vertex_labels(tern_2d$simplex_vertices) + 
  geom_point_interactive(aes(color = Winner, tooltip = text, data_id = DivisionNm)) + 
  scale_fill_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70")
  ) +
  scale_color_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
    name = "Elected Party"
  )

p2d_interactive <- girafe(
  ggobj = p2d,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_tooltip(
      css = "background-color:white;padding:8px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);font-size:12px;"
    )
  )
)

# Line
line_input <- input_data |> 
  filter(DivisionNm %in% c("Higgins", "Monash", "Melbourne"))

# The base plot
p2d_line <- ggplot(line_input, aes(x = x1, y = x2)) +
  geom_ternary_cart() +
  geom_ternary_region(
    aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern_2d$vertex_labels,
    alpha = 0.3, color = "grey50",
    show.legend = FALSE
  ) +
  geom_point_interactive(
    aes(color = Winner, 
        tooltip = text, 
        data_id = DivisionNm)
  ) + 
  stat_ordered_path(
    aes(group = DivisionNm, order_by = CountNumber, color = Winner), 
    size = 0.5
  ) +
  add_vertex_labels(tern_2d$simplex_vertices) +
  scale_fill_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70")
  ) +
  scale_color_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
    name = "Elected Party"
  )

p2d_line_interactive <- girafe(
  ggobj = p2d_line,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_tooltip(
      css = "background-color:white;padding:8px;border-radius:5px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);font-size:12px;"
    )
  )
)

# 5 parts raw
pref25_hd <- aecdop_2025 |> 
  filter(CalculationType == "Preference Percent", CountNumber == 0) |>
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other", 
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))

pref25_hd <- dop_transform(
  data = pref25_hd,
  key_cols = c(DivisionNm, CountNumber),
  value_col = CalculationValue,
  item_col = Party,
  winner_col = Elected,
  winner_identifier = "Y"
)

# Ternable
tern_hd <- ternable(pref25_hd, ALP:IND)

# Detour
party_colors <- c(
  "ALP" = "#E13940",    # Red
  "LNP" = "#1C4F9C",    # Blue
  "GRN" = "#10C25B",    # Green
  "IND" = "#F39C12",    # Orange
  "Other" = "#95A5A6"   # Gray
)

col_first_pref <- c(rep("black", 5),
  party_colors[pref25_hd$Winner])

dtour_data <- tern_hd$simplex_vertices |>
  mutate(Winner = labels) |> 
  mutate(Winner = factor(Winner, levels = c("ALP", "LNP", "GRN", "IND", "Other"))) |>
  bind_rows(get_tern_data(tern_hd, plot_type = "2D")) |> 
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
    edges = get_tern_edges(tern_hd),
    size = 1.5
  )
de
