library(prefio)
library(tidyverse)
library(ggtern)
library(plotly)
library(sf)
library(crosstalk)
library(ggthemes)


# Data

## AEC DOP 2025 and 2022
url_2022 <- "https://results.aec.gov.au/27966/Website/Downloads/HouseDopByDivisionDownload-27966.csv"
aec_2022 <- read_csv(url_2022, skip = 1) |> 
  filter(CalculationType == "Preference Percent") |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  )) 

url_2025 <- "https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv"
aec_2025 <- read_csv(url_2025, skip = 1) |> 
  filter(CalculationType == "Preference Percent") |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))

## 2025 electoral boundaries + centroids
elb <- st_read(here::here("data-raw/2025_ELB/AUS_ELB_region.shp")) |> 
  rmapshaper::ms_simplify()
elb_df <- elb |> 
  mutate(id = row_number()) |>
  st_cast("MULTIPOLYGON") |>
  st_coordinates() |> 
  as.data.frame() |> 
  rename(long = X, lat = Y) |>
  mutate(
    hole = L1 > 1,
    group = paste0("G.", L3, ".", L2)
  ) |> 
  left_join(
    elb |> 
      mutate(id = row_number()) |>
      st_drop_geometry() |> 
      select(id, Elect_div),
    by = c("L3" = "id")
  )

e_centroid <- st_centroid(elb, of_largest_polygon = TRUE) |> 
  mutate(
    long = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )

# Process data

## Preference by round
round_pref <- function(data) {
  df <- data |> 
    left_join(
      data |> 
        filter(Elected == "Y") |> 
        select(DivisionNm, ElectedParty = Party, CountNumber),
      by = c("DivisionNm", "CountNumber")
    ) |> 
    group_by(DivisionNm, CountNumber, Party, ElectedParty) |> 
    summarise(
      Votes = sum(CalculationValue, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    pivot_wider(
      names_from = Party,
      values_from = Votes,
      values_fill = 0
    ) |> 
    mutate(
      Other = 100 - LNP - ALP,
      across(ALP:Other, ~.x/100)
    )

  return(df)
}

pref_2022 <- round_pref(aec_2022)
pref_2025 <- round_pref(aec_2025)

## First round pref 2022 vs 2025
first_pref <- pref_2022 |> 
  filter(
    CountNumber == 0, 
    !(DivisionNm %in% c("Higgins", "North Sydney"))) |> 
  mutate(year = 2022) |> 
  union_all(
    pref_2025 |> 
      filter(
        CountNumber == 0, 
        !(DivisionNm %in% c("Bullwinkel"))) |> 
      mutate(year = 2025)
  ) |> 
  arrange(DivisionNm, year)

# Function

## Convert barycentric to cartesian
bary2cart <- function(data, x, y, z){
  df <- data |> 
    mutate(
      cart_x = {{y}} + 0.5*{{z}},
      cart_y = sqrt(3/4)*{{z}}
    )

  return(df)
}

## Polygon regions for shading
polygon_shade <- function(cartesian = FALSE){
  regions <- tribble(
    ~ALP,   ~Other, ~LNP,   ~group,
    # ALP region
    33.33,  33.33,  33.33,  "ALP",
    50,     50,     0,      "ALP",
    100,    0,      0,      "ALP",
    50,     0,      50,     "ALP",
    33.33,  33.33,  33.33,  "ALP",
    # LNP region
    33.33,  33.33,  33.33,  "LNP",
    0,      50,     50,     "LNP",
    0,      0,      100,    "LNP",
    50,     0,      50,     "LNP",
    33.33,  33.33,  33.33,  "LNP",
    # Other region
    33.33,  33.33,  33.33,  "Other",
    50,     50,     0,      "Other",
    0,      100,    0,      "Other",
    0,      50,     50,     "Other",
    33.33,  33.33,  33.33,  "Other"
  )

  if(cartesian){
    regions <- bary2cart(regions, ALP, LNP, Other) |> 
      select(cart_x, cart_y, group)
  }

  return(regions)
}

## Ternary plot in cartesian coordinates
ggtern_cartesian <- function(data, x, y, z, label = TRUE, ...){
  # Define the triangle
  vertice <- data.frame(
    a = c(100, 0, 0),
    b = c(0, 0, 100),
    c = c(0, 100, 0)
  )
  vertice_df <- bary2cart(vertice, a, b, c) |> 
    rename(v_x = cart_x, v_y = cart_y)

  # Plot
  p <- bary2cart(data, {{x}}, {{y}}, {{z}}) |> 
    ggplot(aes(x = cart_x, y = cart_y, ...)) +
    geom_polygon(
      data = vertice_df,
      aes(x = v_x, y = v_y),
      fill = NA, color = "black"
    ) +
    theme_void() +
    coord_fixed(ratio = 1)

  # Add axis labels
  if(label){
    x_label <- rlang::as_label(rlang::ensym(x))
    y_label <- rlang::as_label(rlang::ensym(y))
    z_label <- rlang::as_label(rlang::ensym(z))
    
    p <- p +
      annotate(
        geom = "text", 
        x = c(0, 50, 100), 
        y = c(0, sqrt(3/4)*100, 0) + 5*c(-1, 1, -1), 
        label = c(x_label, z_label, y_label)
      )
  }

  return(p)
}

#Plot

## Cartesian ternary plot
### Join centroids and first pref
pref_2025_centroid <- pref_2025 |> 
  filter(CountNumber == 0) |> 
  left_join(
    e_centroid |> 
      select(Elect_div, long, lat),
    by = c("DivisionNm" = "Elect_div")
  ) |> 
  mutate(text = paste0(DivisionNm, "\n",
                "ALP: ", round(ALP, 1), "%\n",
                "LNP: ", round(LNP, 1), "%\n",
                "Other: ", round(Other, 1), "%"))

### Specify party color
party_colors <- c(
  "ALP" = "red",
  "LNP" = "blue",
  "Other" = "grey10"
)

### Ternary plot
p_ternary_unlinked <- ggtern_cartesian(
  pref_2025_centroid,
  x = ALP, y = LNP, z = Other) +
  geom_polygon(data = polygon_shade(cartesian = TRUE),
               aes(x = cart_x, y = cart_y, group = group),
               fill = NA, color = "grey50", alpha = 0.3) +
  geom_point(
    aes(color = ElectedParty, text = text), 
    size = 1, alpha = 0.8) +
  scale_color_manual(values = party_colors, name = "Elected Party")

## Linked ternary plot

sd_ternary <- pref_2025_centroid |> 
  bary2cart(ALP, LNP, Other) |>
  SharedData$new(key = ~DivisionNm)

p_ternary <- sd_ternary |>
  ggplot(aes(x = cart_x, y = cart_y)) +
  geom_polygon(
    data = polygon_shade(cartesian = TRUE),
    aes(x = cart_x, y = cart_y, group = group),
    fill = NA, color = "grey50", alpha = 0.3
  ) +
  geom_point(
    aes(color = ElectedParty, text = text),
    size = 1, alpha = 0.8
  ) +
  annotate(
    geom = "text", 
    x = c(0, 50, 100), 
    y = c(0, sqrt(3/4)*100, 0) + 5*c(-1, 1, -1), 
    label = c("ALP", "LNP", "Other")
  ) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(
    legend.position = "none"
  )

plotly_ternary <- ggplotly(p_ternary, tooltip = "text") |> 
  highlight(
    on = "plotly_selected",
    off = "plotly_deselect",
    opacityDim = 0.3
  ) |> 
  layout(
    title = list(
      text = "First preference by electorate (2025)",
      font = list(size = 16) 
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE)
  ) |> 
  style(hoverinfo = "skip", traces = 2)

## Electorate map
p_elec_map <- ggplot() +
  geom_polygon(
    data = elb_df,
    aes(x = long, y = lat, group = group),
    fill = "grey90", color = "white"
  ) +
  geom_point(
    data = sd_ternary,
    aes(x = long, y = lat, color = ElectedParty, text = text),
    size = 1, alpha = 0.8
  ) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  coord_equal() +
  theme_map()

plotly_map <- ggplotly(p_elec_map, tooltip = "text") |> 
  highlight(
    on = "plotly_selected",
    off = "plotly_deselect",
    opacityDim = 0.3
  ) |> 
  layout(
    title = list(
      text = "Result by electorate (2025)",
      font = list(size = 16)  # Same size
    )
  ) |> 
  style(hoverinfo = "skip", traces = 1)


## Linked map and ternary plot
bscols(
  widths = c(3, 6),
  plotly_ternary,
  plotly_map
)

## Change in first pref between 2022 and 2025
p_change <- ggtern_cartesian(first_pref, x = ALP, y = LNP, z = Other) +
  geom_polygon(data = polygon_shade(cartesian = TRUE),
               aes(x = cart_x, y = cart_y, group = group),
               fill = NA, color = "grey50", alpha = 0.3) +
  geom_path(
    aes(
      x = cart_x,
      y = cart_y,
      group = DivisionNm,
      color = ElectedParty
    ),
    arrow = arrow(length = unit(0.1, "inches")),
    size = 0.5,
    alpha = 0.7
  ) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  labs(
    title = "Change in first preference by electorate (2022 to 2025)"
  )

## Preference flow
p_flow <- ggtern_cartesian(
  pref_2025 |> filter(DivisionNm == "Wright") |> arrange(CountNumber),
  x = ALP, y = LNP, z = Other
) +
  geom_polygon(data = polygon_shade(cartesian = TRUE),
               aes(x = cart_x, y = cart_y, group = group),
               fill = NA, color = "grey50", alpha = 0.3) +
  geom_path(aes(color = ElectedParty)) +
  geom_point(aes(color = ElectedParty)) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  labs(
    title = "Preference flow in Wright (2025)"
  )

ggtern(
  pref_2025 |> filter(DivisionNm == "Monash") |> arrange(CountNumber),
  aes(x = ALP, y = Other, z = LNP)
) +
  geom_polygon(data = polygon_shade(),
               aes(x = ALP, y = Other, z = LNP, group = group),
               fill = NA, color = "grey50", alpha = 0.3) +
  geom_path(aes(color = ElectedParty)) +
  geom_point(aes(color = ElectedParty)) +
  scale_color_manual(values = party_colors, name = "Elected Party") 
