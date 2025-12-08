library(votevizr)
library(prefio)
library(tidyverse)
library(ggtern)
library(plotly)
library(sf)
library(crosstalk)
library(ggthemes)

# AEC 2022 election data
url <- "https://results.aec.gov.au/27966/Website/Downloads/HouseDopByDivisionDownload-27966.csv"
aec <- read_csv(url, skip = 1) |> 
  filter(CalculationType == "Preference Percent") |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP") ~ "LNP",
    TRUE ~ PartyAb
  )) 

# Transform data into %1st pref + winner
aec_pref <- aec |> 
  filter(CountNumber == 0) |>
  left_join(
    aec |> 
      filter(Elected == "Y", CountNumber == 0) |> 
      select(DivisionNm, ElectedParty = Party),
    by = "DivisionNm"
  ) |> 
  group_by(DivisionNm, Party, ElectedParty) |> 
    summarise(
      Votes = sum(CalculationValue, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    pivot_wider(
      names_from = Party,
      values_from = Votes,
      values_fill = 0
    ) |> 
    mutate(Other = 100 - LNP - ALP) |>
    select(DivisionNm, ALP, LNP, Other, ElectedParty)

# Define the region
alp_region <- tribble(
  ~ALP,   ~Other, ~LNP,
  33.33,  33.33,  33.33,
  50,     50,    0,
  100,    0,      0,
  50,     0,      50,
  33.33,  33.33,  33.33
)

lnp_region <- tribble(
  ~ALP,   ~Other, ~LNP,
  33.33,  33.33,  33.33,
  0,      50,     50,
  0,      0,      100,
  50,      0,      50,
  33.33,  33.33,  33.33
)

other_region <- tribble(
  ~ALP,   ~Other, ~LNP,
  33.33,  33.33,  33.33,
  50,     50,    0,
  0,      100,    0,
  0,      50,      50,
  33.33,  33.33,  33.33
)

fst_pref_tern <- ggtern(data = aec_pref, aes(x = ALP, y = Other, z = LNP)) +
  geom_polygon(data = alp_region,
                fill = "red", alpha = 0.15) +
  geom_polygon(data = lnp_region,
              fill = "blue", alpha = 0.15) +
  geom_polygon(data = other_region,
                fill = "green", alpha = 0.15) +
  geom_point(aes(color = ElectedParty), size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "green"),
    name = "Elected Party"
  ) +
  labs(
    title = "First preference by electorate (2022 election)",
  ) + 
  theme(
    tern.axis.line.T = element_line(color = "green"),
    tern.axis.line.L = element_line(color = "red"),
    tern.axis.line.R = element_line(color = "blue"),
    tern.axis.text.T = element_text(color = "green"),
    tern.axis.text.L = element_text(color = "red"),
    tern.axis.text.R = element_text(color = "blue"),
    tern.axis.title.T = element_text(color = "green"),
    tern.axis.title.L = element_text(color = "red"),
    tern.axis.title.R = element_text(color = "blue")
  )

# Movement across rounds of voting
aec_round <- aec |> 
  left_join(
    aec |> 
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
  mutate(Other = 100 - LNP - ALP)

ggtern(
  data = aec_round, #|> filter(str_detect(DivisionNm, "^[A-B]")),
  aes(x = ALP, y = Other, z = LNP)) +
  geom_point(
    aes(color = ElectedParty),
    size = 2, alpha = 0.8) +
  geom_path(
    aes(group = DivisionNm, color = ElectedParty)
  ) +
  geom_polygon(data = alp_region,
    fill = "red", alpha = 0.15) +
  geom_polygon(data = lnp_region,
    fill = "blue", alpha = 0.15) +
  geom_polygon(data = other_region,
    fill = "green", alpha = 0.15) +
  scale_color_manual(
  values = c("ALP" = "red", "LNP" = "blue", "Other" = "green"),
  name = "Elected Party"
  )
  # labs(title = "Round-by-round movement in Richmond and Cowper") +
  # theme(
  # tern.axis.line.T = element_line(color = "green"),
  # tern.axis.line.L = element_line(color = "red"),
  # tern.axis.line.R = element_line(color = "blue"),
  # tern.axis.text.T = element_text(color = "green"),
  # tern.axis.text.L = element_text(color = "red"),
  # tern.axis.text.R = element_text(color = "blue"),
  # tern.axis.title.T = element_text(color = "green"),
  # tern.axis.title.L = element_text(color = "red"),
  # tern.axis.title.R = element_text(color = "blue")
  # )

## Link map and ternary
url_map <- "https://github.com/jforbes14/eechidna/raw/8d771b33126c5828352a9c16651235f00d664e10/extra-data/nat_map22.rda"
url_centroid <- "https://github.com/jforbes14/eechidna/raw/8d771b33126c5828352a9c16651235f00d664e10/extra-data/nat_data22.rda"

# Download to temp file and load
temp_map <- tempfile(fileext = ".rda")
download.file(url, temp_map, mode = "wb")
load(temp_map)
unlink(temp_map)

temp_centroid <- tempfile(fileext = ".rda")
download.file(url_centroid, temp_centroid, mode = "wb")
load(temp_centroid)
unlink(temp_centroid)

# Join centroid and pref
aec_map <- aec_pref |> 
  mutate(DivisionNm = str_to_upper(DivisionNm)) |>
  left_join(
    nat_data22 |> 
      select(elect_div, long_c, lat_c),
    by = c("DivisionNm" = "elect_div")
  ) |> 
  mutate(
    x_coord = LNP + 0.5*Other,
    y_coord = sqrt(3/4)*Other,
    text = paste0(DivisionNm, "\n",
                     "ALP: ", round(ALP, 1), "%\n",
                     "LNP: ", round(LNP, 1), "%\n",
                     "Other: ", round(Other, 1), "%"))

party_colors <- c(
  "ALP" = "red",
  "LNP" = "blue",
  "Other" = "grey"
)

sd <- SharedData$new(aec_map, key = ~DivisionNm)

# Plot map
p_elec_map <- ggplot() +
  geom_polygon(
    data = nat_map22,
    aes(x = long, y = lat, group = group),
    fill = "grey90", color = "white"
  ) +
  geom_point(
    data = sd,
    aes(x = long_c, y = lat_c, color = ElectedParty, text = text),
    size = 1, alpha = 0.8
  ) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  coord_equal() +
  labs(
    title = "House of Representative result by electorate (2022)"
  ) +
  theme_map() +
  theme(
    legend.position = "none"
  )

# Convert ternary to cartesian
vertice <- data.frame(
  ALP = c(100, 0, 0),
  Other = c(0, 100, 0),
  LNP = c(0, 0, 100)
) |> 
  mutate(x_coord = LNP + 0.5*Other,
         y_coord = sqrt(3/4)*Other)

# Plot ternary
p_ternary <- sd |> 
  ggplot(aes(x = x_coord, y = y_coord)) +
  geom_polygon(
    data = vertice,
    aes(x = x_coord, y = y_coord, text = NULL), 
    fill = NA, color = "grey20"
  ) +
  geom_point(
    aes(color = ElectedParty, text = text), 
    size = 1, alpha = 0.8
  ) +
  annotate(
    geom = "text", 
    x = c(0, 50, 100), 
    y = c(0, sqrt(3/4)*100, 0) + 5*c(-1, 1, -1), 
    label = c("ALP", "Other", "LNP")
  ) +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = party_colors, name = "Elected Party") +
  labs(
    title = "First preference by electorate (2022)"
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )

map_plotly <- ggplotly(p_elec_map, tooltip = "text") |> 
  highlight(
    on = "plotly_selected",
    off = "plotly_deselect",
    opacityDim = 0.3
  ) |> 
  style(hoverinfo = "skip", traces = 1)
ternary_plotly <- ggplotly(p_ternary, tooltip = "text") |>
  highlight(
    on = "plotly_selected",
    off = "plotly_deselect",
    opacityDim = 0.3
  ) |> 
  layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE)
  )

bscols(
  widths = c(3, 9),
  ternary_plotly,
  map_plotly
)

# Shift in preference
pref_change <- aec_round |> 
  filter(CountNumber %in% c(0, 4))

ggtern(
  aec_round |> filter(CountNumber %in% c(0, 4)),
  aes(x = ALP, y = Other, z = LNP)) +
geom_path(aes(
  group = DivisionNm,
  color = ElectedParty
),
arrow = arrow(ends = "first", length = unit(0.1, "inches"))
)
