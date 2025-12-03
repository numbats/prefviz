library(votevizr)
library(prefio)
library(tidyverse)
library(ggtern)
library(plotly)

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

# Not compatible with ggplotly
# ggplotly(fst_pref_tern)

plot_ly(
  aec_pref,
  a = ~Other,
  b = ~ALP, 
  c = ~LNP,
  color = ~ElectedParty,
  colors = c("ALP" = "red", "LNP" = "blue", "Other" = "green"),
  type = "scatterternary",
  mode = "markers",
  marker = list(size = 8),
  text = ~paste0(
    "<b>", DivisionNm, "</b><br>",
    "ALP: ", round(ALP, 2), "%<br>",
    "Other: ", round(Other, 2), "%<br>",
    "LNP: ", round(LNP, 2), "%<br>",
    "Elected: ", ElectedParty
  ),
  hoverinfo = "text"
) %>%
  layout(
    ternary = list(
      sum = 100,
      aaxis = list(title = "Other"),
      baxis = list(title = "ALP"),
      caxis = list(title = "LNP")
    )
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
  data = aec_round |> filter(DivisionNm %in% c("Richmond", "Cowper")),
  aes(x = ALP, y = Other, z = LNP)) +
  geom_point(
    aes(color = ElectedParty, shape = DivisionNm),
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
  ) +
  labs(title = "Round-by-round movement in Richmond and Cowper") +
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
