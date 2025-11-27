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
    !(PartyAb %in% c("LP", "ALP", "NP")) ~ "Other",
    PartyAb %in% c("LP", "NP") ~ "NLP",
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
    mutate(Other = 100 - NLP - ALP) |>
    select(DivisionNm, ALP, NLP, Other, ElectedParty)

# Define the region
alp_region <- tribble(
  ~ALP,   ~Other, ~NLP,
  33.33,  33.33,  33.33,
  50,     50,    0,
  100,    0,      0,
  50,     0,      50,
  33.33,  33.33,  33.33
)

nlp_region <- tribble(
  ~ALP,   ~Other, ~NLP,
  33.33,  33.33,  33.33,
  0,      50,     50,
  0,      0,      100,
  50,      0,      50,
  33.33,  33.33,  33.33
)

other_region <- tribble(
  ~ALP,   ~Other, ~NLP,
  33.33,  33.33,  33.33,
  50,     50,    0,
  0,      100,    0,
  0,      50,      50,
  33.33,  33.33,  33.33
)

fst_pref_tern <- ggtern(data = aec_pref, aes(x = ALP, y = Other, z = NLP)) +
  geom_polygon(data = alp_region,
                fill = "red", alpha = 0.15) +
  geom_polygon(data = nlp_region,
              fill = "blue", alpha = 0.15) +
  geom_polygon(data = other_region,
                fill = "green", alpha = 0.15) +
  geom_point(aes(color = ElectedParty), size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("ALP" = "red", "NLP" = "blue", "Other" = "green"),
    name = "Elected Party"
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
  c = ~NLP,
  color = ~ElectedParty,
  colors = c("ALP" = "red", "NLP" = "blue", "Other" = "green"),
  type = "scatterternary",
  mode = "markers",
  marker = list(size = 8),
  text = ~paste0(
    "<b>", DivisionNm, "</b><br>",
    "ALP: ", round(ALP, 2), "%<br>",
    "Other: ", round(Other, 2), "%<br>",
    "NLP: ", round(NLP, 2), "%<br>",
    "Elected: ", ElectedParty
  ),
  hoverinfo = "text"
) %>%
  layout(
    ternary = list(
      sum = 100,
      aaxis = list(title = "Other"),
      baxis = list(title = "ALP"),
      caxis = list(title = "NLP")
    )
  )