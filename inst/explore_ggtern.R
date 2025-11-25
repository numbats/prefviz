library(votevizr)
library(prefio)
library(tidyverse)
library(ggtern)

# AEC 2022 election data
url <- "https://results.aec.gov.au/27966/Website/Downloads/HouseDopByDivisionDownload-27966.csv"
aec <- read_csv(url, skip = 1) |> 
  filter(CalculationType == "Preference Percent", CountNumber == 0) |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP")) ~ "Other",
    PartyAb %in% c("LP", "NP") ~ "NLP",
    TRUE ~ PartyAb
  )) 

# Transform data into %1st pref + winner
aec_pref <- aec |> 
  left_join(
    aec |> 
      filter(Elected == "Y") |> 
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

aec_pref2 <- aec_pref |> wide_preferences()

alp_region <- data.frame(
  ALP = c(33.33, 100, 50, 33.33),
  NLP = c(33.33, 0, 50, 33.33),
  Other = c(33.33, 0, 0, 33.33)
)

# NLP winning region: from center to right corner and bottom edge
nlp_region <- data.frame(
  ALP = c(33.33, 50, 0, 33.33),
  NLP = c(33.33, 50, 100, 33.33),
  Other = c(33.33, 0, 0, 33.33)
)

# Other winning region: from center to top corner and edges
other_region <- data.frame(
  ALP = c(33.33, 50, 0, 33.33),
  NLP = c(33.33, 0, 50, 33.33),
  Other = c(33.33, 50, 50, 33.33)
)

# Create the ternary plot with shaded winning regions
ggtern(aec_pref, aes(x = ALP, y = NLP, z = Other)) +
geom_polygon(data = alp_region, aes(x = ALP, y = NLP, z = Other),
              fill = "red", alpha = 0.15, inherit.aes = FALSE) +
geom_polygon(data = nlp_region, aes(x = ALP, y = NLP, z = Other),
              fill = "blue", alpha = 0.15, inherit.aes = FALSE) +
geom_polygon(data = other_region, aes(x = ALP, y = NLP, z = Other),
              fill = "green", alpha = 0.15, inherit.aes = FALSE) +
geom_point(aes(color = ElectedParty), size = 3, alpha = 0.8) +
scale_color_manual(
  values = c("ALP" = "red", "NLP" = "blue", "Other" = "green"),
  name = "Elected Party"
) +
theme_rgbg()
