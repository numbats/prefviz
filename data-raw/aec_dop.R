library(tidyverse)
library(readxl)

## AEC DOP 2025 and 2022
aecdop_2022 <- read_csv("data-raw/2022_distribution_of_pref.csv")
aecdop_2025 <- read_csv("data-raw/2025_distribution_of_pref.csv")

# Transform
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
    ungroup() |>
    pivot_wider(
      names_from = Party,
      values_from = Votes,
      values_fill = 0
    )
  
  return(df)
}

aecdop22_transformed <- aecdop_2022 |> 
  filter(CalculationType == "Preference Percent", CountNumber == 0) |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  )) |> 
  round_pref() |> 
  mutate(
    across(ALP:Other, ~.x/100),
    Other = ifelse(1 - ALP - LNP < 0, 0, 1 - ALP - LNP))

aecdop25_transformed <- aecdop_2025 |>
  filter(CalculationType == "Preference Percent", CountNumber == 0) |>
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  )) |> 
  round_pref() |>
  mutate(
    across(ALP:IND, ~.x/100),
    Other = ifelse(1 - ALP - LNP - GRN - IND < 0, 0, 1 - ALP - LNP - GRN - IND)
  )

usethis::use_data(aecdop_2022, overwrite = TRUE)
usethis::use_data(aecdop_2025, overwrite = TRUE)
usethis::use_data(
  aecdop22_transformed, 
  aecdop25_transformed, 
  overwrite = TRUE, 
  internal = TRUE
)
