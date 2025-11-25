library(votevizr)
library(prefio)
library(tidyverse)
library(ggtern)

### Explore under the hood of votevizr

# Input type: List of candidate preferences
# Not use ggtern while aware that it exists
votevizr::brexit_prefs

# Figure out the region first
brexit_rcv <- first_preference_win_regions(brexit_prefs, split = " > ", method = "RCV")

brexit_rcv %>% 
  ggplot(aes(x = Remain, y = Deal)) +
  geom_polygon(aes(group = candidate, fill = candidate)) + 
  labs(fill = "Winning\ncandidate")

# Then add the points
brexit_rcv_ternary <- brexit_rcv %>% 
  first_preference_shares(split = " > ") %>%
  mutate(Remain = Remain + .5*Deal, Deal = sqrt(3/4)*Deal)

brexit_fps <- brexit_prefs %>% 
  first_preference_shares(split = " > ") %>% 
  mutate(Remain = Remain + .5*Deal, Deal = sqrt(3/4)*Deal)

brexit_rcv_ternary %>%
  ggplot(aes(x = Remain, y = Deal)) +
  geom_polygon(aes(group = candidate, fill = candidate), show.legend = F) +
  geom_point(data = brexit_fps, aes(x = Remain, y = Deal), cex = 0.75) +
  coord_fixed() + # ensures we get an equilateral triangle
  scale_fill_brewer() + 
  theme_void() + 
  expand_limits(x = c(-.1, 1.1), y = sqrt(3/4)*c(-.1, 1.1)) + 
  annotate(geom = "text", x = c(1,.5,0), y = c(0,sqrt(3/4),0) + .05*c(-1,1,-1), label = c("Remain", "Deal", "No deal"))

# Adapt to NSW pref
# Not really sure how to do this atp, converting pref to %pref -> Discussion on input type
nsw_pref <- read_preflib(here::here("data-raw/00058-00000274.soi"))
nsw_pref[which(pref_length(nsw_pref$preferences) == 3),]

# AEC 2022 election data
url <- "https://results.aec.gov.au/27966/Website/Downloads/HouseDopByDivisionDownload-27966.csv"
aec <- read_csv(url, skip = 1) |> 
  filter(CalculationType == "Preference Percent", CountNumber == 0) |> 
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP")) ~ "Other",
    PartyAb %in% c("LP", "NP") ~ "NLP",
    TRUE ~ PartyAb
  )) 

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

ggtern(aec_pref, aes(x = ALP, y = Other, z = NLP)) +
  geom_point(aes(color = ElectedParty), size = 3, alpha = 0.7) +
  theme_bw()

aec_pref
