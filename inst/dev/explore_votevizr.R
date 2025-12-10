library(votevizr)
library(prefio)
library(tidyverse)
library(ggtern)

### Explore under the hood of votevizr

# Input type: List of candidate preferences
# The author did not use ggtern while aware that it exists
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


#### Adapt to preflib format
netflix <- read_preflib("00004 - netflix/00004-00000001.soc", from_preflib = TRUE)

# Convert data to wide format, ready for ggplot
netflix_wide <- netflix |> 
  mutate(
    first_pref = pref_get_items(preferences, rank = 1) |> unlist(),
    second_pref = pref_get_items(preferences, rank = 2) |> unlist(),
    third_pref = pref_get_items(preferences, rank = 3) |> unlist(),
    pref_percent = frequency/sum(frequency),
    winner = pref_irv(netflix, 
      preferences_col = preferences,
      frequency_col = frequency
    )$winner
  )

netflix_first_pref <- netflix_wide |> 
  group_by(first_pref, winner) |> 
  summarise(
    votes = sum(pref_percent),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = first_pref, values_from = votes)

## How the input wrapper works
preflib_soi <- read_preflib("00058 - nswla/00058-00000171.soi", from_preflib = TRUE)

result <- pref_irv(preflib_soi, 
  preferences_col = preferences,
  frequency_col = frequency
)

percent_df <- data.frame()

for (i in 1:length(result$rounds)) { 
  # Get the i-th round of preferences
  round_pref <- result$rounds[[i]] |> 
    mutate(
      pref_percent = value/sum(value),
      round = i) |> 
    select(-value) |> 
    pivot_wider(names_from = candidate, values_from = pref_percent)

  # Record the preference of the current round
  percent_df <- bind_rows(percent_df, round_pref) |> 
    mutate(across(everything(), ~replace_na(.x, 0)))
}

# Add the final winner to the df
percent_df <- percent_df |> 
  mutate(winner = result$winner)