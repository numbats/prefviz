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

# Adapt to NSW pref
# Not really sure how to do this atp, converting pref to %pref -> Discussion on input type
nsw_pref <- read_preflib(here::here("data-raw/00058-00000274.soi"))
nsw_pref[which(pref_length(nsw_pref$preferences) == 3),]
