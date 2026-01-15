# Electoral boundaries map for the 2025 Australian Federal Election

Provides the points that make up the boundaries of each electoral
division in the 2025 Australian Federal Election.

## Usage

``` r
elb_map
```

## Format

A tibble of 8 columns:

- long:

  Longitude of point in polygon

- lat:

  Latitude of point in polygon

- hole:

  Whether the polygon has a hole

- piece:

  Polygon piece number

- group:

  Polygon group number

- order:

  Order of polygon within group

- id:

  Unique identifier for polygon

- elect_div:

  Electoral division name

## Source

Australian Electoral Commission (AEC)
<https://www.aec.gov.au/electorates/maps.htm>

## Examples

``` r
library(ggplot2)
library(ggthemes)
#> Error in library(ggthemes): there is no package called ‘ggthemes’

# Load the dataset
data(elb_map)

# Plot the map
ggplot(elb_map) + 
  geom_polygon(
    aes(x = long, y = lat, group = group),
    fill = "grey90", color = "white") +
  theme_map()
#> Error in theme_map(): could not find function "theme_map"
```
