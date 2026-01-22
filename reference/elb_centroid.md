# Centroids of electoral divisions in the 2025 Australian Federal Election

Provides the centroids of all electorates in the 2025 Australian Federal
Election. The dataset is computed from 2025 Electoral Boundaries data.

## Usage

``` r
elb_centroid
```

## Format

A tibble of 5 columns:

- id:

  Unique identifier for electorate

- elect_div:

  Electoral division name

- area_sqkm:

  Area of the electorate in square kilometres

- long:

  Longitude of the electoratecentroid

- lat:

  Latitude of the electorate centroid

## Source

Australian Electoral Commission (AEC)
<https://www.aec.gov.au/electorates/maps.htm>

## Examples

``` r
library(ggplot2)
library(ggthemes)

# Load the dataset
data(elb_centroid)

# Plot the centroids on top of the electoral boundaries
ggplot(elb_map) + 
  geom_polygon(
    aes(x = long, y = lat, group = group),
    fill = "grey90", color = "white") +
  geom_point(
    data = elb_centroid,
    aes(x = long, y = lat),
    size = 1, alpha = 0.8
  ) +
  theme_map()
```
