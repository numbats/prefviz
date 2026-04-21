# Heatmap of pairwise results

Plots a full N×N heatmap of pairwise results from a
[`pairwise_calculator()`](https://numbats.github.io/prefviz/reference/pairwise_calculator.md)
object. Each cell shows how the row item performed against the column
item. Color always encodes the TCP ratio (green = win, red = lose, white
= 50/50).

## Usage

``` r
pairwise_heatmap(x, value = c("tcp", "count"))
```

## Arguments

- x:

  A `pairwise` object returned by
  [`pairwise_calculator()`](https://numbats.github.io/prefviz/reference/pairwise_calculator.md).

- value:

  `"tcp"` (default) or `"count"`. Controls the tile annotation:

  - `"tcp"`: shows the TCP percentage, e.g. `"56.8%"`.

  - `"count"`: shows the raw vote count, e.g. `"2841"`.

## Value

A ggplot object.

## See also

[`pairwise_calculator()`](https://numbats.github.io/prefviz/reference/pairwise_calculator.md)
to compute the input object.

## Examples

``` r
library(prefio)

prefs <- data.frame(
  A = c(1, 1, 1, 2, 2),
  B = c(2, 2, 3, 1, 3),
  C = c(3, 3, 2, 3, 1)
) |>
  wide_preferences(col = vote, ranking_cols = A:C)

result <- pairwise_calculator(prefs, preferences_col = vote)
pairwise_heatmap(result, value = "tcp")

pairwise_heatmap(result, value = "count")

```
