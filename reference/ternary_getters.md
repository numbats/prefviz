# Getter functions to extract components from ternable object for ternary plots

Performs additional transformations on ternable object components,
making it ready for both 2D ternary plot with `ggplot2` and
high-dimensional ternary plots with `tourr`.

## Usage

``` r
get_tern_data(ternable, plot_type = c("2D", "HD"))

get_tern_edges(ternable, include_data = FALSE)

get_tern_labels(ternable)
```

## Arguments

- ternable:

  A ternable object created by
  [`ternable()`](https://numbats.github.io/prefviz/reference/ternable.md).

- plot_type:

  Only in `get_tern_data()`. Character string specifying the type of
  plot to be drawn. Either "2D" for a 2D ternary plot or "HD" for a
  high-dimensional ternary plot.

## Value

- `get_tern_data()`: A data frame as input for `ggplot2` or `tourr`.

  - If `plot_type = "2D"`, the data frame augments the original data,
    with its ternary coordinates. Used as input data for `ggplot2`.

  - If `plot_type = "HD"`, the data frame combines ternary coordinates
    of original data with those of simplex vertices (without vertex
    labels). Used as input data for `tourr`.

- `get_tern_edges()`: A matrix of simplex edge connections for drawing
  the simplex boundary.

  - If `include_data = FALSE`, the matrix contains only the simplex
    edges. Equivalent to `ternable$simplex_edges`.

  - If `include_data = TRUE`, the matrix combines the simplex edges with
    the data edges. Used when you want to draw lines between the data
    points.

- `get_tern_labels()`: A character vector containing vertex labels. Used
  as vertex labels for `tourr`, via argument `vertex_labels`.

## Details

These functions are designed to work together for creating animated
tours of high-dimensional ternary data:

- `get_tern_data()` provides the point coordinates

- `get_tern_edges()` provides the simplex structure

- `get_tern_labels()` provides labels that align with the data rows

## See also

[`ternable()`](https://numbats.github.io/prefviz/reference/ternable.md)
for creating ternable objects

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a ternable object
tern <- ternable(election_data, ALP:Other)

# Use with tourr (example)
tourr::animate_xy(
 get_tern_data(tern, plot_type = "HD"),
 edges = get_tern_edges(tern),
 obs_labels  = get_tern_labels(tern),
 axes = "bottomleft")

# Use with ggplot2 (example)
ggplot(get_tern_data(tern, plot_type = "2D"), aes(x = x1, y = x2)) +
  geom_ternary_cart() +
  geom_point(aes(color = ElectedParty))
} # }
```
