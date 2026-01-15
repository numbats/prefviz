# Add vertex labels to ternary plot

Adds text labels at the vertices of a ternary simplex with automatic
positioning adjustments.

## Usage

``` r
add_vertex_labels(
  vertex_labels_df,
  nudge_x = c(-0.02, 0.02, 0),
  nudge_y = c(-0.05, -0.05, 0.05),
  ...
)
```

## Arguments

- vertex_labels_df:

  A data frame containing vertex coordinates and labels. Should have
  columns `x1`, `x2`, and `labels`. Can be specified manually or
  obtained from a ternable object: `ternable_object$simplex_vertices`.

- ...:

  Arguments passed to
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html),
  such as `size`, `colour`, `fontface`, etc.

## Examples

``` r
library(ggplot2)

# Create a ternable object
aecdop22_transformed <- prefviz:::aecdop22_transformed
tern <- ternable(aecdop22_transformed, ALP:Other)

ggplot() +
  geom_ternary_cart() +
  add_vertex_labels(tern$simplex_vertices, size = 5, fontface = "bold")

```
