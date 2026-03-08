# Draw the 2D ternary simplex

Draws the boundary of a 2D ternary simplex as an equilateral triangle

## Usage

``` r
add_ternary_base(...)
```

## Arguments

- ...:

  Arguments passed to
  [`ggplot2::geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.html),
  such as `colour`, `fill`, `linewidth`, etc.

## Examples

``` r
library(ggplot2)

# Basic simplex
ggplot() + add_ternary_base()


# Customize appearance
ggplot() + add_ternary_base(colour = "blue", linewidth = 1.5)

```
