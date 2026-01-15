# Draw the 2D ternary simplex

Draws the boundary of a 2D ternary simplex as an equilateral triangle

## Usage

``` r
geom_ternary_cart(...)
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
ggplot() + geom_ternary_cart()


# Customize appearance
ggplot() + geom_ternary_cart(colour = "blue", linewidth = 1.5)
#> Warning: Duplicated aesthetics after name standardisation: colour

```
