# Transform compositional data using Helmert matrix

Transform n-dimension compositional data (all values sum to 1) into an
(n-1)-dimensional Euclidean space using the Helmert matrix. This
dimension reduction is the geometric basis for plotting points within
the simplex.

## Usage

``` r
helmert_transform(data, items = dplyr::everything(), append = FALSE)
```

## Arguments

- data:

  A data frame or matrix containing the compositional data.

- items:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns representing the components of the composition. Default is
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  which selects all columns. Must select at least 3 columns.

- append:

  (Optional) A logical value indicating whether the transformed data
  should be appended to the original data frame. Default is `FALSE`.

## Value

A data frame containing the Helmert-transformed coordinates, named `x1`,
`x2`, ..., `x(n-1)`, where `n` is the number of items. If
`append = TRUE`, these columns are added to the input `data`.

## Examples

``` r
# Example 1: Transform a matrix (all columns)
comp_mat <- matrix(c(0.5, 0.3, 0.2,
                     0.4, 0.4, 0.2,
                     0.6, 0.2, 0.2),
                   ncol = 3, byrow = TRUE)
helmert_transform(comp_mat)
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> # A tibble: 3 × 2
#>         x1    x2
#>      <dbl> <dbl>
#> 1 1.41e- 1 0.163
#> 2 6.50e-18 0.163
#> 3 2.83e- 1 0.163

# Example 2: Transform specific columns in a data frame
df <- data.frame(
  electorate = c("A", "B", "C"),
  ALP = c(0.5, 0.4, 0.6),
  LNP = c(0.3, 0.4, 0.2),
  Other = c(0.2, 0.2, 0.2)
)
helmert_transform(df, items = c(ALP, LNP, Other))
#> # A tibble: 3 × 2
#>         x1    x2
#>      <dbl> <dbl>
#> 1 1.41e- 1 0.163
#> 2 6.50e-18 0.163
#> 3 2.83e- 1 0.163
```
