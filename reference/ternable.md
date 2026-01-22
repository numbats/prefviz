# Create a ternable object

Creates a ternable object, which contains observation coordinates,
simplex vertices, and edges necessary for building a ternary plot in
both two and higher dimensions.

## Usage

``` r
ternable(
  data,
  items = dplyr::everything(),
  group = NULL,
  order_by = NULL,
  decreasing = FALSE,
  na_method = c("drop_na", "drop_group"),
  ...
)
```

## Arguments

- data:

  A data frame containing the item (alternative) columns used to
  construct the ternary plot.

- items:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns representing the items to be plotted as vertices of the
  simplex. Default is
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  which selects all columns. Must select at least 3 columns. All columns
  must be non-negative and sum to 1.

- group:

  Optional column name indicating the grouping variable. If specified,
  the data will be grouped by this variable. This is useful for creating
  paths between observations within each group.

- order_by:

  Optional column name indicating the order variable. If specified, the
  data will be ordered by this variable. This is useful for creating
  paths between observations within each group.

- decreasing:

  Logical. If `TRUE`, paths are ordered in decreasing order of
  `order_by`. If `FALSE` (default), ordering is increasing.

- na_method:

  Character string specifying how to handle missing values in
  `order_by`. One of:

  - `"drop_na"` (default): drop only rows where `order_by` is `NA`;

  - `"drop_group"`: drop entire groups that contain any `NA` in
    `order_by`.

- ...:

  Additional arguments (currently unused, reserved for future
  extensions).

## Value

A ternable object (S3 class) containing:

- `data`:

  : The validated and normalized data frame

- `data_coord`:

  : Transformed coordinates for all observations

- `data_edges`:

  : Edge connections for drawing paths between observations

- `simplex_vertices`:

  : Vertex coordinates and labels for the simplex

- `simplex_edges`:

  : Edge connections for drawing the simplex boundary

- `vertex_labels`:

  : Labels of the vertices, same as names of the selected item columns

## Examples

``` r
# Load and transform the dataset
prefviz:::aecdop25_transformed
#> # A tibble: 150 × 8
#>    DivisionNm CountNumber ElectedParty   ALP    GRN   LNP Other    IND
#>    <chr>            <dbl> <chr>        <dbl>  <dbl> <dbl> <dbl>  <dbl>
#>  1 Adelaide             0 ALP          0.465 0.190  0.242 0.104 0     
#>  2 Aston                0 ALP          0.373 0      0.377 0.209 0.0414
#>  3 Ballarat             0 ALP          0.424 0      0.286 0.262 0.0281
#>  4 Banks                0 ALP          0.364 0.119  0.391 0.106 0.0202
#>  5 Barker               0 LNP          0.225 0.0816 0.5   0.135 0.0586
#>  6 Barton               0 ALP          0.471 0.159  0.242 0.128 0     
#>  7 Bass                 0 ALP          0.396 0.129  0.314 0.107 0.0538
#>  8 Bean                 0 ALP          0.410 0.095  0.230 0     0.264 
#>  9 Bendigo              0 ALP          0.335 0      0.402 0.252 0.0105
#> 10 Bennelong            0 ALP          0.454 0.118  0.351 0.078 0     
#> # ℹ 140 more rows

# Create the ternable object
tern <- ternable(prefviz:::aecdop25_transformed, items = ALP:Other)
#> Warning: Not all rows sum to 1. Normalizing items automatically.
tern
#> Ternable object
#> ----------------
#> Items: ALP, GRN, LNP, Other 
#> Vertices: 4 
#> Edges: 12 
```
