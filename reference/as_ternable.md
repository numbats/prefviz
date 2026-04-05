# Create a ternable object

Creates a ternable object, which contains observation coordinates,
simplex vertices, and edges necessary for building a ternary plot in
both two and higher dimensions.

## Usage

``` r
as_ternable(
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
prefviz::aecdop25_transformed
#> # A tibble: 976 × 8
#>    DivisionNm CountNumber ElectedParty   ALP   GRN   LNP     Other    IND
#>    <chr>            <dbl> <chr>        <dbl> <dbl> <dbl>     <dbl>  <dbl>
#>  1 Adelaide             0 ALP          0.465 0.190 0.242 0.104     0     
#>  2 Adelaide             1 ALP          0.467 0.194 0.242 0.0968    0     
#>  3 Adelaide             2 ALP          0.476 0.205 0.244 0.0745    0     
#>  4 Adelaide             3 ALP          0.483 0.210 0.249 0.0578    0     
#>  5 Adelaide             4 ALP          0.493 0.222 0.285 0.0001000 0     
#>  6 Adelaide             5 ALP          0.691 0     0.309 0         0     
#>  7 Aston                0 ALP          0.373 0     0.377 0.209     0.0414
#>  8 Aston                1 ALP          0.373 0     0.378 0.206     0.0434
#>  9 Aston                2 ALP          0.376 0     0.380 0.210     0.0338
#> 10 Aston                3 ALP          0.378 0     0.384 0.202     0.0358
#> # ℹ 966 more rows

# Create the ternable object
tern <- as_ternable(prefviz::aecdop25_transformed, items = ALP:IND)
#> Warning: Not all rows sum to 1. Normalizing items automatically.
tern
#> Ternable object
#> ----------------
#> Items: ALP, GRN, LNP, Other, IND 
#> Vertices: 5 
#> Edges: 20 
```
