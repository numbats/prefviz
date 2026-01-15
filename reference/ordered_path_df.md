# Validate and order path data

Internal helper to validate and reorder data within each group according
to the `order_by` aesthetic. Used by
[`stat_ordered_path()`](https://numbats.github.io/prefviz/reference/stat_ordered_path.md).

## Usage

``` r
ordered_path_df(
  data,
  group,
  order_by,
  decreasing = FALSE,
  na_method = c("drop_na", "drop_group")
)
```

## Arguments

- data:

  A data frame containing at least an `order_by` column (and optionally
  a `group` column created by ggplot2).

- decreasing:

  Logical. If `TRUE`, sort `order_by` in decreasing order; otherwise in
  increasing order.

- na_method:

  Character string indicating how to handle missing values in
  `order_by`. One of `"drop_na"` or `"drop_group"`.

## Value

A data frame with the same columns as `data`, but potentially fewer rows
(after dropping rows or groups) and with rows reordered within each
group.
