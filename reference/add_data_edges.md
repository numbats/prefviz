# Add data edges

Internal helper to create paths/edges between observations in a ternary
plot. Used by `new_ternable()` to create the `data_edges` component.

## Usage

``` r
add_data_edges(data, group_col_chr)
```

## Arguments

- data:

  A data frame input from `new_ternable()`

- group_col_chr:

  Character vector of group column names
