# Find the Condorcet winner or loser

Scans the TCP tibble to identify the item that beats (or loses to) every
other item head-to-head.

## Usage

``` r
condorcet_find(tcp, items, win = TRUE)
```

## Arguments

- tcp:

  Tibble returned by
  [`pairwise_build_tcp()`](https://numbats.github.io/prefviz/reference/pairwise_build_tcp.md).

- items:

  Character vector of all item names.

- win:

  Logical. `TRUE` to find the Condorcet winner (tcp \> 0.5 in every
  pair); `FALSE` to find the Condorcet loser (tcp \< 0.5 in every pair).

## Value

The item name as a character string, or `NA_character_` if none exists
(e.g. when preference cycles are present).
