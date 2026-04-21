# Build TCP tibble from adjacency matrix

Converts the raw N×N adjacency matrix from
[`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html)
into a tidy tibble with one row per unordered pair {A, B}.

## Usage

``` r
pairwise_build_tcp(adj)
```

## Arguments

- adj:

  N×N integer matrix from
  [`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html).

## Value

A tibble with columns `item_a`, `item_b`, `wins_a`, `wins_b`, `total`,
`tcp_a`, `tcp_b`, `h2h_winner`.

## Details

Matrix indexing note:
[`prefio::wide_preferences()`](https://fleverest.github.io/prefio/reference/preferences.html)
treats ranking values as descending (higher value = more preferred),
which reverses the usual adjacency convention. As a result `adj[a, b]`
counts b's wins over a, so `wins_a` is read from `adj[b, a]`.
