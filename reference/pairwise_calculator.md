# Compute pairwise results from ranked preference data

Computes all pairwise comparisons from a set of ranked preferences using
[`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html).
For each pair of items, reports the number of voters who preferred each
item and the Two-Candidate Preferred (TCP) ratio. Also identifies the
Condorcet winner and loser where they exist.

## Usage

``` r
pairwise_calculator(x, preferences_col = NULL, frequency_col = NULL)
```

## Arguments

- x:

  A `preferences` object, or a data frame / tibble containing a
  `preferences`-typed column.

- preferences_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  When `x` is a data frame, the column containing the preferences.
  Passed directly to
  [`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html).

- frequency_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional column containing ballot frequencies. Passed directly to
  [`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html).

## Value

An S3 object of class `"pairwise"` with four components:

- `pairwise_matrix`:

  N×N integer matrix from
  [`prefio::adjacency()`](https://fleverest.github.io/prefio/reference/adjacency.html).

- `two_candidate_preferred`:

  Tibble with one row per pair, columns: `item_a`, `item_b`, `wins_a`,
  `wins_b`, `total`, `tcp_a`, `tcp_b`, `h2h_winner`.

- `condorcet_winner`:

  Name of the Condorcet winner, or `NA` if none.

- `condorcet_loser`:

  Name of the Condorcet loser, or `NA` if none.

## Details

**TCP ratio** (`tcp_a`, `tcp_b`) is computed as
`wins / (wins_a + wins_b)`. The denominator is the number of voters who
expressed a preference between the specific pair, not the total number
of voters. This correctly handles partial rankings where some voters did
not rank all items.

**Condorcet winner**: the item that beats every other item head-to-head
(tcp \> 0.5 in all pairs it appears in). At most one can exist.

**Condorcet loser**: the item that loses to every other item
head-to-head (tcp \< 0.5 in all pairs it appears in). At most one can
exist. Neither winner nor loser may exist when preference cycles are
present.

## See also

[`pairwise_heatmap()`](https://numbats.github.io/prefviz/reference/pairwise_heatmap.md)
to visualise the results.

## Examples

``` r
library(prefio)

prefs <- data.frame(
  A = c(1, 1, 1, 2, 2),
  B = c(2, 2, 3, 1, 3),
  C = c(3, 3, 2, 3, 1)
) |>
  wide_preferences(col = vote, ranking_cols = A:C)

result <- pairwise_calculator(prefs, preferences_col = vote)
print(result)
#> Pairwise analysis (3 items)
#> 
#> Head-to-head results (first 5 rows):
#>  item_a item_b wins_a wins_b total tcp_a tcp_b h2h_winner
#>       A      B      4      1     5 80.0% 20.0%          A
#>       A      C      4      1     5 80.0% 20.0%          A
#>       B      C      3      2     5 60.0% 40.0%          B
#> 
#> Condorcet winner: A
#> Condorcet loser:  C
```
