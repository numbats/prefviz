# Get full distribution of preferences in each instant runoff voting round as percentage

Compute the preference in each round of instant runoff voting from input
data, transforming the results into a tidy format for visualization.
Each row represents one round, with columns for each candidate's
preference percentage and the election winner.

## Usage

``` r
dop_irv(x, value_type = c("percentage", "count"), ...)
```

## Arguments

- x:

  Input data. Accepts the same formats as
  [`prefio::pref_irv()`](https://fleverest.github.io/prefio/reference/pref_irv.html):

  - A preference vector where each element represents one ballot

  - A data frame with a column for preference

- value_type:

  Character string specifying the output format. Either:

  - `"percentage"` (default): Returns vote shares as proportions (0-1)

  - `"count"`: Returns raw vote counts

- ...:

  Additional arguments passed to
  [`prefio::pref_irv()`](https://fleverest.github.io/prefio/reference/pref_irv.html),
  including:

  - `preferences_col`: Column name containing preference orderings

  - `frequency_col`: Column name containing vote frequencies

## Value

A tibble with the following structure:

- `round`: Integer, the round number (1 to n)

- One column per candidate: Numeric, the percentage of votes (0-1) that
  candidate received in that round. NA values are replaced with 0 for
  eliminated candidates.

- `winner`: Character, the name of the eventual IRV winner (same for all
  rows)

## Examples

``` r
# Example 1: From preference vector
votes <- c("A > B > C", "B > A > C", "C > B > A", "A > B > C")
dop_irv(votes, value_type = "count")
#> # A tibble: 0 × 0

# Example 2: From data frame with custom column names
vote_data <- tibble::tibble(
  prefs = c("A > B > C", "B > C > A", "C > A > B"),
  counts = c(100, 75, 25)
)
dop_irv(vote_data, value_type = "percentage",
        preferences_col = prefs,
        frequency_col = counts)
#> # A tibble: 0 × 0
```
