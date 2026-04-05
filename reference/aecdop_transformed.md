# Distribution of preferences in wide form for selected parties (2022 and 2025)

Wide-form versions of the Australian Federal Election distribution-of-
preferences data, aggregated to selected parties within each electoral
division and couting round. Each row gives the vote share for a set of
parties and an "Other" category at a given count stage in a given
division. These datasets are derived from `aecdop_2022` and
`aecdop_2025` for ease of analysis and visualisation.

## Usage

``` r
aecdop22_transformed

aecdop25_transformed
```

## Format

For `aecdop22_transformed`, a tibble with 1,052 rows and 6 columns:

- DivisionNm:

  Name of the electoral division (e.g., "Adelaide").

- CountNumber:

  Round in the counting procedure, starting from 0 (first preference).

- ElectedParty:

  Party abbreviation of the candidate ultimately elected in the division
  (e.g., "ALP", "LNP").

- ALP:

  Proportion of votes for the Australian Labor Party at this count,
  between 0 and 1.

- LNP:

  Proportion of votes for the Coalition grouping at this count, between
  0 and 1.

- Other:

  Proportion of votes for all other parties and candidates combined at
  this count, between 0 and 1.

For `aecdop25_transformed`, a tibble with 976 rows and 8 columns:

- DivisionNm:

  Name of the electoral division (e.g., "Adelaide").

- CountNumber:

  Round in the counting procedure, starting from 0 (first preference).

- ElectedParty:

  Party abbreviation of the candidate ultimately elected in the division
  (e.g., "ALP", "GRN", "LNP", "IND").

- ALP:

  Proportion of votes for the Australian Labor Party at this count,
  between 0 and 1.

- GRN:

  Proportion of votes for the Australian Greens at this count, between 0
  and 1.

- LNP:

  Proportion of votes for the Coalition grouping at this count, between
  0 and 1.

- Other:

  Proportion of votes for all other parties and candidates combined at
  this count, between 0 and 1.

- IND:

  Proportion of votes for independent candidates at this count, between
  0 and 1.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
1052 rows and 6 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 976
rows and 8 columns.

## Details

Two datasets are provided:

- `aecdop22_transformed`: 2022 Federal Election (1,052 rows), aggregated
  to Labor (ALP), Coalition (LNP), and Other (Other)

- `aecdop25_transformed`: 2025 Federal Election (976 rows), aggregated
  to Labor (ALP), Coalition (LNP), Greens (GRN), Independent (IND) and
  Other (Other)

Within each row, the party columns represent proportions that sum to 1
(up to rounding), giving a compositional view of the distribution of
preferences at each count stage. These structures are designed for use
in simplex-based visualisations and related methods.

## See also

[`aecdop_2022`](https://numbats.github.io/prefviz/reference/aecdop.md),
[`aecdop_2025`](https://numbats.github.io/prefviz/reference/aecdop.md)

## Examples

``` r
data(aecdop22_transformed)
data(aecdop25_transformed)

# Proportions for Adelaide over the count in 2022
aecdop22_transformed |>
  dplyr::filter(DivisionNm == "Adelaide")
#> # A tibble: 6 × 6
#>   DivisionNm CountNumber ElectedParty   ALP   LNP Other
#>   <chr>            <dbl> <chr>        <dbl> <dbl> <dbl>
#> 1 Adelaide             0 ALP          0.400 0.32  0.280
#> 2 Adelaide             1 ALP          0.400 0.321 0.279
#> 3 Adelaide             2 ALP          0.403 0.322 0.275
#> 4 Adelaide             3 ALP          0.408 0.327 0.265
#> 5 Adelaide             4 ALP          0.418 0.351 0.232
#> 6 Adelaide             5 ALP          0.619 0.381 0    
```
