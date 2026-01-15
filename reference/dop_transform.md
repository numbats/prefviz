# Transform AEC distribution of preferences from long to wide format

Transform AEC distribution of preferences from long to wide format, with
optional scaling and normalization. This function is useful for
converting all distribution of preference data with similar format into
format ready for ternary plots.

## Usage

``` r
dop_transform(
  data,
  key_cols,
  value_col,
  item_col,
  normalize = TRUE,
  scale = 1,
  fill_value = 0,
  winner_col = NULL,
  winner_identifier = "Y"
)
```

## Arguments

- data:

  A data frame containing preference or vote distribution data, with
  format similar to
  https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv

- key_cols:

  Columns that identify unique observations, e.g., DivisionNm,
  CountNumber

- value_col:

  Numeric and non-negative. Column containing the numeric values to
  aggregate, e.g., CalculationValue, Votes.

- item_col:

  Column name containing the items (candidates/parties) of the election,
  e.g., Party, Candidate. This column will become column names in the
  output wide format.

- normalize:

  Logical. If `TRUE` (default), normalizes values within each group to
  sum to 1. If `FALSE`, returns raw aggregated values.

- scale:

  Numeric. If `normalize = FALSE`, divides all values by this scale
  factor. Default is 1 (no scaling).

- fill_value:

  Numeric. Value to use for missing combinations after pivoting. Default
  is 0.

- winner_col:

  Optional character string specifying a column that indicates the
  winner/elected party. If provided, this column will be joined back to
  the output based on key columns. Useful for preserving election
  outcome information. Default is `NULL`.

- winner_identifier:

  Optional character string specifying the value in `winner_col` that
  identifies winning candidates (e.g., "Y", "Elected"). Only used if
  `winner_col` is specified. Default is "Y".

## Value

A data frame in wide format with:

- Key columns identifying each observation

- Columns for each item (candidate/party) containing
  aggregated/normalized values

- Winner column (if `winner_col` was specified)

## Examples

``` r
# Convert AEC 2025 Distribution of Preference data to wide format
data(aecdop_2025)

# We are interested in the preferences of Labor, Coalition, Greens and Independent. 
# The rest of the parties are aggregated as Other.
aecdop_2025 <- aecdop_2025 |>
 filter(CalculationType == "Preference Percent") |> 
  mutate(PartyAb = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
   TRUE ~ PartyAb))
#> Error in mutate(filter(aecdop_2025, CalculationType == "Preference Percent"),     PartyAb = case_when(!(PartyAb %in% c("LP", "ALP", "NP", "LNP",         "LNQ")) ~ "Other", PartyAb %in% c("LP", "NP", "LNP",         "LNQ") ~ "LNP", TRUE ~ PartyAb)): could not find function "mutate"

dop_transform(
  data = aecdop_2025,
  key_cols = c(DivisionNm, CountNumber),
  value_col = CalculationValue,
  item_col = Party,
  winner_col = Elected
)
#> Error in dop_transform(data = aecdop_2025, key_cols = c(DivisionNm, CountNumber),     value_col = CalculationValue, item_col = Party, winner_col = Elected): Columns not found in data: Party
```
