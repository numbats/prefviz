# Distribution of preferences by candidate by division in the Australian Federal Election (2022 and 2025)

Provides details on how votes are distributed and transferred among
candidates in all count stages of the preferential voting system. All
electoral divisions in the Australian Federal Election are included.

## Usage

``` r
aecdop_2022

aecdop_2025
```

## Format

A tibble of 14 columns:

- StateAb:

  State or territory abbreviation (e.g., "ACT", "NSW", "VIC")

- DivisionID:

  Numeric identifier for the electoral division

- DivisionNm:

  Name of the electoral division (e.g., "Bean", "Canberra")

- CountNumber:

  Round in the counting procedure, starting from 0 (first preference)

- BallotPosition:

  Position of the candidate on the ballot paper

- CandidateID:

  Unique numeric identifier for the candidate

- Surname:

  Candidate's surname

- GivenNm:

  Candidate's given name(s)

- PartyAb:

  Party abbreviation (e.g., "UAPP", "ALP", "LP")

- PartyNm:

  Full party name (e.g., "United Australia Party", "Australian Labor
  Party")

- Elected:

  Whether the candidate was elected: "Y" (yes) or "N" (no)

- HistoricElected:

  Whether the candidate was elected in the previous election: "Y" (yes)
  or "N" (no)

- CalculationType:

  Type of calculation:

  - Preference CountNumber of votes received

  - Preference PercentPercentage of votes received

  - Transfer CountNumber of votes transferred from other candidates

  - Transfer PercentPercentage of votes transferred from other
    candidates

- CalculationValue:

  Numeric value for the calculation type (votes or percentage)

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 35096 rows and 14 columns.

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 30888 rows and 14 columns.

## Source

Australian Electoral Commission (AEC) [Distribution of Preferences
2022](https://results.aec.gov.au/27966/Website/HouseDownloadsMenu-27966-Csv.htm)
[Distribution of Preferences
2025](https://results.aec.gov.au/31496/Website/HouseDownloadsMenu-31496-Csv.htm)

## Details

Two datasets are provided:

- `aecdop_2022`: 2022 Federal Election (35,096 rows)

- `aecdop_2025`: 2025 Federal Election (30,888 rows)

## Examples

``` r
# Load the datasets
data(aecdop_2022)
data(aecdop_2025)

# First preferences for Bean division in 2022
aecdop_2022 |> 
  dplyr::filter(DivisionNm == "Bean",
         CountNumber == 0,
         CalculationType == "Preference Count")
#> # A tibble: 6 × 14
#>   StateAb DivisionID DivisionNm CountNumber BallotPosition CandidateID Surname 
#>   <chr>        <dbl> <chr>            <dbl>          <dbl>       <dbl> <chr>   
#> 1 ACT            318 Bean                 0              1       36239 CONWAY  
#> 2 ACT            318 Bean                 0              2       37455 AMBARD  
#> 3 ACT            318 Bean                 0              3       36231 SMITH   
#> 4 ACT            318 Bean                 0              4       32130 CHRISTIE
#> 5 ACT            318 Bean                 0              5       36243 SAVERY  
#> 6 ACT            318 Bean                 0              6       37198 HIATT   
#> # ℹ 7 more variables: GivenNm <chr>, PartyAb <chr>, PartyNm <chr>,
#> #   Elected <chr>, HistoricElected <chr>, CalculationType <chr>,
#> #   CalculationValue <dbl>
```
