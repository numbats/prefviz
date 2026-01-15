# Transforming voting data to ternable-friendly format

``` r
library(prefviz)
library(prefio)
library(dplyr)
library(kableExtra)
```

## Introduction

Raw voting data comes in all shapes and forms, and not all voting data
can be easily transformed into a format that is ready for ternary plots.

This vignette shows how to transform common voting data formats into a
`ternable`-friendly format that can be used for ternary plots. These
include PrefLib-formatted data, raw ballot data in long and wide forms,
and AEC distribution of preference data.

## `ternable`-friendly data

[`ternable()`](https://numbats.github.io/prefviz/reference/ternable.md)
creates a `ternable` object, which is a S3 object that contains the data
and metadata necessary for ternary plots.

A `ternable`-friendly data frame must have the following
characteristics:

- At least 3 numeric columns representing the preferences for 3
  items/candidates of the election. These columns will act as the
  vertices of the ternary plot.
- Values in these item/candidate columns must be non-negative, and sum
  to 1.

``` r
df <- tibble(
  electorate = c("A", "B", "C"),
  PartyA = c(0.5, 0.4, 0.6),
  PartyB = c(0.3, 0.4, 0.2),
  PartyC = c(0.2, 0.2, 0.2)
)

df
#> # A tibble: 3 × 4
#>   electorate PartyA PartyB PartyC
#>   <chr>       <dbl>  <dbl>  <dbl>
#> 1 A             0.5    0.3    0.2
#> 2 B             0.4    0.4    0.2
#> 3 C             0.6    0.2    0.2
```

The above data frame is a minimal example of a `ternable`-friendly data
frame. It contains 3 columns, each representing the preferences for a
party in a specific electorate. The values in these columns are all
non-negative and sum to 1.

## PrefLib-formatted data

[PrefLib format](https://preflib.github.io/PrefLib-Jekyll/format) is a
common format for storing preferential data, and can be easily
downloaded using
[`prefio::read_preflib()`](https://fleverest.github.io/prefio/reference/read_preflib.html)
function.

In this example, we will use the [NSW Legislative Assembly Election
dataset](https://preflib.github.io/PrefLib-Jekyll/dataset/00058).

``` r
nswla <- read_preflib("00058 - nswla/00058-00000171.soi", from_preflib = TRUE)
nswla
#> # A tibble: 1,104 × 2
#> # ℹ 1,094 more rows
#> # ℹ 2 more variables: preferences <prefrncs>, frequency <int>
```

The dataset contains preferential data, with each row representing a
unique set of preferences and their respective frequencies. What is
missing to transform this data into a `ternable`-friendly format is the
breakdown of preferences for each candidate in each round of voting.

The function
[`dop_irv()`](https://numbats.github.io/prefviz/reference/dop_irv.md) in
this package helps us simulate the Instant Runoff Voting (IRV) process
to get the round-by-round preferences for each candidate, and then
transform the data into a `ternable`-friendly format. A caveat for this
function is that it does not handle ties in preferences given a ballot,
which works fine under Australian IRV rules, but may not be appropriate
for other electoral systems.

``` r
dop_irv(
  nswla, value_type = "percentage",
  preferences_col = preferences,
  frequency_col = frequency)
#> # A tibble: 4 × 8
#>   round `HAYLEN Jo` `WEI Leo` `RAUE Tom` `MAKRIS Andrea` `ROMANOVSKY Teresa`
#>   <int>       <dbl>     <dbl>      <dbl>           <dbl>               <dbl>
#> 1     1       0.464     0.233      0.206          0.0572              0.0252
#> 2     2       0.469     0.236      0.209          0.0595              0.0270
#> 3     3       0.478     0.240      0.218          0.0646              0     
#> 4     4       0.504     0.253      0.244          0                   0     
#> # ℹ 2 more variables: `SINDEN Dale` <dbl>, winner <chr>
```

## Raw ballot data in long and wide formats

Raw ballot data mostly comes in long or wide format. Both formats can be
transformed into Preflib format, which can then be converted to
`ternable`-friendly format using
[`dop_irv()`](https://numbats.github.io/prefviz/reference/dop_irv.md).

**Processing long format**

Consider the following example of simulated ballot data in long format
for the Melbourne electorate, with 3 parties: ALP, LNP, and Other, and a
typical column for preference rank.

| ballot_id | elect_division | party | preference_rank |
|----------:|:---------------|:------|----------------:|
|         1 | Melbourne      | ALP   |               1 |
|         1 | Melbourne      | LNP   |               2 |
|         1 | Melbourne      | Other |               3 |
|         2 | Melbourne      | ALP   |               2 |
|         2 | Melbourne      | LNP   |               1 |
|         2 | Melbourne      | Other |               3 |
|         3 | Melbourne      | ALP   |               3 |
|         3 | Melbourne      | LNP   |               2 |
|         3 | Melbourne      | Other |               1 |
|         4 | Melbourne      | ALP   |               1 |
|         4 | Melbourne      | LNP   |               3 |
|         4 | Melbourne      | Other |               2 |
|         5 | Melbourne      | ALP   |               2 |
|         5 | Melbourne      | LNP   |               3 |
|         5 | Melbourne      | Other |               1 |

``` r
# Convert to PrefLib format
preflib_long <- prefio::long_preferences(
  ballot_long,
  vote,
  id_cols = c(ballot_id, elect_division),
  item_col = party,
  rank_col = preference_rank
)

# Convert to ternable-friendly format
dop_irv(preflib_long$vote, value_type = "percentage")
#> # A tibble: 2 × 5
#>   round   ALP   LNP Other winner
#>   <int> <dbl> <dbl> <dbl> <chr> 
#> 1     1   0.4   0.2   0.4 ALP   
#> 2     2   0.6   0     0.4 ALP
```

**Processing wide format**

Consider the same example, but in wide format, with each column
representing an item/candidate and its value representing the preference
rank.

| ballot_id | elect_division | ALP | LNP | Other |
|----------:|:---------------|----:|----:|------:|
|         1 | Melbourne      |   1 |   2 |     3 |
|         2 | Melbourne      |   2 |   1 |     3 |
|         3 | Melbourne      |   3 |   2 |     1 |
|         4 | Melbourne      |   1 |   3 |     2 |
|         5 | Melbourne      |   2 |   3 |     1 |

``` r
# Convert to PrefLib format
preflib_wide <- prefio::wide_preferences(ballot_wide, vote, ALP:Other)

# Convert to ternable-friendly format
dop_irv(preflib_wide$vote, value_type = "percentage")
#> # A tibble: 2 × 5
#>   round   ALP   LNP Other winner
#>   <int> <dbl> <dbl> <dbl> <chr> 
#> 1     1   0.4   0.2   0.4 ALP   
#> 2     2   0.6   0     0.4 ALP
```

## AEC distribution of preferences

This section applies specifically to the [AEC distribution of
preferences](https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv)
data. However, it can also be adapted to datasets of similar format,
where

- Each row represents an aggregated preference count and/or percentage
  for a specific candidate in a specific group/electorate.
- There must be at least a primary key column that uniquely identifies
  each row, a value column representing the preference count/percentage,
  and an item column representing the items/parties.
- There may also be a column representing the electoral status of the
  item/party (elected or not elected). This is optional.

In this example, we will work with 2025 Distribution of Preference data,
which is included in this package as `aecdop_2025`. We are interested in
preference percentages of 4 main parties: Labor, The Coalition, Greens,
and Independents. The rest is grouped into Other.

Before transforming the data, let’s do some data wrangling to filter for
relevant information.

``` r
# only include preference percentage and parties of interest

aecdop_2025 <- aecdop_2025 |> 
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    # all parties not in the main parties are grouped into "Other"
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other", 
    # group all parties in the Coalition into "LNP"
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))
```

The data is now ready for transformation, with the following
characteristics:

- `DivisionNm` and `CountNumber` columns together form the primary key
  that uniquely identifies each row.
- `CalculationValue` column contains the preference percentage.
- `Party` column contains the item/party.
- `Elected` column contains the elected/electoral status of the party.

We will use the
[`dop_transform()`](https://numbats.github.io/prefviz/reference/dop_transform.md)
function to transform the data into a `ternable`-friendly format.

``` r
dop_transform(
  data = aecdop_2025,
  key_cols = c(DivisionNm, CountNumber),
  value_col = CalculationValue,
  item_col = Party,
  winner_col = Elected,
  winner_identifier = "Y"
)
#> # A tibble: 976 × 8
#>    DivisionNm CountNumber   ALP   GRN   LNP  Other    IND Winner
#>    <chr>            <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <chr> 
#>  1 Adelaide             0 0.465 0.190 0.242 0.104  0      ALP   
#>  2 Adelaide             1 0.467 0.194 0.242 0.0967 0      ALP   
#>  3 Adelaide             2 0.476 0.205 0.244 0.0745 0      ALP   
#>  4 Adelaide             3 0.483 0.210 0.249 0.0578 0      ALP   
#>  5 Adelaide             4 0.493 0.222 0.285 0      0      ALP   
#>  6 Adelaide             5 0.691 0     0.309 0      0      ALP   
#>  7 Aston                0 0.373 0     0.377 0.209  0.0414 ALP   
#>  8 Aston                1 0.373 0     0.378 0.206  0.0434 ALP   
#>  9 Aston                2 0.376 0     0.380 0.210  0.0338 ALP   
#> 10 Aston                3 0.378 0     0.384 0.202  0.0358 ALP   
#> # ℹ 966 more rows
```
