library(tidyverse)
library(readxl)

## AEC DOP 2025 and 2022
url_2022 <- "https://results.aec.gov.au/27966/Website/Downloads/HouseDopByDivisionDownload-27966.csv"
aecdop_2022 <- read_csv(url_2022, skip = 1) 

url_2025 <- "https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv"
aecdop_2025 <- read_csv(url_2025, skip = 1) 

usethis::use_data(aecdop_2022, overwrite = TRUE)
usethis::use_data(aecdop_2025, overwrite = TRUE)