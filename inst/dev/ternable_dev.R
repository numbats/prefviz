pref_2022 <- read_csv("inst/dev/pref_2022.csv")

# Validate input
tern_object <- function(data, alternatives = everything()) {
  stopifnot(is.data.frame(data))
  
  alternative_col_ind <- tidyselect::eval_select(
      rlang::enquo(alternatives), 
      data)
  alternative_col_chr <- colnames(data)[alternative_col_ind]

  return(alternative_col_ind)
}

# All column selection automatically handled

alt_data <- pref_2022 |> select(ALP, LNP, Other)
tern_object(pref_2022, c(ALP, DivisionNm))
