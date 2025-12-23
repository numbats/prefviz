# Helper 
valid_df <- data.frame(
  division = c("Div1", "Div2", "Div3", "Div4", "Div5"),
  ALP = c(0.40, 0.38, 0.52, 0.41, 0.35),
  LNP = c(0.30, 0.42, 0.28, 0.39, 0.45),
  GRN = c(0.12, 0.10, 0.15, 0.11, 0.08),
  Other = c(0.08, 0.10, 0.05, 0.09, 0.12)
)

test_that("ternable works with different selection styles", {
  # Column names
  expect_equal(
    ternable(valid_df, ALP:Other)$alternative_names,
    c("ALP", "LNP", "GRN", "Other")
  )

  # Column indices
  expect_equal(
    ternable(valid_df, c(2,4,5))$alternative_names,
    c("ALP", "GRN", "Other")
  )

  # Character vector
  expect_equal(
    ternable(valid_df, c("ALP", "LNP", "GRN"))$alternative_names,
    c("ALP", "LNP", "GRN")
  )

  # Tidyselect helpers 
  expect_equal(
    ternable(valid_df, where(is.numeric))$alternative_names,
    c("ALP", "LNP", "GRN", "Other")
  )

  expect_equal(
    ternable(valid_df, -c(division, GRN))$alternative_names,
    c("ALP", "LNP", "Other")
  )

  # Regex
  expect_equal(
    ternable(valid_df, matches("^[ALG]"))$alternative_names,
    c("ALP", "LNP", "GRN")
  )

  # Error when column not found
  expect_error(
    ternable(valid_df, c(ALP, LNP, Other, Extra)),
    "exist"
  )
  
  expect_error(
    ternable(valid_df, 3:7),
    "columns past the end"
  )

  # Return a ternable
  expect_s3_class(ternable(valid_df, ALP:Other), "ternable")
})

# Validator
test_that("ternable detects on numeric column", {
  expect_error(
    ternable(valid_df, 1:3),
    "All alternative columns must be numeric"
  )
}
)

test_that("ternable detects on negative values", {
  invalid_negative_df <- data.frame(
    division = c("Div1", "Div2", "Div3", "Div4", "Div5"),
    ALP = c(0.45, 0.38, -0.10, 0.41, 0.35),
    LNP = c(0.35, 0.42, 0.68, 0.39, 0.45),
    GRN = c(0.12, 0.10, 0.15, -0.05, 0.08),
    Other = c(0.08, 0.10, 0.27, 0.25, 0.12)
  )

  expect_error(
    ternable(invalid_negative_df, 2:5),
    "negative"
  )
})

test_that("ternable normalizes input that does not sum to 1",{
  invalid_sum_df <- data.frame(
    division = c("Div1", "Div2", "Div3", "Div4", "Div5"),
    ALP = c(0.32, 0.38, 0.82, 0.41, 0.49),
    LNP = c(0.35, 0.42, 0.12, 0.39, 0.45),
    GRN = c(0.12, 0.10, 0.15, 0.11, 0.08),
    Other = c(0.10, 0.15, 0.08, 0.12, 0.15)
  )

  expect_warning(
    ternable(invalid_sum_df, 2:5),
    "Not all rows sum to 1. Normalizing alternatives automatically."
  )
}
)
