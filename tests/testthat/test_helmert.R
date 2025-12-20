test_that("helmert_transform rejects invalid input types", {
  # Test with vector
  expect_error(
    helmert_transform(c(0.5, 0.3, 0.2)),
    "Input must be a matrix or a data frame"
  )
  
  # Test with list
  expect_error(
    helmert_transform(list(a = 1, b = 2)),
    "Input must be a matrix or a data frame"
  )
})

test_that("helmert_transform handles invalid alternatives",{
  mat <- matrix(c(0.5, 0.3, 0.2,
                  0.4, 0.4, 0.2,
                  0.6, 0.2, 0.5),
                ncol = 3, byrow = TRUE)
  colnames(mat) <- c("A", "B", "C")
  
  # Error when alternatives is not a character or numeric vector
  expect_error(
    helmert_transform(mat, alternatives = list(A = 1, B = 2)),
    "Alternatives must be a character vector or a numeric vector"
  )

  # Error when columns not found
  expect_error(
    helmert_transform(mat, alternatives = c("A", "B", "D")),
    "Columns not found in data: D"
  )
  
  # Error when column index out of range
  expect_error(
    helmert_transform(mat, alternatives = c(1, 5)),
    "Alternatives column indices must be between 1 and 3"
  )

  # Warning when rows do not sum to 1
  expect_warning(
    helmert_transform(mat),
    "Input rows do not sum to 1. Normalizing automatically."
  )

  # Check output
  res <- helmert_transform(mat)
  expect_equal(ncol(res), 5) # 3 columns original + 2 columns of coordinates
  expect_s3_class(res, "data.frame")
})

test_that("helmert_transform rejects negative values", {
  mat <- matrix(c(0.5, 0.3, 0.2,
                  0.4, 0.4, 0.2,
                  -0.6, 0.2, 0.5),
                ncol = 3, byrow = TRUE)
  
  # Error when input contains negative values
  expect_error(
    helmert_transform(mat),
    "Input contains negative values. Compositional data must be non-negative."
  )
})
