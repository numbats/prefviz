test_that("matrix input acceptable", {
  mat <- matrix(c(0.5, 0.3, 0.2,
                  0.4, 0.4, 0.2,
                  0.6, 0.2, 0.2),
                  ncol = 3, byrow = TRUE)

  expect_no_error(helmert_transform(mat))
})

test_that("helmert_transform works with different selection styles", {
  df <- data.frame(
    electorate = c("A", "B", "C"),
    ALP = c(0.5, 0.4, 0.6),
    LNP = c(0.3, 0.4, 0.2),
    Other = c(0.2, 0.2, 0.2)
  )

  expect_no_error(helmert_transform(df, ALP:Other))
  expect_no_error(helmert_transform(df, c(ALP, LNP, Other)))
  expect_no_error(helmert_transform(df, c("ALP", "LNP", "Other")))
  expect_no_error(helmert_transform(df, where(is.numeric)))
  expect_no_error(helmert_transform(df, -c(electorate)))
  expect_no_error(helmert_transform(df, 2:4))

  # Error when column not found
  expect_error(
    helmert_transform(df, c(ALP, LNP, Other, Extra)),
    "exist"
  )
})

