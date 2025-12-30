test_data <- data.frame(
  x = runif(10),
  y = runif(10),
  category = sample(c("A", "B", "C"), 10, replace = TRUE),
  value = rnorm(10)
)

test_that("No warning when using after_stat() correctly", {
  expect_no_warning(
    ggplot() + geom_ternary_region(aes(fill = after_stat(vertex_labels)))
  )
  
  expect_no_warning(
    ggplot() + geom_ternary_region(aes(color = after_stat(vertex_labels)))
  )
})

test_that("Warning when mapping fill to column without after_stat()", {
  expect_warning(
    ggplot(test_data) +
      geom_ternary_region(aes(fill = category)),
    regexp = "aesthetic 'fill' is mapped"
  )
})

test_that("Warning when mapping color to column without after_stat()", {
  expect_warning(
    ggplot(test_data) +
      geom_ternary_region(aes(colour = category)),
    regexp = "aesthetic 'colour' is mapped"
  )
})

test_that("Warning when mapping alpha to column without after_stat()", {
  expect_warning(
    ggplot(test_data) +
      geom_ternary_region(aes(alpha = value)),
    regexp = "aesthetic 'alpha' is mapped"
  )
})

test_that("Warning when mapping group to column without after_stat()", {
  expect_warning(
    ggplot(test_data) +
      geom_ternary_region(aes(group = category)),
    regexp = "aesthetic 'group' is mapped"
  )
})

test_that("Multiple warnings for multiple incorrect aesthetics", {  
  warnings <- capture_warnings(
    ggplot(test_data) + geom_ternary_region(aes(fill = category, color = category))
  )
  
  # Both fill and color should be present
  expect_true(any(grepl("'fill'", warnings)))
  expect_true(any(grepl("'color'|'colour'", warnings)))
  
  # At least 2 warnings
  expect_true(length(warnings) >= 2)
})

test_that("No warning for NULL or missing mapping", {
  expect_no_warning(ggplot() + geom_ternary_region())
  expect_no_warning(ggplot() + geom_ternary_region(mapping = NULL))
})

test_that("No warning for fixed aesthetics", {
  expect_no_warning(
    ggplot() + geom_ternary_region(fill = "red", color = "blue", alpha = 0.5)
  )
})

test_that("No warning for functions inside after_stat()", {
  expect_no_warning(
    ggplot() + geom_ternary_region(aes(fill = after_stat(factor(vertex_labels))))
  )
})

test_that("Warning for functions without after_stat()", {
  expect_warning(
    ggplot(test_data) + geom_ternary_region(aes(fill = factor(category))),
    regexp = "aesthetic 'fill' is mapped to column"
  )
})

test_that("Warnings only for incorrect mappings in mixed usage", {
  expect_warning(
    ggplot(test_data) +
      geom_ternary_region(aes(fill = category, color = after_stat(vertex_labels))),
    regexp = "aesthetic 'fill' is mapped"
  )
  
  # Should NOT warn about color
  warnings <- capture_warnings(
    ggplot(test_data) +
      geom_ternary_region(aes(fill = category, color = after_stat(vertex_labels)))
  )
  
  expect_false(any(grepl("'color'.*after_stat", warnings)))
})