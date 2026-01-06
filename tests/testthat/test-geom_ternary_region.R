test_data <- data.frame(
  x = runif(10),
  y = runif(10),
  category = sample(c("A", "B", "C"), 10, replace = TRUE),
  value = rnorm(10)
)

#-----Warnings when not using after_stat() for aesthetic mappings----
test_that("No warning when using after_stat() correctly", {
  expect_no_warning(
    ggplot2::ggplot(test_data) + geom_ternary_region(ggplot2::aes(fill = ggplot2::after_stat(vertex_labels)))
  )
  
  expect_no_warning(
    ggplot2::ggplot(test_data) + geom_ternary_region(ggplot2::aes(color = ggplot2::after_stat(vertex_labels)))
  )
})

test_that("Warning when mapping fill to column without after_stat()", {
  expect_warning(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(fill = category)),
    regexp = "mapped to column from input data"
  )
})

test_that("Warning when mapping color to column without after_stat()", {
  expect_warning(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(colour = category)),
    regexp = "mapped to column from input data"
  )
})

test_that("Warning when mapping alpha to column without after_stat()", {
  expect_warning(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(alpha = value)),
    regexp = "mapped to column from input data"
  )
})

test_that("Warning when mapping group to column without after_stat()", {
  expect_warning(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(group = category)),
    regexp = "mapped to column from input data"
  )
})

test_that("Multiple warnings for multiple incorrect aesthetics", {  
  warnings <- capture_warnings(
    ggplot2::ggplot(test_data) + geom_ternary_region(ggplot2::aes(fill = category, color = category))
  )
  
  # Both fill and color should be present
  expect_true(any(grepl("'fill'", warnings)))
  expect_true(any(grepl("'color'|'colour'", warnings)))
  
  # At least 2 warnings
  expect_true(length(warnings) >= 2)
})

test_that("No warning for NULL or missing mapping", {
  expect_no_warning(ggplot2::ggplot(test_data) + geom_ternary_region())
  expect_no_warning(ggplot2::ggplot(test_data) + geom_ternary_region(mapping = NULL))
})

test_that("No warning for fixed aesthetics", {
  expect_no_warning(
    ggplot2::ggplot(test_data) + geom_ternary_region(fill = "red", color = "blue", alpha = 0.5)
  )
})

test_that("No warning for functions inside after_stat()", {
  expect_no_warning(
    ggplot2::ggplot(test_data) + geom_ternary_region(ggplot2::aes(fill = after_stat(factor(vertex_labels))))
  )
})

test_that("Warning for functions without after_stat()", {
  expect_warning(
    ggplot2::ggplot(test_data) + geom_ternary_region(ggplot2::aes(fill = factor(category))),
    regexp = "aesthetic 'fill' is mapped to column"
  )
})

test_that("Warnings only for incorrect mappings in mixed usage", {
  expect_warning(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(fill = category, color = ggplot2::after_stat(vertex_labels))),
    regexp = "aesthetic 'fill' is mapped"
  )
  
  # Should NOT warn about color
  warnings <- capture_warnings(
    ggplot2::ggplot(test_data) +
      geom_ternary_region(ggplot2::aes(fill = category, color = ggplot2::after_stat(vertex_labels)))
  )
  
  expect_false(any(grepl("'color'.*after_stat", warnings)))
})

#-----Warnings when there is no vertex_labels-----

test_that("Message when using after_stat() without vertex_labels", {
  # Test for fill ggplot2::aesthetic
  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(fill = ggplot2::after_stat(vertex_labels))),
    regexp = "You've mapped aesthetics using after_stat\\(\\), but 'vertex_labels' is NULL"
  )

  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(colour = ggplot2::after_stat(vertex_labels))),
    regexp = "provide the 'vertex_labels' argument"
  )
})

test_that("No message when vertex_labels is provided with after_stat()", {
  expect_no_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(
        ggplot2::aes(fill = ggplot2::after_stat(vertex_labels)),
        vertex_labels = c("A", "B", "C")
      )
  )
})

test_that("No message when mapping without after_stat()", {
  expect_no_message(
    suppressWarnings(
      ggplot2::ggplot(test_data) + 
        geom_ternary_region(ggplot2::aes(fill = category))
    )
  )
})

test_that("Message for fill aesthetic with after_stat() and no vertex_labels", {
  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(fill = ggplot2::after_stat(vertex_labels))),
    regexp = "You've mapped aesthetics using after_stat"
  )
})

test_that("Message for colour aesthetic with after_stat() and no vertex_labels", {
  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(colour = ggplot2::after_stat(vertex_labels))),
    regexp = "You've mapped aesthetics using after_stat"
  )
})

test_that("Message for alpha aesthetic with after_stat() and no vertex_labels", {
  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(alpha = ggplot2::after_stat(vertex_labels))),
    regexp = "You've mapped aesthetics using after_stat"
  )
})

test_that("Message for group aesthetic with after_stat() and no vertex_labels", {
  expect_message(
    ggplot2::ggplot(test_data) + 
      geom_ternary_region(ggplot2::aes(group = ggplot2::after_stat(vertex_labels))),
    regexp = "You've mapped aesthetics using after_stat"
  )
})
