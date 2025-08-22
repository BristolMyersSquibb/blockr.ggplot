test_that("boxplot_block constructor", {
  # Test basic constructor
  blk <- new_boxplot_block()
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_boxplot_block(x = "cyl", y = "mpg")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_boxplot_block(
    x = "cyl", 
    y = "mpg", 
    color = "gear", 
    fill = "am",
    show_outliers = FALSE
  )
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("boxplot_block with various parameter inputs", {
  # Test single variable boxplot (no grouping)
  blk <- new_boxplot_block(y = "mpg")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test grouped boxplot
  blk <- new_boxplot_block(x = "cyl", y = "mpg")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with color aesthetic
  blk <- new_boxplot_block(x = "cyl", y = "mpg", color = "gear")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with fill aesthetic
  blk <- new_boxplot_block(x = "cyl", y = "mpg", fill = "am")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("boxplot_block show_outliers parameter", {
  # Test show_outliers = TRUE (default)
  blk <- new_boxplot_block(show_outliers = TRUE)
  expect_s3_class(blk, "boxplot_block")
  
  # Test show_outliers = FALSE
  blk <- new_boxplot_block(show_outliers = FALSE)
  expect_s3_class(blk, "boxplot_block")
})

test_that("boxplot_block structure", {
  blk <- new_boxplot_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "boxplot_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("boxplot_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_boxplot_block(y = "mpg")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("boxplot_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_boxplot_block(x = character(0), y = character(0))
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with some empty, some filled
  blk <- new_boxplot_block(x = character(0), y = "mpg")
  expect_s3_class(blk, c("boxplot_block", "ggplot_block", "plot_block", "block"))
})
