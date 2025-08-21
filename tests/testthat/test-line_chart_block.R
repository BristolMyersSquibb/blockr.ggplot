test_that("line_chart_block constructor", {
  # Test basic constructor
  blk <- new_line_chart_block()
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_line_chart_block(x = "wt", y = "mpg")
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_line_chart_block(
    x = "wt", 
    y = "mpg", 
    color = "cyl", 
    linetype = "gear",
    size = 1.5,
    show_points = FALSE
  )
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("line_chart_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_line_chart_block(character(0), character(0))
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test required parameters only
  blk <- new_line_chart_block("wt", "mpg")
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test with color grouping
  blk <- new_line_chart_block("wt", "mpg", color = "cyl")
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test with linetype grouping
  blk <- new_line_chart_block("wt", "mpg", linetype = "gear")
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("line_chart_block size parameter", {
  # Test various size values
  blk <- new_line_chart_block(size = 0.5)
  expect_s3_class(blk, "line_chart_block")
  
  blk <- new_line_chart_block(size = 1.0)
  expect_s3_class(blk, "line_chart_block")
  
  blk <- new_line_chart_block(size = 2.0)
  expect_s3_class(blk, "line_chart_block")
})

test_that("line_chart_block show_points parameter", {
  # Test show_points = TRUE (default)
  blk <- new_line_chart_block(show_points = TRUE)
  expect_s3_class(blk, "line_chart_block")
  
  # Test show_points = FALSE (line only)
  blk <- new_line_chart_block(show_points = FALSE)
  expect_s3_class(blk, "line_chart_block")
})

test_that("line_chart_block structure", {
  blk <- new_line_chart_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "line_chart_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("line_chart_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_line_chart_block(x = "wt", y = "mpg")
  expect_s3_class(blk, c("line_chart_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})