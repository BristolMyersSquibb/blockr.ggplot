test_that("bar_chart_block constructor", {
  # Test basic constructor
  blk <- new_bar_chart_block()
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_bar_chart_block(x = "cyl", y = "mpg")
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_bar_chart_block(
    x = "cyl", 
    y = "mpg", 
    fill = "gear", 
    position = "dodge",
    flip_coords = TRUE
  )
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("bar_chart_block position parameter", {
  # Test valid position values
  blk <- new_bar_chart_block(position = "stack")
  expect_s3_class(blk, "bar_chart_block")
  
  blk <- new_bar_chart_block(position = "dodge")
  expect_s3_class(blk, "bar_chart_block")
  
  blk <- new_bar_chart_block(position = "fill")
  expect_s3_class(blk, "bar_chart_block")
})

test_that("bar_chart_block with various parameter inputs", {
  # Test empty parameters (count plot)
  blk <- new_bar_chart_block(character(0))
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test x-only (count by category)
  blk <- new_bar_chart_block("cyl")
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test x and y (column chart)
  blk <- new_bar_chart_block("cyl", "mpg")
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test with grouping
  blk <- new_bar_chart_block("cyl", fill = "gear")
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("bar_chart_block structure", {
  blk <- new_bar_chart_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "bar_chart_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("bar_chart_block flip_coords parameter", {
  # Test flip_coords = FALSE (default)
  blk <- new_bar_chart_block(flip_coords = FALSE)
  expect_s3_class(blk, "bar_chart_block")
  
  # Test flip_coords = TRUE (horizontal bars)
  blk <- new_bar_chart_block(flip_coords = TRUE)
  expect_s3_class(blk, "bar_chart_block")
})

test_that("bar_chart_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_bar_chart_block(x = "cyl")
  expect_s3_class(blk, c("bar_chart_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})
