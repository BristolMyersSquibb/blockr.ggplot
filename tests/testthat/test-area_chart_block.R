test_that("area_chart_block constructor", {
  # Test basic constructor
  blk <- new_area_chart_block()
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_area_chart_block(x = "Date", y = "value")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_area_chart_block(
    x = "Date", 
    y = "value", 
    fill = "category", 
    position = "fill",
    alpha = 0.8
  )
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("area_chart_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_area_chart_block(character(0), character(0))
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test required parameters only
  blk <- new_area_chart_block("Date", "value")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test with fill aesthetic
  blk <- new_area_chart_block("Date", "value", fill = "category")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test different position values
  blk <- new_area_chart_block("Date", "value", position = "stack")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("area_chart_block position parameter", {
  # Test valid position values
  blk <- new_area_chart_block(position = "stack")
  expect_s3_class(blk, "area_chart_block")
  
  blk <- new_area_chart_block(position = "fill")
  expect_s3_class(blk, "area_chart_block")
})

test_that("area_chart_block alpha parameter", {
  # Test various alpha values
  blk <- new_area_chart_block(alpha = 0.3)
  expect_s3_class(blk, "area_chart_block")
  
  blk <- new_area_chart_block(alpha = 1.0)
  expect_s3_class(blk, "area_chart_block")
})

test_that("area_chart_block structure", {
  blk <- new_area_chart_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "area_chart_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("area_chart_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_area_chart_block(x = "Date", y = "value")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("area_chart_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_area_chart_block(x = character(0), y = character(0))
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
  
  # Test with some empty, some filled
  blk <- new_area_chart_block(x = character(0), y = "value")
  expect_s3_class(blk, c("area_chart_block", "ggplot_block", "plot_block", "block"))
})