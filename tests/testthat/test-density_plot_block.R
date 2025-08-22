test_that("density_plot_block constructor", {
  # Test basic constructor
  blk <- new_density_plot_block()
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_density_plot_block(x = "mpg", fill = "cyl")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_density_plot_block(
    x = "mpg", 
    fill = "cyl", 
    color = "gear",
    alpha = 0.7,
    adjust = 1.2
  )
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("density_plot_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_density_plot_block(character(0))
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test required parameters only
  blk <- new_density_plot_block("mpg")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with fill aesthetic
  blk <- new_density_plot_block("mpg", fill = "cyl")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with color aesthetic
  blk <- new_density_plot_block("mpg", color = "gear")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("density_plot_block alpha parameter", {
  # Test various alpha values
  blk <- new_density_plot_block(alpha = 0.2)
  expect_s3_class(blk, "density_plot_block")
  
  blk <- new_density_plot_block(alpha = 1.0)
  expect_s3_class(blk, "density_plot_block")
})

test_that("density_plot_block adjust parameter", {
  # Test various adjust values
  blk <- new_density_plot_block(adjust = 0.5)
  expect_s3_class(blk, "density_plot_block")
  
  blk <- new_density_plot_block(adjust = 2.0)
  expect_s3_class(blk, "density_plot_block")
})

test_that("density_plot_block structure", {
  blk <- new_density_plot_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "density_plot_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("density_plot_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_density_plot_block(x = "mpg")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("density_plot_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_density_plot_block(x = character(0))
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with some empty, some filled
  blk <- new_density_plot_block(x = character(0), fill = "cyl")
  expect_s3_class(blk, c("density_plot_block", "ggplot_block", "plot_block", "block"))
})
