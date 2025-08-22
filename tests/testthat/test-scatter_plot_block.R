test_that("scatter_plot_block constructor", {
  # Test basic constructor
  blk <- new_scatter_plot_block()
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_scatter_plot_block(x = "wt", y = "mpg")
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_scatter_plot_block(
    x = "wt", 
    y = "mpg", 
    color = "cyl", 
    shape = "gear",
    size = "hp",
    alpha = 0.5, 
    add_smooth = TRUE
  )
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("scatter_plot_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_scatter_plot_block(character(0), character(0))
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test single parameters
  blk <- new_scatter_plot_block("wt", "mpg")
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with optional aesthetics
  blk <- new_scatter_plot_block("wt", "mpg", color = "cyl")
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("scatter_plot_block structure", {
  blk <- new_scatter_plot_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "scatter_plot_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("scatter_plot_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_scatter_plot_block(x = "wt", y = "mpg")
  expect_s3_class(blk, c("scatter_plot_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("scatter_plot_block parameter validation", {
  # Test various alpha values
  blk <- new_scatter_plot_block(alpha = 0.1)
  expect_s3_class(blk, "scatter_plot_block")
  
  blk <- new_scatter_plot_block(alpha = 1.0)
  expect_s3_class(blk, "scatter_plot_block")
  
  # Test add_smooth parameter
  blk <- new_scatter_plot_block(add_smooth = TRUE)
  expect_s3_class(blk, "scatter_plot_block")
  
  blk <- new_scatter_plot_block(add_smooth = FALSE)
  expect_s3_class(blk, "scatter_plot_block")
})
