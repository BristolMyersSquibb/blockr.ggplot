test_that("violin_plot_block constructor", {
  # Test basic constructor
  blk <- new_violin_plot_block()
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_violin_plot_block(x = "cyl", y = "mpg")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_violin_plot_block(
    x = "cyl", 
    y = "mpg", 
    fill = "gear", 
    color = "am",
    trim = FALSE,
    scale = "count"
  )
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("violin_plot_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_violin_plot_block(character(0), character(0))
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test required parameters only
  blk <- new_violin_plot_block("cyl", "mpg")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with fill aesthetic
  blk <- new_violin_plot_block("cyl", "mpg", fill = "gear")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with color aesthetic
  blk <- new_violin_plot_block("cyl", "mpg", color = "am")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
})

test_that("violin_plot_block scale parameter", {
  # Test valid scale values
  blk <- new_violin_plot_block(scale = "area")
  expect_s3_class(blk, "violin_plot_block")
  
  blk <- new_violin_plot_block(scale = "count")
  expect_s3_class(blk, "violin_plot_block")
  
  blk <- new_violin_plot_block(scale = "width")
  expect_s3_class(blk, "violin_plot_block")
})

test_that("violin_plot_block trim parameter", {
  # Test trim = TRUE (default)
  blk <- new_violin_plot_block(trim = TRUE)
  expect_s3_class(blk, "violin_plot_block")
  
  # Test trim = FALSE
  blk <- new_violin_plot_block(trim = FALSE)
  expect_s3_class(blk, "violin_plot_block")
})

test_that("violin_plot_block structure", {
  blk <- new_violin_plot_block()
  
  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))
  
  # Check class hierarchy
  expect_true(inherits(blk, "violin_plot_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("violin_plot_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  
  # Test that block can generate expressions
  blk <- new_violin_plot_block(x = "cyl", y = "mpg")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("violin_plot_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_violin_plot_block(x = character(0), y = character(0))
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
  
  # Test with some empty, some filled
  blk <- new_violin_plot_block(x = character(0), y = "mpg")
  expect_s3_class(blk, c("violin_plot_block", "ggplot_block", "plot_block", "block"))
})
