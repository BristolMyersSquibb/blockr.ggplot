test_that("heatmap_block constructor", {
  # Test basic constructor
  blk <- new_heatmap_block()
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # Test constructor with parameters
  blk <- new_heatmap_block(x = "row", y = "col", fill = "value")
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # Test constructor with all parameters
  blk <- new_heatmap_block(
    x = "row",
    y = "col",
    fill = "value",
    show_values = TRUE,
    color_palette = "plasma"
  )
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )
})

test_that("heatmap_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_heatmap_block(character(0), character(0), character(0))
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # Test required parameters
  blk <- new_heatmap_block("row", "col", "value")
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # Test with different combinations
  blk <- new_heatmap_block("x", "y", "z")
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )
})

test_that("heatmap_block color_palette parameter", {
  # Test valid color palette values
  blk <- new_heatmap_block(color_palette = "viridis")
  expect_s3_class(blk, "heatmap_block")

  blk <- new_heatmap_block(color_palette = "plasma")
  expect_s3_class(blk, "heatmap_block")

  blk <- new_heatmap_block(color_palette = "inferno")
  expect_s3_class(blk, "heatmap_block")

  blk <- new_heatmap_block(color_palette = "magma")
  expect_s3_class(blk, "heatmap_block")

  blk <- new_heatmap_block(color_palette = "blues")
  expect_s3_class(blk, "heatmap_block")
})

test_that("heatmap_block show_values parameter", {
  # Test show_values = FALSE (default)
  blk <- new_heatmap_block(show_values = FALSE)
  expect_s3_class(blk, "heatmap_block")

  # Test show_values = TRUE
  blk <- new_heatmap_block(show_values = TRUE)
  expect_s3_class(blk, "heatmap_block")
})

test_that("heatmap_block structure", {
  blk <- new_heatmap_block()

  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))

  # Check class hierarchy
  expect_true(inherits(blk, "heatmap_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("heatmap_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Test that block can generate expressions
  blk <- new_heatmap_block(x = "row", y = "col", fill = "value")
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("heatmap_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_heatmap_block(
    x = character(0),
    y = character(0),
    fill = character(0)
  )
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )

  # Test with some empty, some filled
  blk <- new_heatmap_block(x = character(0), y = "col", fill = "value")
  expect_s3_class(
    blk,
    c("heatmap_block", "ggplot_block", "plot_block", "block")
  )
})
