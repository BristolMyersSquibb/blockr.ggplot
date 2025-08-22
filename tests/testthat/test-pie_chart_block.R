test_that("pie_chart_block constructor", {
  # Test basic constructor
  blk <- new_pie_chart_block()
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test constructor with parameters
  blk <- new_pie_chart_block(x = "category", y = "value")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test constructor with all parameters
  blk <- new_pie_chart_block(
    x = "category",
    y = "value",
    fill = "group",
    donut = TRUE,
    show_labels = FALSE
  )
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )
})

test_that("pie_chart_block with various parameter inputs", {
  # Test empty parameters
  blk <- new_pie_chart_block(character(0))
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test x-only (count by category)
  blk <- new_pie_chart_block("category")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test x and y (value-based pie)
  blk <- new_pie_chart_block("category", "value")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test with custom fill
  blk <- new_pie_chart_block("category", fill = "group")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )
})

test_that("pie_chart_block donut parameter", {
  # Test donut = FALSE (default pie)
  blk <- new_pie_chart_block(donut = FALSE)
  expect_s3_class(blk, "pie_chart_block")

  # Test donut = TRUE (donut chart)
  blk <- new_pie_chart_block(donut = TRUE)
  expect_s3_class(blk, "pie_chart_block")
})

test_that("pie_chart_block show_labels parameter", {
  # Test show_labels = TRUE (default)
  blk <- new_pie_chart_block(show_labels = TRUE)
  expect_s3_class(blk, "pie_chart_block")

  # Test show_labels = FALSE
  blk <- new_pie_chart_block(show_labels = FALSE)
  expect_s3_class(blk, "pie_chart_block")
})

test_that("pie_chart_block structure", {
  blk <- new_pie_chart_block()

  # Check that block has required components
  expect_true("expr_server" %in% names(blk))
  expect_true("expr_ui" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
  expect_true(is.function(blk[["expr_ui"]]))

  # Check class hierarchy
  expect_true(inherits(blk, "pie_chart_block"))
  expect_true(inherits(blk, "ggplot_block"))
  expect_true(inherits(blk, "plot_block"))
  expect_true(inherits(blk, "block"))
})

test_that("pie_chart_block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  # Test that block can generate expressions
  blk <- new_pie_chart_block(x = "category")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # The block should be properly structured for expression generation
  expect_true("expr_server" %in% names(blk))
  expect_true(is.function(blk[["expr_server"]]))
})

test_that("pie_chart_block handles empty parameters correctly", {
  # Test with character(0) inputs - should not fail
  blk <- new_pie_chart_block(x = character(0), y = character(0))
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )

  # Test with some empty, some filled
  blk <- new_pie_chart_block(x = character(0), y = "value")
  expect_s3_class(
    blk,
    c("pie_chart_block", "ggplot_block", "plot_block", "block")
  )
})
