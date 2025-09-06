test_that("chart_block constructor", {
  # Test basic constructor
  blk <- new_chart_block()
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with type parameter
  blk <- new_chart_block(type = "bar")
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with x and y
  blk <- new_chart_block(type = "point", x = "col1", y = "col2")
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with all aesthetics
  blk <- new_chart_block(
    type = "point",
    x = "col1",
    y = "col2",
    color = "col3",
    fill = "col4",
    size = "col5",
    shape = "col6"
  )
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("chart_block supports all chart types", {
  chart_types <- c(
    "point",
    "bar",
    "line",
    "boxplot",
    "violin",
    "density",
    "area",
    "histogram"
  )

  for (type in chart_types) {
    blk <- new_chart_block(type = type)
    expect_s3_class(
      blk,
      c("chart_block", "ggplot_block", "plot_block", "block")
    )
  }
})

test_that("chart_block handles empty inputs", {
  # Test with character(0) inputs
  blk <- new_chart_block(
    x = character(),
    y = character(),
    color = character()
  )
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))
})

test_that("chart_block with specific options", {
  # Test bar chart with position
  blk <- new_chart_block(type = "bar", position = "dodge")
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))

  # Test histogram with bins
  blk <- new_chart_block(type = "histogram", bins = 50)
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))

  # Test with alpha
  blk <- new_chart_block(alpha = 0.5)
  expect_s3_class(blk, c("chart_block", "ggplot_block", "plot_block", "block"))
})
