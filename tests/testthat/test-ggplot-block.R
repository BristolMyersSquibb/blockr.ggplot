# Tests for new_ggplot_block - General constructor tests
# Following blockr.dplyr testing conventions

test_that("ggplot block constructor", {
  # Test basic constructor
  blk <- new_ggplot_block()
  expect_s3_class(blk, c("ggplot_block", "ggplot_transform_block", "block"))

  # Test constructor with parameters
  blk <- new_ggplot_block(type = "bar", x = "cyl", y = "mpg")
  expect_s3_class(blk, c("ggplot_block", "ggplot_transform_block", "block"))

  # Test constructor with all chart types
  chart_types <- c(
    "point", "bar", "line", "boxplot", "violin",
    "density", "area", "histogram", "pie"
  )
  for (ct in chart_types) {
    blk <- new_ggplot_block(type = ct, x = "cyl")
    expect_s3_class(blk, c("ggplot_block", "ggplot_transform_block", "block"))
  }
})
