test_that("facet_block constructor", {
  # Test basic constructor
  blk <- new_facet_block()
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test constructor with facet_type parameter
  blk <- new_facet_block(facet_type = "grid")
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test constructor with facets parameter (for wrap)
  blk <- new_facet_block(facet_type = "wrap", facets = "category")
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test constructor with rows and cols (for grid)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = "group",
    cols = "type"
  )
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))
})

test_that("facet_block supports both facet types", {
  facet_types <- c("wrap", "grid")

  for (type in facet_types) {
    blk <- new_facet_block(facet_type = type)
    expect_s3_class(
      blk,
      c("facet_block", "ggplot_transform_block", "transform_block", "block")
    )
  }
})

test_that("facet_block handles empty inputs", {
  # Test with character(0) inputs
  blk <- new_facet_block(
    facets = character(),
    rows = character(),
    cols = character()
  )
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))
})

test_that("facet_block with layout options", {
  # Test with ncol/nrow for wrap
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = "category",
    ncol = "2",
    nrow = "3"
  )
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test with scales option
  blk <- new_facet_block(scales = "free")
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test with labeller option
  blk <- new_facet_block(labeller = "label_both")
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))
})

test_that("facet_block with all options", {
  # Test wrap with all options
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = c("var1", "var2"),
    ncol = "3",
    nrow = "",
    scales = "free_x",
    labeller = "label_both",
    dir = "v"
  )
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))

  # Test grid with all options
  blk <- new_facet_block(
    facet_type = "grid",
    rows = "row_var",
    cols = "col_var",
    scales = "free_y",
    labeller = "label_parsed",
    space = "free_x"
  )
  expect_s3_class(blk, c("facet_block", "ggplot_transform_block", "transform_block", "block"))
})
