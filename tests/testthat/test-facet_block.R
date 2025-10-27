test_that("facet_block constructor", {
  # Test basic constructor
  blk <- new_facet_block()
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with facet_type parameter
  blk <- new_facet_block(facet_type = "grid")
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with facets parameter (for wrap)
  blk <- new_facet_block(facet_type = "wrap", facets = "category")
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with rows and cols (for grid)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = "group",
    cols = "type"
  )
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )
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
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("facet_block with layout options", {
  # Test with ncol/nrow for wrap
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = "category",
    ncol = "2",
    nrow = "3"
  )
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test with scales option
  blk <- new_facet_block(scales = "free")
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test with labeller option
  blk <- new_facet_block(labeller = "label_both")
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )
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
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test grid with all options
  blk <- new_facet_block(
    facet_type = "grid",
    rows = "row_var",
    cols = "col_var",
    scales = "free_y",
    labeller = "label_parsed",
    space = "free_x"
  )
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )
})

# Server-side tests
test_that("facet_block server input widgets update reactive values", {
  # Create a sample ggplot object
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block()$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Test facet_type input
      session$setInputs(facet_type = "grid")
      expect_equal(facet_type(), "grid")

      session$setInputs(facet_type = "wrap")
      expect_equal(facet_type(), "wrap")

      # Test facets input (for wrap)
      session$setInputs(facets = "cyl")
      expect_equal(facets(), "cyl")

      # Test rows input (for grid)
      session$setInputs(rows = "gear")
      expect_equal(rows(), "gear")

      # Test cols input (for grid)
      session$setInputs(cols = "carb")
      expect_equal(cols(), "carb")

      # Test scales input
      session$setInputs(scales = "free_x")
      expect_equal(scales(), "free_x")

      # Test labeller input
      session$setInputs(labeller = "label_both")
      expect_equal(labeller(), "label_both")
    }
  )
})

test_that("facet_block state is correctly returned", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block()$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Check default values
      expect_equal(session$returned$state$facet_type(), "wrap")
      expect_equal(session$returned$state$scales(), "fixed")
      expect_equal(session$returned$state$labeller(), "label_value")

      # Update inputs and check state is updated
      session$setInputs(facet_type = "grid")
      expect_equal(session$returned$state$facet_type(), "grid")

      session$setInputs(scales = "free")
      expect_equal(session$returned$state$scales(), "free")

      session$setInputs(rows = "cyl")
      expect_equal(session$returned$state$rows(), "cyl")

      session$setInputs(cols = "gear")
      expect_equal(session$returned$state$cols(), "gear")
    }
  )
})

test_that("facet_block expr evaluates correctly with facet_wrap", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(facet_type = "wrap", facets = "cyl")$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")

      # Check that faceting was applied
      expect_true("FacetWrap" %in% class(evaluated_expr$facet))
    }
  )
})

test_that("facet_block expr evaluates correctly with facet_grid", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(
      facet_type = "grid",
      rows = "cyl",
      cols = "gear"
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")

      # Check that faceting was applied
      expect_true("FacetGrid" %in% class(evaluated_expr$facet))
    }
  )
})

test_that("facet_block expr evaluates correctly with multiple facets in wrap", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(
      facet_type = "wrap",
      facets = c("cyl", "gear")
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")

      # Check that faceting was applied
      expect_true("FacetWrap" %in% class(evaluated_expr$facet))
    }
  )
})

test_that("facet_block expr evaluates correctly with scales option", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      scales = "free"
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("facet_block expr evaluates correctly with ncol/nrow options", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      ncol = "2"
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("facet_block handles empty facets gracefully", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_facet_block(
      facet_type = "wrap",
      facets = character()
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression - should return original plot
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})
