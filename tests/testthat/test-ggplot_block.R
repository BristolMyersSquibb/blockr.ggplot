test_that("ggplot_block constructor", {
  # Test basic constructor
  blk <- new_ggplot_block()
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with type parameter
  blk <- new_ggplot_block(type = "bar")
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with x and y
  blk <- new_ggplot_block(type = "point", x = "col1", y = "col2")
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))

  # Test constructor with all aesthetics
  blk <- new_ggplot_block(
    type = "point",
    x = "col1",
    y = "col2",
    color = "col3",
    fill = "col4",
    size = "col5",
    shape = "col6"
  )
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("ggplot_block supports all chart types", {
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
    blk <- new_ggplot_block(type = type)
    expect_s3_class(
      blk,
      c("ggplot_block", "ggplot_block", "plot_block", "block")
    )
  }
})

test_that("ggplot_block handles empty inputs", {
  # Test with character(0) inputs
  blk <- new_ggplot_block(
    x = character(),
    y = character(),
    color = character()
  )
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("ggplot_block with specific options", {
  # Test bar chart with position
  blk <- new_ggplot_block(type = "bar", position = "dodge")
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))

  # Test histogram with bins
  blk <- new_ggplot_block(type = "histogram", bins = 50)
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))

  # Test with alpha
  blk <- new_ggplot_block(alpha = 0.5)
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

# Server-side tests
test_that("ggplot_block server input widgets update reactive values", {
  testServer(
    app = new_ggplot_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Test type input
      session$setInputs(type = "bar")
      expect_equal(type(), "bar")

      # Test x input
      session$setInputs(x = "mpg")
      expect_equal(x(), "mpg")

      # Test y input
      session$setInputs(y = "hp")
      expect_equal(y(), "hp")

      # Test color input
      session$setInputs(color = "cyl")
      expect_equal(color(), "cyl")

      # Test fill input
      session$setInputs(fill = "gear")
      expect_equal(fill(), "gear")

      # Test position input
      session$setInputs(position = "dodge")
      expect_equal(position(), "dodge")

      # Test bins input
      session$setInputs(bins = 20)
      expect_equal(bins(), 20)
    }
  )
})

test_that("ggplot_block state is correctly returned", {
  testServer(
    app = new_ggplot_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Check default values
      expect_equal(session$returned$state$type(), "point")
      expect_equal(session$returned$state$x(), character())
      expect_equal(session$returned$state$position(), "stack")
      expect_equal(session$returned$state$bins(), 30)
      expect_equal(session$returned$state$donut(), FALSE)

      # Update inputs and check state is updated
      session$setInputs(type = "histogram")
      expect_equal(session$returned$state$type(), "histogram")

      session$setInputs(x = "mpg")
      expect_equal(session$returned$state$x(), "mpg")

      session$setInputs(bins = 15)
      expect_equal(session$returned$state$bins(), 15)
    }
  )
})

test_that("ggplot_block expr evaluates correctly for point chart", {
  testServer(
    app = new_ggplot_block(type = "point", x = "mpg", y = "hp")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")

      # Check that the plot has the expected data
      expect_equal(nrow(evaluated_expr$data), nrow(mtcars))
    }
  )
})

test_that("ggplot_block expr evaluates correctly for bar chart", {
  testServer(
    app = new_ggplot_block(type = "bar")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(x = "cyl")

      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly for histogram", {
  testServer(
    app = new_ggplot_block(type = "histogram", x = "mpg", bins = 20)$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly with color aesthetic", {
  testServer(
    app = new_ggplot_block(type = "point", x = "mpg", y = "hp", color = "cyl")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly with fill aesthetic", {
  testServer(
    app = new_ggplot_block(type = "bar", x = "cyl", fill = "gear")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block handles empty optional aesthetics", {
  testServer(
    app = new_ggplot_block(
      type = "point",
      x = "mpg",
      y = "hp",
      color = character(),
      fill = character()
    )$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression - should work without optional aesthetics
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block handles (none) selections", {
  testServer(
    app = new_ggplot_block(type = "point")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(x = "mpg")
      session$setInputs(y = "hp")
      session$setInputs(color = "(none)")
      session$setInputs(fill = "(none)")

      # Evaluate the expression - should work with (none) selections
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly for line chart", {
  testServer(
    app = new_ggplot_block(type = "line", x = "mpg", y = "hp")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly for boxplot", {
  testServer(
    app = new_ggplot_block(type = "boxplot", x = "factor(cyl)", y = "mpg")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("ggplot_block expr evaluates correctly for density plot", {
  testServer(
    app = new_ggplot_block(type = "density", x = "mpg")$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})
