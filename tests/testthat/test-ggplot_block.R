library(testthat)
library(blockr.ggplot)
library(shiny)

# ==============================================================================
# LAYER 1: Unit Tests (Pure Functions, No Shiny)
# ==============================================================================

test_that("ggplot_block constructor creates valid block", {
  blk <- new_ggplot_block()

  # Check class hierarchy
  expect_s3_class(blk, "ggplot_block")
  expect_s3_class(blk, "ggplot_transform_block")
  expect_s3_class(blk, "block")

  # Check has required components
  expect_true(is.function(blk$expr_server))
  expect_true(is.function(blk$expr_ui))
})

test_that("ggplot_block constructor accepts all parameters", {
  blk <- new_ggplot_block(
    type = "bar",
    x = "mpg",
    y = "hp",
    color = "cyl",
    fill = "gear",
    size = "wt",
    shape = "am",
    linetype = "vs",
    group = "carb",
    alpha = "qsec",
    density_alpha = 0.5,
    position = "dodge",
    bins = 20,
    donut = TRUE
  )

  expect_s3_class(blk, "ggplot_block")
})

test_that("ggplot_block has correct allow_empty_state", {
  blk <- new_ggplot_block()

  expect_true("y" %in% blk$allow_empty_state)
  expect_true("color" %in% blk$allow_empty_state)
  expect_true("fill" %in% blk$allow_empty_state)
  expect_true("size" %in% blk$allow_empty_state)
  expect_true("shape" %in% blk$allow_empty_state)
  expect_true("linetype" %in% blk$allow_empty_state)
  expect_true("group" %in% blk$allow_empty_state)
  expect_true("alpha" %in% blk$allow_empty_state)
})

# ==============================================================================
# LAYER 2: testServer (ALL Shiny Interactions)
# ==============================================================================

# -----------------------------------------------------------------------------
# Pattern 1: Testing Expression Generation (expr_server)
# -----------------------------------------------------------------------------

test_that("ggplot_block generates scatter plot expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression structure
      expr_result <- result$expr()
      expr_str <- deparse(expr_result)
      expr_str <- paste0(expr_str, collapse = "")
      expect_true(grepl("ggplot2::ggplot", expr_str))
      expect_true(grepl("geom_point", expr_str))
      expect_true(grepl("x = mpg", expr_str))
      expect_true(grepl("y = hp", expr_str))
      expect_true(grepl("theme_minimal", expr_str))

      # Test state
      expect_true(is.reactive(result$state$type))
      expect_equal(result$state$type(), "point")
      expect_equal(result$state$x(), "mpg")
      expect_equal(result$state$y(), "hp")
    }
  )
})

test_that("ggplot_block generates bar chart expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "bar", x = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_bar", expr_str))
      expect_true(grepl("x = cyl", expr_str))
      expect_equal(session$returned$state$type(), "bar")
    }
  )
})

test_that("ggplot_block generates histogram expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "histogram", x = "mpg", bins = 20)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_histogram", expr_str))
      expect_true(grepl("bins = 20", expr_str))
      expect_equal(session$returned$state$bins(), 20)
    }
  )
})

test_that("ggplot_block generates line chart expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "line", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_line", expr_str))
    }
  )
})

test_that("ggplot_block generates boxplot expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "boxplot", x = "cyl", y = "mpg")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_boxplot", expr_str))
    }
  )
})

test_that("ggplot_block generates violin plot expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "violin", x = "cyl", y = "mpg")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_violin", expr_str))
    }
  )
})

test_that("ggplot_block generates density plot expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "density", x = "mpg", density_alpha = 0.6)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_density", expr_str))
      expect_true(grepl("alpha = 0.6", expr_str))
    }
  )
})

test_that("ggplot_block generates area chart expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "area", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_area", expr_str))
    }
  )
})

test_that("ggplot_block generates pie chart expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "pie", x = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_bar|geom_col", expr_str))
      expect_true(grepl("coord_polar", expr_str))
    }
  )
})

test_that("ggplot_block handles optional aesthetics", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(
    type = "point",
    x = "mpg",
    y = "hp",
    color = "cyl",
    size = "wt"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("colour = cyl", expr_str))
      expect_true(grepl("size = wt", expr_str))
    }
  )
})

test_that("ggplot_block handles empty aesthetics with (none)", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(
    type = "point",
    x = "mpg",
    y = character() # Empty y
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Should have y = "(none)" in state
      expect_equal(session$returned$state$y(), "(none)")
    }
  )
})

test_that("ggplot_block converts fill to factor for histograms", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "histogram", x = "mpg", fill = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("as.factor\\(cyl\\)", expr_str))
    }
  )
})

test_that("ggplot_block converts shape to factor", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp", shape = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("as.factor\\(cyl\\)", expr_str))
    }
  )
})

test_that("ggplot_block handles density plot with fill and group", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "density", x = "mpg", fill = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      # Should have both fill and group set to the same variable
      expect_true(grepl("fill = as.factor\\(cyl\\)", expr_str))
      expect_true(grepl("group = as.factor\\(cyl\\)", expr_str))
    }
  )
})

test_that("ggplot_block handles bar chart with position", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "bar", x = "cyl", position = "dodge")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("position = 'dodge'", expr_str))
    }
  )
})

test_that("ggplot_block handles donut chart", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "pie", x = "cyl", donut = TRUE)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("xlim", expr_str))
      expect_true(session$returned$state$donut())
    }
  )
})

test_that("ggplot_block returns blank plot when x is missing", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = character())

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_blank", expr_str))
    }
  )
})

test_that("ggplot_block returns blank plot when required y is missing", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = character())

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Point chart requires y, should return blank plot
      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_blank", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# UI Interaction Tests with session$setInputs()
# -----------------------------------------------------------------------------

test_that("ggplot_block switches chart types via UI interaction", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$type(), "point")

      # Change chart type via UI
      session$setInputs(type = "bar")
      session$flushReact()

      # Check updated state and expression
      expect_equal(session$returned$state$type(), "bar")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("geom_bar|geom_col", expr_str))
    }
  )
})

test_that("ggplot_block updates aesthetic mappings via UI", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Add color aesthetic via UI
      session$setInputs(color = "cyl")
      session$flushReact()

      expect_equal(session$returned$state$color(), "cyl")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("colour = cyl", expr_str))
    }
  )
})

test_that("ggplot_block updates position via UI", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "bar", x = "cyl", position = "stack")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Change position via UI
      session$setInputs(position = "dodge")
      session$flushReact()

      expect_equal(session$returned$state$position(), "dodge")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("position = 'dodge'", expr_str))
    }
  )
})

test_that("ggplot_block updates bins via UI", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "histogram", x = "mpg", bins = 30)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Change bins via UI
      session$setInputs(bins = 15)
      session$flushReact()

      expect_equal(session$returned$state$bins(), 15)
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("bins = 15", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Pattern 2: Testing Plot Generation (block_server) - PREFERRED
# -----------------------------------------------------------------------------

test_that("ggplot_block creates valid scatter plot - testServer", {
  block <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      # Test the actual plot object
      expect_s3_class(result, "ggplot")

      # Check plot data
      expect_equal(nrow(result$data), nrow(mtcars))

      # Check aesthetics
      expect_true("x" %in% names(result$mapping))
      expect_true("y" %in% names(result$mapping))

      # Check geom layer
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomPoint")
    }
  )
})

test_that("ggplot_block creates valid bar chart", {
  block <- new_ggplot_block(type = "bar", x = "cyl")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomBar")
    }
  )
})

test_that("ggplot_block creates valid histogram", {
  block <- new_ggplot_block(type = "histogram", x = "mpg", bins = 20)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomBar")
      expect_equal(result$layers[[1]]$stat_params$bins, 20)
    }
  )
})

test_that("ggplot_block creates valid line chart", {
  block <- new_ggplot_block(type = "line", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomLine")
    }
  )
})

test_that("ggplot_block creates valid boxplot", {
  block <- new_ggplot_block(type = "boxplot", x = "cyl", y = "mpg")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomBoxplot")
    }
  )
})

test_that("ggplot_block applies theme_minimal by default", {
  block <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      # Check that theme has been applied (theme object exists)
      expect_true(!is.null(result$theme))
    }
  )
})
