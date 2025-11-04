library(testthat)
library(blockr.ggplot)
library(shiny)
library(ggplot2)

# ==============================================================================
# LAYER 1: Unit Tests (Pure Functions, No Shiny)
# ==============================================================================

test_that("facet_block constructor creates valid block", {
  blk <- new_facet_block()

  # Check class hierarchy
  expect_s3_class(blk, "facet_block")
  expect_s3_class(blk, "ggplot_transform_block")
  expect_s3_class(blk, "block")

  # Check has required components
  expect_true(is.function(blk$expr_server))
  expect_true(is.function(blk$expr_ui))
})

test_that("facet_block constructor accepts all parameters", {
  blk <- new_facet_block(
    facet_type = "grid",
    facets = c("cyl"),
    rows = c("am"),
    cols = c("gear"),
    ncol = "2",
    nrow = "3",
    scales = "free_x",
    labeller = "label_both",
    dir = "v",
    space = "free_y"
  )

  expect_s3_class(blk, "facet_block")
})

test_that("facet_block has correct allow_empty_state", {
  blk <- new_facet_block()

  expect_true("facets" %in% blk$allow_empty_state)
  expect_true("rows" %in% blk$allow_empty_state)
  expect_true("cols" %in% blk$allow_empty_state)
  expect_true("ncol" %in% blk$allow_empty_state)
  expect_true("nrow" %in% blk$allow_empty_state)
})

test_that("facet_block has dat_valid function", {
  blk <- new_facet_block()

  # Should accept ggplot objects
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  expect_silent(blk$dat_valid(test_plot))

  # Should reject non-ggplot objects
  expect_error(blk$dat_valid(mtcars))
})

# ==============================================================================
# LAYER 2: testServer (ALL Shiny Interactions)
# ==============================================================================

# -----------------------------------------------------------------------------
# facet_wrap Tests
# -----------------------------------------------------------------------------

test_that("facet_block generates facet_wrap expression with single variable", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

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

      expect_true(grepl("facet_wrap", expr_str))
      expect_true(grepl("~cyl", expr_str))

      # Test state
      expect_equal(result$state$facet_type(), "wrap")
      expect_equal(result$state$facets(), "cyl")
    }
  )
})

test_that("facet_block generates facet_wrap with multiple variables", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl", "gear"))

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("facet_wrap", expr_str))
      expect_true(grepl("~cyl \\+ gear", expr_str))
    }
  )
})

test_that("facet_block generates facet_wrap with ncol parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), ncol = "2")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("ncol = 2", expr_str))
      expect_equal(session$returned$state$ncol(), "2")
    }
  )
})

test_that("facet_block generates facet_wrap with nrow parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), nrow = "3")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("nrow = 3", expr_str))
      expect_equal(session$returned$state$nrow(), "3")
    }
  )
})

test_that("facet_block generates facet_wrap with scales parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = c("cyl"),
    scales = "free_x"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("scales = 'free_x'", expr_str))
    }
  )
})

test_that("facet_block generates facet_wrap with labeller parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = c("cyl"),
    labeller = "label_both"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("labeller = 'label_both'", expr_str))
    }
  )
})

test_that("facet_block generates facet_wrap with dir parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), dir = "v")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("dir = 'v'", expr_str))
      expect_equal(session$returned$state$dir(), "v")
    }
  )
})

test_that("facet_block returns data unchanged when no facets selected", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = character())

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      # Should just return data
      expect_true(grepl("\\(data\\)", expr_str))
      expect_false(grepl("facet_wrap", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# facet_grid Tests
# -----------------------------------------------------------------------------

test_that("facet_block generates facet_grid expression with rows", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl"),
    cols = character()
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("facet_grid", expr_str))
      expect_true(grepl("cyl ~ \\.", expr_str))
      expect_equal(session$returned$state$facet_type(), "grid")
    }
  )
})

test_that("facet_block generates facet_grid with columns", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = character(),
    cols = c("gear")
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("facet_grid", expr_str))
      expect_true(grepl("\\. ~ gear", expr_str))
    }
  )
})

test_that("facet_block generates facet_grid with both rows and columns", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl"),
    cols = c("gear")
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("facet_grid", expr_str))
      expect_true(grepl("cyl ~ gear", expr_str))
    }
  )
})

test_that("facet_block generates facet_grid with multiple row variables", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl", "am"),
    cols = character()
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("cyl \\+ am ~ \\.", expr_str))
    }
  )
})

test_that("facet_block generates facet_grid with space parameter", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl"),
    cols = c("gear"),
    space = "free_x"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("space = 'free_x'", expr_str))
    }
  )
})

test_that("facet_block returns data unchanged when no grid vars selected", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(
    facet_type = "grid",
    rows = character(),
    cols = character()
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      # Should just return data
      expect_true(grepl("\\(data\\)", expr_str))
      expect_false(grepl("facet_grid", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# UI Interaction Tests
# -----------------------------------------------------------------------------

test_that("facet_block switches between wrap and grid via UI", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$facet_type(), "wrap")

      # Switch to grid
      session$setInputs(facet_type = "grid", rows = c("cyl"), cols = character())
      session$flushReact()

      expect_equal(session$returned$state$facet_type(), "grid")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("facet_grid", expr_str))
    }
  )
})

test_that("facet_block updates facets via UI", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Change facets via UI
      session$setInputs(facets = c("gear"))
      session$flushReact()

      expect_equal(session$returned$state$facets(), "gear")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("~gear", expr_str))
    }
  )
})

test_that("facet_block updates ncol via UI", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), ncol = "2")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Change ncol via UI
      session$setInputs(ncol = "3")
      session$flushReact()

      expect_equal(session$returned$state$ncol(), "3")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("ncol = 3", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Pattern 2: Testing Plot Generation (block_server)
# -----------------------------------------------------------------------------

test_that("facet_block creates valid faceted plot with facet_wrap", {
  # Create a ggplot object first
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  block <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      # Test the actual plot object
      expect_s3_class(result, "ggplot")

      # Check that faceting was applied
      expect_s3_class(result$facet, "FacetWrap")
    }
  )
})

test_that("facet_block creates valid faceted plot with facet_grid", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  block <- new_facet_block(facet_type = "grid", rows = c("cyl"), cols = c("gear"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_s3_class(result$facet, "FacetGrid")
    }
  )
})

test_that("facet_block passes through unchanged plot when no facets", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  block <- new_facet_block(facet_type = "wrap", facets = character())

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      # Should have default null facet
      expect_s3_class(result$facet, "FacetNull")
    }
  )
})
