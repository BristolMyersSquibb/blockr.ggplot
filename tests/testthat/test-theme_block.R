library(testthat)
library(blockr.ggplot)
library(shiny)
library(ggplot2)

# ==============================================================================
# LAYER 1: Unit Tests (Pure Functions, No Shiny)
# ==============================================================================

test_that("theme_block constructor creates valid block", {
  blk <- new_theme_block()

  # Check class hierarchy
  expect_s3_class(blk, "theme_block")
  expect_s3_class(blk, "ggplot_transform_block")
  expect_s3_class(blk, "block")

  # Check has required components
  expect_true(is.function(blk$expr_server))
  expect_true(is.function(blk$expr_ui))
})

test_that("theme_block constructor accepts all parameters", {
  blk <- new_theme_block(
    panel_bg = "#ffffff",
    plot_bg = "#f0f0f0",
    base_size = 12,
    base_family = "sans",
    show_major_grid = "show",
    show_minor_grid = "hide",
    grid_color = "#cccccc",
    show_panel_border = "show",
    legend_position = "right",
    base_theme = "minimal"
  )

  expect_s3_class(blk, "theme_block")
})

test_that("theme_block has correct allow_empty_state", {
  blk <- new_theme_block()

  expect_true("panel_bg" %in% blk$allow_empty_state)
  expect_true("plot_bg" %in% blk$allow_empty_state)
  expect_true("base_size" %in% blk$allow_empty_state)
  expect_true("base_family" %in% blk$allow_empty_state)
  expect_true("grid_color" %in% blk$allow_empty_state)
  expect_true("legend_position" %in% blk$allow_empty_state)
})

# ==============================================================================
# LAYER 2: testServer (ALL Shiny Interactions)
# ==============================================================================

# -----------------------------------------------------------------------------
# Base Theme Tests
# -----------------------------------------------------------------------------

test_that("theme_block applies minimal theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal")

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

      expect_true(grepl("theme_minimal", expr_str))
      expect_equal(result$state$base_theme(), "minimal")
    }
  )
})

test_that("theme_block applies classic theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "classic")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("theme_classic", expr_str))
    }
  )
})

test_that("theme_block applies gray theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "gray")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("theme_gray", expr_str))
    }
  )
})

test_that("theme_block applies bw theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "bw")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("theme_bw", expr_str))
    }
  )
})

test_that("theme_block with auto theme keeps upstream theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "auto")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      # Should just return data without applying a theme
      expect_true(grepl("\\(data\\)", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Color Customization Tests
# -----------------------------------------------------------------------------

test_that("theme_block customizes panel background", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "#ffffff")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.background", expr_str))
      expect_true(grepl("#ffffff", expr_str))
    }
  )
})

test_that("theme_block customizes plot background", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", plot_bg = "#f0f0f0")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("plot.background", expr_str))
      expect_true(grepl("#f0f0f0", expr_str))
    }
  )
})

test_that("theme_block customizes grid color", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", grid_color = "#cccccc")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.grid", expr_str))
      expect_true(grepl("#cccccc", expr_str))
    }
  )
})

test_that("theme_block handles empty color values", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "", plot_bg = "")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Empty colors should be treated as "use theme default"
      expect_equal(session$returned$state$panel_bg(), "")
      expect_equal(session$returned$state$plot_bg(), "")
    }
  )
})

# -----------------------------------------------------------------------------
# Typography Tests
# -----------------------------------------------------------------------------

test_that("theme_block customizes base font size", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", base_size = 14)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("text = ggplot2::element_text", expr_str))
      expect_true(grepl("size = 14", expr_str))
    }
  )
})

test_that("theme_block customizes base font family", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", base_family = "serif")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("family = \"serif\"", expr_str))
    }
  )
})

test_that("theme_block handles auto base size", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", base_size = NA_real_)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # NA base_size should not add text element for size
      expect_true(is.na(session$returned$state$base_size()))
    }
  )
})

# -----------------------------------------------------------------------------
# Grid and Border Tests
# -----------------------------------------------------------------------------

test_that("theme_block shows major grid lines", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_major_grid = "show")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.grid.major", expr_str))
      expect_true(grepl("element_line", expr_str))
    }
  )
})

test_that("theme_block hides major grid lines", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_major_grid = "hide")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.grid.major = ggplot2::element_blank", expr_str))
    }
  )
})

test_that("theme_block shows minor grid lines", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_minor_grid = "show")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.grid.minor", expr_str))
      expect_true(grepl("element_line", expr_str))
    }
  )
})

test_that("theme_block hides minor grid lines", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_minor_grid = "hide")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.grid.minor = ggplot2::element_blank", expr_str))
    }
  )
})

test_that("theme_block shows panel border", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_panel_border = "show")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.border", expr_str))
      expect_true(grepl("element_rect", expr_str))
    }
  )
})

test_that("theme_block hides panel border", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", show_panel_border = "hide")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("panel.border = ggplot2::element_blank", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Legend Position Tests
# -----------------------------------------------------------------------------

test_that("theme_block sets legend position to right", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", legend_position = "right")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("legend.position = \"right\"", expr_str))
    }
  )
})

test_that("theme_block sets legend position to none", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", legend_position = "none")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("legend.position = \"none\"", expr_str))
    }
  )
})

test_that("theme_block with auto legend position keeps default", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", legend_position = "auto")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expect_equal(session$returned$state$legend_position(), "auto")
    }
  )
})

# -----------------------------------------------------------------------------
# UI Interaction Tests
# -----------------------------------------------------------------------------

test_that("theme_block switches base theme via UI", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$base_theme(), "minimal")

      # Change theme via UI
      session$setInputs(base_theme = "classic")
      session$flushReact()

      expect_equal(session$returned$state$base_theme(), "classic")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("theme_classic", expr_str))
    }
  )
})

test_that("theme_block updates legend position via UI", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)
  blk <- new_theme_block(base_theme = "minimal", legend_position = "right")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Change legend position via UI
      session$setInputs(legend_position = "bottom")
      session$flushReact()

      expect_equal(session$returned$state$legend_position(), "bottom")
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("legend.position = \"bottom\"", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Pattern 2: Testing Plot Generation (block_server)
# -----------------------------------------------------------------------------

test_that("theme_block creates valid themed plot", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  block <- new_theme_block(base_theme = "minimal")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      # Test the actual plot object
      expect_s3_class(result, "ggplot")

      # Check that theme has been applied
      expect_true(!is.null(result$theme))
    }
  )
})

test_that("theme_block passes through plot with auto theme", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
    geom_point() +
    theme_classic()
  block <- new_theme_block(base_theme = "auto")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
    }
  )
})

test_that("theme_block applies customizations to plot", {
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  block <- new_theme_block(
    base_theme = "minimal",
    legend_position = "none",
    show_major_grid = "hide"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")

      # Check that theme modifications were applied
      expect_equal(result$theme$legend.position, "none")
    }
  )
})
