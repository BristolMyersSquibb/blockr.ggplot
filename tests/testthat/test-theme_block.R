test_that("theme_block constructor creates valid object", {
  blk <- new_theme_block()
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "plot_block", "block")
  )
})

test_that("theme_block constructor accepts parameters", {
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "#FFFFFF")
  expect_s3_class(blk, "theme_block")
})

test_that("theme_block preserves upstream theme with auto", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "auto")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression - should just return (data) when auto
      expr_result <- result$expr()
      expr_str <- deparse(expr_result)
      expr_str <- paste0(expr_str, collapse = "")
      expect_true(grepl("\\(data\\)", expr_str))
      expect_false(grepl("theme_minimal", expr_str))

      # Test state
      expect_equal(result$state$base_theme(), "auto")
    }
  )
})

test_that("theme_block applies base theme", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("theme_minimal", expr_str))
    }
  )
})

test_that("theme_block applies classic theme", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "classic")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("theme_classic", expr_str))
    }
  )
})

test_that("theme_block handles panel background color", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "#FFFFFF")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("panel.background", expr_str))
      expect_true(grepl("#FFFFFF", expr_str))
    }
  )
})

test_that("theme_block handles plot background color", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", plot_bg = "#F0F0F0")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("plot.background", expr_str))
      expect_true(grepl("#F0F0F0", expr_str))
    }
  )
})

test_that("theme_block handles base size", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", base_size = 14)

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("text = ggplot2::element_text", expr_str))
      expect_true(grepl("size = 14", expr_str))
    }
  )
})

test_that("theme_block handles base family", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", base_family = "serif")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('family = "serif"', expr_str))
    }
  )
})

test_that("theme_block hides major grid lines", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", show_major_grid = "hide")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(
        grepl("panel.grid.major = ggplot2::element_blank\\(\\)", expr_str)
      )
    }
  )
})

test_that("theme_block shows minor grid lines", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", show_minor_grid = "show")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(
        grepl("panel.grid.minor = ggplot2::element_line\\(\\)", expr_str)
      )
    }
  )
})

test_that("theme_block handles grid color", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", grid_color = "#CCCCCC")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("panel.grid.major", expr_str))
      expect_true(grepl("#CCCCCC", expr_str))
    }
  )
})

test_that("theme_block shows panel border", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", show_panel_border = "show")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("panel.border = ggplot2::element_rect", expr_str))
    }
  )
})

test_that("theme_block hides panel border", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", show_panel_border = "hide")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(
        grepl("panel.border = ggplot2::element_blank\\(\\)", expr_str)
      )
    }
  )
})

test_that("theme_block handles legend position", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", legend_position = "bottom")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('legend.position = "bottom"', expr_str))
    }
  )
})

test_that("theme_block handles legend position none", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", legend_position = "none")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('legend.position = "none"', expr_str))
    }
  )
})

test_that("theme_block combines multiple theme options", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(
    base_theme = "minimal",
    panel_bg = "#FFFFFF",
    base_size = 12,
    show_major_grid = "hide",
    legend_position = "bottom"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("theme_minimal", expr_str))
      expect_true(grepl("panel.background", expr_str))
      expect_true(grepl("size = 12", expr_str))
      expect_true(grepl("panel.grid.major = ggplot2::element_blank", expr_str))
      expect_true(grepl('legend.position = "bottom"', expr_str))
    }
  )
})

test_that("theme_block state reactives work", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "#FFFFFF")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$base_theme(), "minimal")
      expect_equal(session$returned$state$panel_bg(), "#FFFFFF")
    }
  )
})

test_that("theme_block updates state via UI inputs", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$base_theme(), "minimal")

      # Update via input
      session$setInputs(base_theme = "classic")
      session$flushReact()

      expect_equal(session$returned$state$base_theme(), "classic")

      # Expression should reflect the change
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("theme_classic", expr_str))
    }
  )
})

test_that("theme_block creates valid themed plot using block_server", {
  input_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  block <- new_theme_block(base_theme = "minimal", legend_position = "bottom")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() input_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      # Check that theme was applied
      expect_equal(result$theme$legend.position, "bottom")
    }
  )
})

test_that("theme_block handles empty string colors as auto", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", panel_bg = "", plot_bg = "")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Empty colors should not appear in expression
      expect_false(grepl("panel.background", expr_str))
      expect_false(grepl("plot.background", expr_str))
    }
  )
})

test_that("theme_block handles NA base_size as auto", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", base_size = NA_real_)

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # NA base_size should not appear in expression
      expect_false(grepl("size =", expr_str))
    }
  )
})

test_that("theme_block handles auto base_family", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "minimal", base_family = "auto")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Auto base_family should not appear in expression
      expect_false(grepl("family =", expr_str))
    }
  )
})

test_that("theme_block applies gray theme with special plot background", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(base_theme = "gray")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("theme_gray", expr_str))
      # Gray theme should set plot background
      expect_true(grepl("plot.background", expr_str))
      expect_true(grepl("#EBEBEB", expr_str))
    }
  )
})

test_that("theme_block handles auto for grid options", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_theme_block(
    base_theme = "minimal",
    show_major_grid = "auto",
    show_minor_grid = "auto"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Auto grid options with no color should not add grid customizations
      # (unless grid_color is set)
      # Just base theme should be applied
      expect_true(grepl("theme_minimal", expr_str))
    }
  )
})
