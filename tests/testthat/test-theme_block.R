test_that("theme_block constructor", {
  # Test basic constructor
  blk <- new_theme_block()
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with base_theme parameter
  blk <- new_theme_block(base_theme = "minimal")
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with color parameters
  blk <- new_theme_block(panel_bg = "#FFFFFF", plot_bg = "#F0F0F0")
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with all parameters
  blk <- new_theme_block(
    panel_bg = "#FFFFFF",
    plot_bg = "#F0F0F0",
    base_size = 12,
    base_family = "sans",
    show_major_grid = "show",
    show_minor_grid = "hide",
    grid_color = "#CCCCCC",
    show_panel_border = "show",
    legend_position = "right",
    base_theme = "minimal"
  )
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("theme_block supports various base themes", {
  base_themes <- c(
    "auto",
    "minimal",
    "classic",
    "gray",
    "bw",
    "light",
    "dark",
    "void"
  )

  for (theme in base_themes) {
    blk <- new_theme_block(base_theme = theme)
    expect_s3_class(
      blk,
      c("theme_block", "ggplot_transform_block", "transform_block", "block")
    )
  }
})

test_that("theme_block handles empty inputs", {
  # Test with empty string inputs (should use defaults)
  blk <- new_theme_block(
    panel_bg = "",
    plot_bg = "",
    grid_color = ""
  )
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("theme_block with specific options", {
  # Test with grid options
  blk <- new_theme_block(
    show_major_grid = "show",
    show_minor_grid = "hide"
  )
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test with font options
  blk <- new_theme_block(
    base_size = 14,
    base_family = "serif"
  )
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test with legend position
  blk <- new_theme_block(legend_position = "bottom")
  expect_s3_class(
    blk,
    c("theme_block", "ggplot_transform_block", "transform_block", "block")
  )
})

# Server-side tests
test_that("theme_block server input widgets update reactive values", {
  # Create a sample ggplot object
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block()$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Test base_theme input
      session$setInputs(base_theme = "minimal")
      expect_equal(base_theme(), "minimal")

      # Test legend_position input
      session$setInputs(legend_position = "bottom")
      expect_equal(legend_position(), "bottom")

      # Test show_major_grid input
      session$setInputs(show_major_grid = "show")
      expect_equal(show_major_grid(), "show")

      # Test show_minor_grid input
      session$setInputs(show_minor_grid = "hide")
      expect_equal(show_minor_grid(), "hide")

      # Test show_panel_border input
      session$setInputs(show_panel_border = "show")
      expect_equal(show_panel_border(), "show")

      # Test base_family input
      session$setInputs(base_family = "serif")
      expect_equal(base_family(), "serif")
    }
  )
})

test_that("theme_block state is correctly returned", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block()$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Check default values
      expect_equal(session$returned$state$base_theme(), "auto")
      expect_equal(session$returned$state$legend_position(), "auto")
      expect_equal(session$returned$state$show_major_grid(), "auto")
      expect_equal(session$returned$state$show_minor_grid(), "auto")

      # Update inputs and check state is updated
      session$setInputs(base_theme = "minimal")
      expect_equal(session$returned$state$base_theme(), "minimal")

      session$setInputs(legend_position = "top")
      expect_equal(session$returned$state$legend_position(), "top")

      session$setInputs(show_major_grid = "hide")
      expect_equal(session$returned$state$show_major_grid(), "hide")
    }
  )
})

test_that("theme_block expr evaluates correctly with base theme", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(base_theme = "minimal")$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("theme_block expr evaluates correctly with legend position", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, color = factor(cyl))) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(legend_position = "bottom")$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("theme_block expr evaluates correctly with grid options", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      show_major_grid = "show",
      show_minor_grid = "hide"
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

test_that("theme_block expr evaluates correctly with panel border", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(show_panel_border = "show")$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("theme_block expr evaluates correctly with font settings", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      base_size = 14,
      base_family = "serif"
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

test_that("theme_block expr evaluates correctly with background colors", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      panel_bg = "#FFFFFF",
      plot_bg = "#F0F0F0"
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

test_that("theme_block expr evaluates correctly with grid color", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      show_major_grid = "show",
      grid_color = "#CCCCCC"
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

test_that("theme_block handles auto values correctly", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      base_theme = "auto",
      legend_position = "auto",
      show_major_grid = "auto"
    )$expr_server,
    args = list(data = reactive(sample_plot)),
    expr = {
      # Evaluate the expression - should preserve upstream theme
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("theme_block expr evaluates correctly with multiple options", {
  sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, color = factor(cyl))) +
    ggplot2::geom_point()

  testServer(
    app = new_theme_block(
      base_theme = "minimal",
      panel_bg = "#FFFFFF",
      plot_bg = "#F5F5F5",
      base_size = 12,
      base_family = "sans",
      show_major_grid = "show",
      show_minor_grid = "hide",
      grid_color = "#E0E0E0",
      show_panel_border = "show",
      legend_position = "bottom"
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
