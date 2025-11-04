test_that("grid_block constructor creates valid object", {
  blk <- new_grid_block()
  expect_s3_class(blk, c("grid_block", "rbind_block", "ggplot_transform_block", "plot_block", "block"))
})

test_that("grid_block constructor accepts parameters", {
  blk <- new_grid_block(ncol = "2", nrow = "2", title = "Test Grid")
  expect_s3_class(blk, "grid_block")
})

test_that("grid_block generates wrap_plots expression with single plot", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block()

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression structure
      expr_result <- result$expr()
      expr_str <- deparse(expr_result)
      expr_str <- paste0(expr_str, collapse = "")
      expect_true(grepl("patchwork::wrap_plots", expr_str))
    }
  )
})

test_that("grid_block generates wrap_plots expression with multiple plots", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block()

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("patchwork::wrap_plots", expr_str))
      expect_true(grepl("plot1", expr_str))
      expect_true(grepl("plot2", expr_str))
    }
  )
})

test_that("grid_block handles ncol parameter", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block(ncol = "2")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("plot_layout", expr_str))
      expect_true(grepl("ncol = 2", expr_str))
    }
  )
})

test_that("grid_block handles nrow parameter", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block(nrow = "1")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("nrow = 1", expr_str))
    }
  )
})

test_that("grid_block handles title annotation", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(title = "My Grid Title")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("plot_annotation", expr_str))
      expect_true(grepl('title = "My Grid Title"', expr_str))
    }
  )
})

test_that("grid_block handles subtitle annotation", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(subtitle = "A subtitle")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('subtitle = "A subtitle"', expr_str))
    }
  )
})

test_that("grid_block handles caption annotation", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(caption = "Data source")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('caption = "Data source"', expr_str))
    }
  )
})

test_that("grid_block handles tag_levels annotation", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block(tag_levels = "A")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl('tag_levels = "A"', expr_str))
    }
  )
})

test_that("grid_block handles guides parameter", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, color = cyl)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg, fill = gear)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block(guides = "collect")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("plot_layout", expr_str))
      expect_true(grepl('guides = "collect"', expr_str))
    }
  )
})

test_that("grid_block handles empty string parameters correctly", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(ncol = "", nrow = "", title = "")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Empty ncol/nrow should not appear in expression
      expect_false(grepl("ncol =", expr_str))
      expect_false(grepl("nrow =", expr_str))
      # Empty title should not appear
      expect_false(grepl("plot_annotation", expr_str))
    }
  )
})

test_that("grid_block state reactives work", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(ncol = "2", title = "Initial Title")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$ncol(), "2")
      expect_equal(session$returned$state$title(), "Initial Title")
    }
  )
})

test_that("grid_block updates state via UI inputs", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_grid_block(ncol = "2")

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$ncol(), "2")

      # Update via input
      session$setInputs(ncol = "3")
      session$flushReact()

      expect_equal(session$returned$state$ncol(), "3")

      # Expression should reflect the change
      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("ncol = 3", expr_str))
    }
  )
})

test_that("grid_block creates valid patchwork object using block_server", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
    ggplot2::geom_bar(stat = "identity")

  block <- new_grid_block(ncol = "2", title = "Test Grid")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(
      x = block,
      data = list(
        plot1 = function() plot1,
        plot2 = function() plot2
      )
    ),
    {
      session$flushReact()
      result <- session$returned$result()

      # Result should be a patchwork object (which is also a ggplot)
      expect_s3_class(result, "ggplot")
      expect_true(inherits(result, "patchwork"))
    }
  )
})

test_that("grid_block filters out NULL inputs", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive(NULL)

  blk <- new_grid_block()

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Should only include plot1, not plot2
      expect_true(grepl("plot1", expr_str))
      expect_false(grepl("plot2", expr_str))
    }
  )
})

test_that("grid_block combines multiple layout and annotation options", {
  plot1 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  plot2 <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, y = mpg)) +
      ggplot2::geom_bar(stat = "identity")
  })
  blk <- new_grid_block(
    ncol = "2",
    title = "My Title",
    subtitle = "My Subtitle",
    tag_levels = "A"
  )

  testServer(
    blk$expr_server,
    args = list(plot1 = plot1, plot2 = plot2),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("plot_layout", expr_str))
      expect_true(grepl("plot_annotation", expr_str))
      expect_true(grepl("ncol = 2", expr_str))
      expect_true(grepl('title = "My Title"', expr_str))
      expect_true(grepl('subtitle = "My Subtitle"', expr_str))
      expect_true(grepl('tag_levels = "A"', expr_str))
    }
  )
})

test_that("grid_block validates at least one input is required", {
  block <- new_grid_block()

  # dat_valid should require at least 1 argument
  expect_error(block$dat_valid(), "length")
})
