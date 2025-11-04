test_that("grid_block constructor creates valid object", {
  blk <- new_grid_block()
  expect_s3_class(
    blk,
    c(
      "grid_block",
      "rbind_block",
      "ggplot_transform_block",
      "plot_block",
      "block"
    )
  )
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

      # Test that expression is valid
      expr_result <- result$expr()
      expect_true(is.call(expr_result))
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

      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})
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

      # Test state
      expect_equal(session$returned$state$ncol(), "2")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$nrow(), "1")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$title(), "My Grid Title")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$subtitle(), "A subtitle")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$caption(), "Data source")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$tag_levels(), "A")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$guides(), "collect")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$ncol(), "")
      expect_equal(session$returned$state$nrow(), "")
      expect_equal(session$returned$state$title(), "")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test that expression is still valid after update
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
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

      # Test state
      expect_equal(session$returned$state$ncol(), "2")
      expect_equal(session$returned$state$title(), "My Title")
      expect_equal(session$returned$state$subtitle(), "My Subtitle")
      expect_equal(session$returned$state$tag_levels(), "A")
      
      # Test that expression is valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("grid_block validates at least one input is required", {
  block <- new_grid_block()

  # dat_valid should require at least 1 argument
  expect_error(block$dat_valid(), "length")
})
