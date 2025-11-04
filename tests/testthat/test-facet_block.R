test_that("facet_block constructor creates valid object", {
  blk <- new_facet_block()
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "plot_block", "block")
  )
})

test_that("facet_block constructor accepts parameters", {
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))
  expect_s3_class(blk, "facet_block")
})

test_that("facet_block generates facet_wrap expression", {
  # Create a ggplot object as input
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test state (more reliable than string matching)
      expect_equal(result$state$facet_type(), "wrap")
      expect_equal(result$state$facets(), c("cyl"))
      
      # Test that expression is a call
      expr_result <- result$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block generates facet_wrap with multiple variables", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl", "gear"))

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$facets(), c("cyl", "gear"))
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block generates facet_wrap with ncol", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), ncol = "2")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$ncol(), "2")
      expect_equal(session$returned$state$facets(), c("cyl"))
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block generates facet_wrap with scales option", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = c("cyl"),
    scales = "free"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$scales(), "free")
      expect_equal(session$returned$state$facets(), c("cyl"))
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block generates facet_grid expression", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "grid", rows = c("cyl"), cols = c("gear"))

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$facet_type(), "grid")
      expect_equal(session$returned$state$rows(), c("cyl"))
      expect_equal(session$returned$state$cols(), c("gear"))
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block handles empty facets for wrap", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = character())

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$facets(), character())
      
      # Expression should still be valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result) || is.name(expr_result))
    }
  )
})

test_that("facet_block handles empty facets for grid", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(
    facet_type = "grid",
    rows = character(),
    cols = character()
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$rows(), character())
      expect_equal(session$returned$state$cols(), character())
      
      # Expression should still be valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result) || is.name(expr_result))
    }
  )
})

test_that("facet_block generates facet_grid with only rows", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl"),
    cols = character()
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$rows(), c("cyl"))
      expect_equal(session$returned$state$cols(), character())
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block generates facet_grid with only cols", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(
    facet_type = "grid",
    rows = character(),
    cols = c("gear")
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$rows(), character())
      expect_equal(session$returned$state$cols(), c("gear"))
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block switches between wrap and grid", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Initial state
      expect_equal(session$returned$state$facet_type(), "wrap")

      # Change facet type
      session$setInputs(facet_type = "grid")
      session$flushReact()

      expect_equal(session$returned$state$facet_type(), "grid")
    }
  )
})

test_that("facet_block creates valid faceted plot using block_server", {
  input_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  block <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() input_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      # Check that facet was added
      expect_true(inherits(result$facet, "FacetWrap"))
    }
  )
})

test_that("facet_block creates valid facet_grid plot using block_server", {
  input_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  block <- new_facet_block(
    facet_type = "grid",
    rows = c("cyl"),
    cols = c("gear")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() input_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      # Check that facet was added
      expect_true(inherits(result$facet, "FacetGrid"))
    }
  )
})

test_that("facet_block handles direction parameter for wrap", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(facet_type = "wrap", facets = c("cyl"), dir = "v")

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$dir(), "v")
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block handles labeller parameter", {
  input_plot <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
      ggplot2::geom_point()
  })
  blk <- new_facet_block(
    facet_type = "wrap",
    facets = c("cyl"),
    labeller = "label_both"
  )

  testServer(
    blk$expr_server,
    args = list(data = input_plot),
    {
      session$flushReact()

      # Test state
      expect_equal(session$returned$state$labeller(), "label_both")
      
      # Test that expression is a call
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("facet_block validates input is ggplot object", {
  block <- new_facet_block(facet_type = "wrap", facets = c("cyl"))

  # dat_valid should require ggplot object
  expect_error(block$dat_valid(mtcars), "inherits")
  expect_silent(block$dat_valid(ggplot2::ggplot(mtcars)))
})
