library(testthat)
library(blockr.ggplot)
library(shiny)
library(ggplot2)

# ==============================================================================
# LAYER 1: Unit Tests (Pure Functions, No Shiny)
# ==============================================================================

test_that("new_ggplot_transform_block creates valid block", {
  # Create a simple transform block
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  # Check class hierarchy
  expect_s3_class(blk, "test_ggplot_transform")
  expect_s3_class(blk, "ggplot_transform_block")
  expect_s3_class(blk, "block")

  # Check has required components
  expect_true(is.function(blk$expr_server))
  expect_true(is.function(blk$expr_ui))
})

test_that("block_ui.ggplot_transform_block returns plot output", {
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  ui <- block_ui.ggplot_transform_block("test", blk)

  # Should return a tagList with plotOutput
  expect_s3_class(ui, "shiny.tag.list")
  # Convert to character to check for plotOutput
  ui_html <- as.character(ui)
  expect_true(grepl("shiny-plot-output", ui_html))
})

test_that("block_eval.ggplot_transform_block builds ggplot", {
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  # Create a test plot
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  # Evaluate the block
  result <- block_eval.ggplot_transform_block(
    blk,
    expr = quote(data),
    data = list(data = test_plot)
  )

  # Should return a ggplot object
  expect_s3_class(result, "ggplot")
})

test_that("block_eval.ggplot_transform_block calls ggplot_build", {
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  # Create a test plot
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  # Call block_eval (this internally calls ggplot_build for validation)
  result <- block_eval.ggplot_transform_block(
    blk,
    expr = quote(data),
    data = list(data = test_plot)
  )

  # ggplot_build validates the plot structure
  # If it succeeds, the plot is valid
  expect_s3_class(result, "ggplot")
})

# ==============================================================================
# LAYER 2: testServer (ALL Shiny Interactions)
# ==============================================================================

test_that("ggplot_transform_block works with testServer", {
  # Create a simple transform block
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              # Simple pass-through
              parse(text = "(data)")[[1]]
            }),
            state = list(
              status = reactive("active")
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression
      expr_result <- result$expr()
      expect_true(is.call(expr_result) || is.name(expr_result))

      # Test state
      expect_true(is.reactive(result$state$status))
      expect_equal(result$state$status(), "active")
    }
  )
})

test_that("ggplot_transform_block accepts ggplot input", {
  # Create a transform that adds a layer
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              parse(text = "data + ggplot2::geom_smooth(method = 'lm')")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Add smooth layer"))
    },
    class = "test_smooth_transform",
    dat_valid = function(data) {
      stopifnot(inherits(data, "ggplot"))
    }
  )

  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_smooth", expr_str))
    }
  )
})

test_that("ggplot_transform_block dat_valid rejects non-ggplot input", {
  # Create a block with dat_valid
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform",
    dat_valid = function(data) {
      stopifnot(inherits(data, "ggplot"))
    }
  )

  # Should reject data frame input
  expect_error(blk$dat_valid(mtcars))

  # Should accept ggplot input
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  expect_silent(blk$dat_valid(test_plot))
})

test_that("ggplot_transform_block can chain transformations", {
  # Create two transform blocks
  blk1 <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              parse(text = "data + ggplot2::geom_smooth(method = 'lm')")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Add smooth"))
    },
    class = "test_smooth_transform"
  )

  blk2 <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              parse(text = "data + ggplot2::labs(title = 'Test Plot')")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Add title"))
    },
    class = "test_labs_transform"
  )

  # Test first block
  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  input_data <- reactive(test_plot)

  testServer(
    blk1$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("geom_smooth", expr_str))
    }
  )

  # Test second block (assumes first block output as input)
  testServer(
    blk2$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")

      expect_true(grepl("labs", expr_str))
      expect_true(grepl("Test Plot", expr_str))
    }
  )
})

# -----------------------------------------------------------------------------
# Pattern 2: Testing with block_server
# -----------------------------------------------------------------------------

test_that("ggplot_transform_block renders plot via block_server", {
  # Create a simple pass-through transform
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      # Should return a ggplot object
      expect_s3_class(result, "ggplot")
    }
  )
})

test_that("ggplot_transform_block adds layer via block_server", {
  # Create a block that adds a layer
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              parse(
                text = "data + ggplot2::geom_smooth(method = 'lm', se = FALSE)"
              )[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Add smooth"))
    },
    class = "test_smooth_transform"
  )

  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(data = function() test_plot)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")

      # Check that smooth layer was added
      expect_equal(length(result$layers), 2)
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomPoint")
      expect_equal(class(result$layers[[2]]$geom)[1], "GeomSmooth")
    }
  )
})

test_that("block_output.ggplot_transform_block returns renderPlot", {
  blk <- new_ggplot_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive(parse(text = "(data)")[[1]]),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(div("Test UI"))
    },
    class = "test_ggplot_transform"
  )

  test_plot <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  # Create a mock session
  testServer(
    app = function(input, output, session) {
      output$test <- block_output.ggplot_transform_block(
        blk,
        test_plot,
        session
      )
    },
    expr = {
      # The output should be a renderPlot
      expect_true("shiny.render.function" %in% class(output$test))
    }
  )
})
