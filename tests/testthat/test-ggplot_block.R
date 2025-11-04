test_that("ggplot_block constructor creates valid object", {
  blk <- new_ggplot_block()
  expect_s3_class(blk, c("ggplot_block", "ggplot_transform_block", "plot_block", "block"))
})

test_that("ggplot_block constructor accepts parameters", {
  blk <- new_ggplot_block(type = "bar", x = "cyl", y = "mpg")
  expect_s3_class(blk, "ggplot_block")
})

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

      # Change chart type
      session$setInputs(type = "bar")
      session$flushReact()

      expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
      expect_true(grepl("geom_bar|geom_col", expr_str))
      expect_equal(session$returned$state$type(), "bar")
    }
  )
})

test_that("ggplot_block handles optional aesthetics with (none)", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp", color = character())

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Should have (none) as default for color
      expect_equal(session$returned$state$color(), "(none)")

      # Expression should still be valid
      expr_result <- session$returned$expr()
      expect_true(is.call(expr_result))
    }
  )
})

test_that("ggplot_block handles color aesthetic", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp", color = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("colour = cyl", expr_str))
    }
  )
})

test_that("ggplot_block handles fill aesthetic for histogram", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "histogram", x = "mpg", fill = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Histogram should convert fill to factor
      expect_true(grepl("fill = as.factor\\(cyl\\)", expr_str))
    }
  )
})

test_that("ggplot_block creates valid plot object using block_server", {
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

test_that("ggplot_block creates valid bar chart using block_server", {
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

test_that("ggplot_block creates valid histogram using block_server", {
  block <- new_ggplot_block(type = "histogram", x = "mpg", bins = 15)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomBar")
      expect_equal(result$layers[[1]]$stat_params$bins, 15)
    }
  )
})

test_that("ggplot_block handles density plot with fill grouping", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "density", x = "mpg", fill = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Density should set group to match fill
      expect_true(grepl("group = as.factor\\(cyl\\)", expr_str))
      expect_true(grepl("fill = as.factor\\(cyl\\)", expr_str))
    }
  )
})

test_that("ggplot_block handles pie chart with donut option", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "pie", x = "cyl", donut = TRUE)

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("coord_polar", expr_str))
      expect_true(grepl("xlim", expr_str))
    }
  )
})

test_that("ggplot_block handles missing required y for scatter plot", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = character())

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      # Should generate blank plot when required y is missing
      expect_true(grepl("geom_blank", expr_str))
    }
  )
})

test_that("ggplot_block updates column choices when data changes", {
  data1 <- reactive(mtcars[, c("mpg", "hp")])
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = data1),
    {
      session$flushReact()

      # Initial columns should be from data1
      cols <- colnames(data1())
      expect_equal(length(cols), 2)
      expect_true("mpg" %in% cols)
      expect_true("hp" %in% cols)
    }
  )
})
