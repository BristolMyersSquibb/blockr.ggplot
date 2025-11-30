# Tests for new_ggplot_block - Point (Scatter) plot type
# Following blockr.dplyr testing conventions
#
# Point plots support:
#   required: x, y
#   optional: color, shape, size, alpha, fill

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("point plot with x and y - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Verify result is a ggplot object
      expect_true(inherits(result, "ggplot"))

      # Verify aesthetics are correctly mapped
      expect_equal(rlang::as_name(result$mapping$x), "wt")
      expect_equal(rlang::as_name(result$mapping$y), "mpg")

      # Verify it has a point geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomPoint" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("point plot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # Get reference to nested expr module
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "wt")

      # Change x input to "hp"
      expr$setInputs(x = "hp")
      session$flushReact()

      # Verify mapping updated
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing y input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # Get reference to nested expr module
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initial result
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "mpg")

      # Change y input to "hp"
      expr$setInputs(y = "hp")
      session$flushReact()

      # Verify mapping updated
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Optional aesthetics tests - color, shape, size, alpha, fill
# =============================================================================

test_that("point plot with color - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    color = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Note: ggplot2 uses British spelling "colour"
      expect_equal(rlang::as_name(result$mapping$colour), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing color input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no color mapping
      result <- session$returned$result()
      expect_null(result$mapping$colour)

      # Set color to "cyl"
      expr$setInputs(color = "cyl")
      session$flushReact()

      # Verify color mapping added
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$colour), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot with size - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    size = "hp"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$size), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing size input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no size mapping
      result <- session$returned$result()
      expect_null(result$mapping$size)

      # Set size to "hp"
      expr$setInputs(size = "hp")
      session$flushReact()

      # Verify size mapping added
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$size), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot with shape - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    shape = "gear"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Shape is wrapped in as.factor() in the block
      shape_expr <- deparse(result$mapping$shape)
      expect_true(grepl("gear", shape_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing shape input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no shape mapping
      result <- session$returned$result()
      expect_null(result$mapping$shape)

      # Set shape to "gear"
      expr$setInputs(shape = "gear")
      session$flushReact()

      # Verify shape mapping added
      result <- session$returned$result()
      shape_expr <- deparse(result$mapping$shape)
      expect_true(grepl("gear", shape_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    alpha = "qsec"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$alpha), "qsec")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no alpha mapping
      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      # Set alpha to "qsec"
      expr$setInputs(alpha = "qsec")
      session$flushReact()

      # Verify alpha mapping added
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "qsec")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    fill = "am"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$fill), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("point plot - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no fill mapping
      result <- session$returned$result()
      expect_null(result$mapping$fill)

      # Set fill to "am"
      expr$setInputs(fill = "am")
      session$flushReact()

      # Verify fill mapping added
      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$fill), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
