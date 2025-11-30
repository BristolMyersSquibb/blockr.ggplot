# Tests for new_ggplot_block - Area chart type
# Following blockr.dplyr testing conventions
#
# Area charts support:
#   required: x, y
#   optional: fill, alpha

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("area plot with x and y - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "wt")
      expect_equal(rlang::as_name(result$mapping$y), "mpg")

      # Verify it has an area geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomArea" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("area plot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "wt")

      expr$setInputs(x = "hp")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("area plot - changing y input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "mpg")

      expr$setInputs(y = "hp")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Optional aesthetics tests - fill, alpha
# =============================================================================

test_that("area plot with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg",
    fill = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$fill), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("area plot - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$fill)

      expr$setInputs(fill = "cyl")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$fill), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("area plot with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
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

test_that("area plot - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "area",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      expr$setInputs(alpha = "qsec")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "qsec")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
