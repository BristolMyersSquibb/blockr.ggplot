# Tests for new_ggplot_block - Bar chart type
# Following blockr.dplyr testing conventions
#
# Bar charts support:
#   required: x
#   optional: y, fill, color, alpha
#   specific: position (stack, dodge, fill)

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("bar plot with x only - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "cyl")

      # Verify it has a bar geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomBar" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot with x and y (geom_col) - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "cyl")
      expect_equal(rlang::as_name(result$mapping$y), "mpg")

      # Verify it has a col geom layer (geom_col)
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomCol" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("bar plot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "cyl")

      # Change x input
      expr$setInputs(x = "gear")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "gear")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot - changing y input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially no y mapping (geom_bar)
      result <- session$returned$result()
      expect_null(result$mapping$y)

      # Add y input - should switch to geom_col
      expr$setInputs(y = "mpg")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "mpg")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Optional aesthetics tests - fill, color, alpha
# =============================================================================

test_that("bar plot with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Fill is wrapped in as.factor() for bar charts
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("gear", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$fill)

      expr$setInputs(fill = "gear")
      session$flushReact()

      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("gear", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Note: color/alpha tests use geom_col (with y) because geom_bar with stat_count
# drops these aesthetics during aggregation

test_that("bar plot (geom_col) with color - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "mpg",
    color = "am"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$colour), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot (geom_col) - color input updates mapping", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$colour)

      expr$setInputs(color = "am")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$colour), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot (geom_col) with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "mpg",
    alpha = "wt"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$alpha), "wt")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot (geom_col) - alpha input updates mapping", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      expr$setInputs(alpha = "wt")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "wt")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Specific options tests - position
# =============================================================================

test_that("bar plot with position dodge - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear",
    position = "dodge"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Check position in layer
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionDodge"))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("bar plot - changing position input - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear",
    position = "stack"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # Initially stacked
      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionStack"))

      # Change to dodge
      expr$setInputs(position = "dodge")
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionDodge"))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
