# Tests for new_ggplot_block - Line chart type
# Following blockr.dplyr testing conventions
#
# Line charts support:
#   required: x, y
#   optional: color, linetype, alpha, group

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("line plot with x and y - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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

      # Verify it has a line geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomLine" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("line plot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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

test_that("line plot - changing y input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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
# Optional aesthetics tests - color, linetype, alpha, group
# =============================================================================

test_that("line plot with color - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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
      expect_equal(rlang::as_name(result$mapping$colour), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("line plot - changing color input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$colour)

      expr$setInputs(color = "cyl")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$colour), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("line plot with linetype - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
    x = "wt",
    y = "mpg",
    linetype = "gear"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Linetype is wrapped in as.factor() in the block
      linetype_expr <- deparse(result$mapping$linetype)
      expect_true(grepl("gear", linetype_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("line plot - changing linetype input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$linetype)

      expr$setInputs(linetype = "gear")
      session$flushReact()

      result <- session$returned$result()
      # Linetype is wrapped in as.factor()
      linetype_expr <- deparse(result$mapping$linetype)
      expect_true(grepl("gear", linetype_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("line plot with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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

test_that("line plot - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
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

test_that("line plot with group - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
    x = "wt",
    y = "mpg",
    group = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$group), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("line plot - changing group input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "line",
    x = "wt",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$group)

      expr$setInputs(group = "cyl")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$group), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
