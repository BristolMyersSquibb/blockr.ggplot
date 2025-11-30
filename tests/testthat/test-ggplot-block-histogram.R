# Tests for new_ggplot_block - Histogram type
# Following blockr.dplyr testing conventions
#
# Histograms support:
#   required: x
#   optional: fill, color, alpha
#   specific: bins, position (stack, identity, dodge)

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("histogram with x - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "mpg")

      # Verify it has a histogram geom layer (actually GeomBar with stat bin)
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

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("histogram - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "mpg")

      expr$setInputs(x = "hp")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "hp")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Optional aesthetics tests - fill, color, alpha
# =============================================================================

test_that("histogram with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Fill is wrapped in as.factor() for histograms
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("cyl", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg"
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
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("cyl", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Note: color (border color) mapped to a variable is dropped during stat_bin
# because each histogram bin aggregates multiple observations. This is expected
# behavior, but we test that the mapping is still created correctly.

test_that("histogram with color - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    color = "am"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # Expect warning about colour dropped during stat transform
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$colour), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram - changing color input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$colour)

      # Warning is thrown during setInputs (which triggers internal flush)
      expect_warning(
        {
          expr$setInputs(color = "am")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$colour), "am")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# Note: alpha mapped to variable is dropped during stat_bin like color

test_that("histogram with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    alpha = "wt"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # Expect warning about alpha dropped during stat transform
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$alpha), "wt")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      # Warning is thrown during setInputs (which triggers internal flush)
      expect_warning(
        {
          expr$setInputs(alpha = "wt")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "wt")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Specific options tests - bins, position
# =============================================================================

test_that("histogram with custom bins - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    bins = 15
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Check bins in layer params
      layer <- result$layers[[1]]
      expect_equal(layer$stat_params$bins, 15)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram - changing bins input - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    bins = 30
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_equal(layer$stat_params$bins, 30)

      expr$setInputs(bins = 10)
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_equal(layer$stat_params$bins, 10)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram with position dodge - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl",
    position = "dodge"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionDodge"))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("histogram - changing position input - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl",
    position = "stack"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionStack"))

      expr$setInputs(position = "identity")
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_true(inherits(layer$position, "PositionIdentity"))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
