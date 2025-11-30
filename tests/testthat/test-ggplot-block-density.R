# Tests for new_ggplot_block - Density plot type
# Following blockr.dplyr testing conventions
#
# Density plots support:
#   required: x
#   optional: fill, group
#   specific: density_alpha (fixed alpha value)

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("density plot with x - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
    x = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "mpg")

      # Verify it has a density geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomDensity" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("density plot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
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
# Optional aesthetics tests - fill (group is auto-set from fill for density)
# =============================================================================

test_that("density plot with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
    x = "mpg",
    fill = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Fill is wrapped in as.factor() for density plots
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("cyl", fill_expr))

      # Group should also be set (auto-matched to fill for density)
      group_expr <- deparse(result$mapping$group)
      expect_true(grepl("cyl", group_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("density plot - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
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

# =============================================================================
# Specific options tests - density_alpha
# =============================================================================

test_that("density plot with custom alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
    x = "mpg",
    density_alpha = 0.5
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Check alpha in layer aes_params
      layer <- result$layers[[1]]
      expect_equal(layer$aes_params$alpha, 0.5)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("density plot - changing density_alpha input - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "density",
    x = "mpg",
    density_alpha = 0.8
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_equal(layer$aes_params$alpha, 0.8)

      expr$setInputs(density_alpha = 0.3)
      session$flushReact()

      result <- session$returned$result()
      layer <- result$layers[[1]]
      expect_equal(layer$aes_params$alpha, 0.3)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
