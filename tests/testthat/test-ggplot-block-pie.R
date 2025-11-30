# Tests for new_ggplot_block - Pie chart type
# Following blockr.dplyr testing conventions
#
# Pie charts support:
#   required: x (categories)
#   optional: y, fill, alpha
#   specific: donut (TRUE/FALSE for donut style)
#
# Note: Pie charts use stat_count internally, which drops aesthetics like alpha
# during statistical transformation. Tests use expect_warning() where appropriate.

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================
test_that("pie chart with x only - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # Pie charts with stat_count warn about dropped aesthetics
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))

      # Verify it has polar coordinates
      expect_true(inherits(result$coordinates, "CoordPolar"))

      # Verify it has a bar/col geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true(any(c("GeomBar", "GeomCol") %in% layer_classes))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("pie chart with x and y - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl",
    y = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # With y specified, uses geom_col which doesn't have the warning
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_true(inherits(result$coordinates, "CoordPolar"))

      # With y specified, should have y mapping
      expect_equal(rlang::as_name(result$mapping$y), "mpg")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("pie chart - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )

      # For pie charts, x is used for fill, so check fill mapping
      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("cyl", fill_expr))

      expect_warning(
        {
          expr$setInputs(x = "gear")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("gear", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Optional aesthetics tests - fill, alpha
# =============================================================================

test_that("pie chart with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl",
    fill = "gear"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # When fill is explicitly set, it should use that
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("gear", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("pie chart - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )

      # Initially fill is from x
      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("cyl", fill_expr))

      # Set explicit fill
      expect_warning(
        {
          expr$setInputs(fill = "am")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("am", fill_expr))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("pie chart with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl",
    alpha = "mpg"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$alpha), "mpg")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("pie chart - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      expect_warning(
        {
          expr$setInputs(alpha = "mpg")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "mpg")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

# =============================================================================
# Specific options tests - donut
# =============================================================================

test_that("pie chart with donut style - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl",
    donut = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_true(inherits(result$coordinates, "CoordPolar"))

      # For donut, x should be numeric (2) for the donut hole
      # The xlim should be set
      expect_true(!is.null(result$scales$get_scales("x")$limits))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("pie chart - changing donut input - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "pie",
    x = "cyl",
    donut = FALSE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      expect_warning(
        session$flushReact(),
        "aesthetics were dropped"
      )

      # Initially not a donut
      result <- session$returned$result()
      expect_true(inherits(result, "ggplot"))

      # Change to donut
      expect_warning(
        {
          expr$setInputs(donut = TRUE)
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_true(inherits(result$coordinates, "CoordPolar"))
      # Donut should have xlim set
      expect_true(!is.null(result$scales$get_scales("x")$limits))
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
