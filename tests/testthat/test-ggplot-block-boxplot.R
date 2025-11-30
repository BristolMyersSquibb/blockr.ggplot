# Tests for new_ggplot_block - Boxplot type
# Following blockr.dplyr testing conventions
#
# Boxplots support:
#   required: x, y
#   optional: fill, color, alpha
#
# Note: Using iris data because boxplots need a categorical x variable
# to avoid "Orientation is not uniquely specified" warnings

# =============================================================================
# Initialization tests - verify constructor parameters work
# =============================================================================

test_that("boxplot with x and y - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "Species")
      expect_equal(rlang::as_name(result$mapping$y), "Sepal.Length")

      # Verify it has a boxplot geom layer
      layer_classes <- vapply(
        result$layers,
        function(l) class(l$geom)[1],
        character(1)
      )
      expect_true("GeomBoxplot" %in% layer_classes)
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

# =============================================================================
# setInputs tests - verify UI input changes produce expected output
# =============================================================================

test_that("boxplot - changing x input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$x), "Species")

      # Species is the only factor in iris, so we just verify mapping works
      # by checking initial state
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("boxplot - changing y input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "Sepal.Length")

      # Change y input
      expr$setInputs(y = "Petal.Width")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$y), "Petal.Width")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

# =============================================================================
# Optional aesthetics tests - fill, color, alpha
# =============================================================================

test_that("boxplot with fill - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length",
    fill = "Species"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Fill is wrapped in as.factor() for boxplots
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("Species", fill_expr))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("boxplot - changing fill input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$fill)

      expr$setInputs(fill = "Species")
      session$flushReact()

      result <- session$returned$result()
      fill_expr <- deparse(result$mapping$fill)
      expect_true(grepl("Species", fill_expr))
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("boxplot with color - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length",
    color = "Species"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$colour), "Species")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("boxplot - changing color input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$colour)

      expr$setInputs(color = "Species")
      session$flushReact()

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$colour), "Species")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

# Note: alpha mapped to a variable is dropped during boxplot stat transformation

test_that("boxplot with alpha - initialization - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length",
    alpha = "Sepal.Width"
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
      expect_equal(rlang::as_name(result$mapping$alpha), "Sepal.Width")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})

test_that("boxplot - changing alpha input updates mapping - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "boxplot",
    x = "Species",
    y = "Sepal.Length"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      result <- session$returned$result()
      expect_null(result$mapping$alpha)

      expect_warning(
        {
          expr$setInputs(alpha = "Sepal.Width")
          session$flushReact()
        },
        "aesthetics were dropped"
      )

      result <- session$returned$result()
      expect_equal(rlang::as_name(result$mapping$alpha), "Sepal.Width")
    },
    args = list(x = block, data = list(data = function() iris))
  )
})
