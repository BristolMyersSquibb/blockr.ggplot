# Tests for non-syntactic column names (backtick handling)
# See: https://github.com/BristolMyersSquibb/blockr.ggplot/issues/69

# Blocks build their expressions as language objects, so `as.name()` carries a
# non-syntactic name through without any quoting step; there is no string for
# the parser to misread. These tests drive the real blocks end to end to
# confirm that. `test-expr-golden.R` locks the emitted code itself.

# =============================================================================
# ggplot block tests with non-syntactic column names
# =============================================================================

test_that("ggplot point plot works with non-syntactic x column - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create data with non-syntactic column names
  test_data <- data.frame(
    `2025 Sales` = c(100, 200, 300, 400),
    `Product Revenue` = c(50, 100, 150, 200),
    check.names = FALSE
  )

  block <- new_ggplot_block(
    type = "point",
    x = "2025 Sales",
    y = "Product Revenue"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # The plot should render without error
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("ggplot bar plot works with non-syntactic column - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create data with non-syntactic column names (like penguins_raw)
  test_data <- data.frame(
    `Species Name` = c("A", "A", "B", "B", "C"),
    `Body Mass (g)` = c(3500, 3600, 4200, 4100, 3800),
    check.names = FALSE
  )

  block <- new_ggplot_block(
    type = "bar",
    x = "Species Name"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

test_that("ggplot with non-syntactic fill column - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    x_col = c(1, 2, 3, 4),
    y_col = c(10, 20, 30, 40),
    `Group Type` = c("A", "B", "A", "B"),
    check.names = FALSE
  )

  block <- new_ggplot_block(
    type = "point",
    x = "x_col",
    y = "y_col",
    color = "Group Type"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() test_data))
  )
})

# =============================================================================
# facet block tests with non-syntactic column names
# =============================================================================

test_that("facet_wrap works with a non-syntactic column - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    `Year Group` = rep(c("2023", "2024"), 5),
    check.names = FALSE
  )

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  block <- new_facet_block(facet_type = "wrap", facets = "Year Group")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_true(inherits(result$facet, "FacetWrap"))
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() plot))
  )
})

test_that("facet_grid works with non-syntactic columns - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    x = 1:12,
    y = rnorm(12),
    `Row Var` = rep(c("A", "B"), 6),
    `Col Var` = rep(c("X", "Y", "Z"), each = 4),
    check.names = FALSE
  )

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  block <- new_facet_block(
    facet_type = "grid",
    rows = "Row Var",
    cols = "Col Var"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      expect_true(inherits(result$facet, "FacetGrid"))
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() plot))
  )
})
