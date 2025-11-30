# Tests for non-syntactic column names (backtick handling)
# See: https://github.com/BristolMyersSquibb/blockr.ggplot/issues/69

# =============================================================================
# Utility function tests
# =============================================================================

test_that("needs_backticks correctly identifies non-syntactic names", {
  # Standard R names don't need backticks

  expect_false(needs_backticks("normal_name"))
  expect_false(needs_backticks("x"))
  expect_false(needs_backticks("my.var"))


  # Names starting with numbers need backticks
  expect_true(needs_backticks("2025"))
  expect_true(needs_backticks("2025 Sales"))
  expect_true(needs_backticks("123abc"))

  # Names with spaces need backticks

  expect_true(needs_backticks("Product Category"))
  expect_true(needs_backticks("First Name"))

  # Names with special characters need backticks
  expect_true(needs_backticks("col-name"))
  expect_true(needs_backticks("col/name"))
  expect_true(needs_backticks("col@name"))

  # Empty and NA don't need backticks
  expect_false(needs_backticks(""))
  expect_false(needs_backticks(NA_character_))
})

test_that("needs_backticks works with vectors", {
  result <- needs_backticks(c("normal", "2025 Sales", "col-name", "", NA))
  expect_equal(result, c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("backtick_if_needed wraps non-syntactic names", {
  # Standard names unchanged
  expect_equal(backtick_if_needed("normal_name"), "normal_name")
  expect_equal(backtick_if_needed("x"), "x")

  # Non-syntactic names get backticks
  expect_equal(backtick_if_needed("2025 Sales"), "`2025 Sales`")
  expect_equal(backtick_if_needed("Product Category"), "`Product Category`")
  expect_equal(backtick_if_needed("col-name"), "`col-name`")
})

test_that("backtick_if_needed works with vectors", {
  result <- backtick_if_needed(c("normal", "2025 Sales", "col-name"))
  expect_equal(result, c("normal", "`2025 Sales`", "`col-name`"))
})

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
# Direct facet code tests (evaluating expression directly)
# =============================================================================

test_that("facet_wrap formula with non-syntactic column is valid", {
  skip_if_not_installed("ggplot2")

  # Test that backtick_if_needed produces valid facet formulas
  facet_var <- "Year Group"
  facet_formula_str <- paste0("~", backtick_if_needed(facet_var))

  # This should create a valid formula
  expect_equal(facet_formula_str, "~`Year Group`")

  # Verify ggplot2 can parse this
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    `Year Group` = rep(c("2023", "2024"), 5),
    check.names = FALSE
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  # Evaluate the facet expression directly
  facet_expr <- parse(text = sprintf(
    "p + ggplot2::facet_wrap(%s)",
    facet_formula_str
  ))[[1]]

  result <- eval(facet_expr)
  expect_true(inherits(result, "ggplot"))
  expect_true(inherits(result$facet, "FacetWrap"))
  expect_no_error(ggplot2::ggplot_build(result))
})

test_that("facet_grid formula with non-syntactic columns is valid", {
  skip_if_not_installed("ggplot2")

  # Test row and column variables with non-syntactic names
  row_var <- "Row Var"
  col_var <- "Col Var"
  grid_formula_str <- paste0(
    backtick_if_needed(row_var),
    " ~ ",
    backtick_if_needed(col_var)
  )

  expect_equal(grid_formula_str, "`Row Var` ~ `Col Var`")

  test_data <- data.frame(
    x = 1:12,
    y = rnorm(12),
    `Row Var` = rep(c("A", "B"), 6),
    `Col Var` = rep(c("X", "Y", "Z"), each = 4),
    check.names = FALSE
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  # Evaluate the facet expression directly
  facet_expr <- parse(text = sprintf(
    "p + ggplot2::facet_grid(%s)",
    grid_formula_str
  ))[[1]]

  result <- eval(facet_expr)
  expect_true(inherits(result, "ggplot"))
  expect_true(inherits(result$facet, "FacetGrid"))
  expect_no_error(ggplot2::ggplot_build(result))
})
