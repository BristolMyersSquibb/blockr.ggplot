# Test Suite Documentation

## Overview

This test suite provides comprehensive coverage for all major blocks in blockr.ggplot:
- `ggplot_block` (23 tests)
- `facet_block` (20 tests) 
- `grid_block` (20 tests)
- `theme_block` (30 tests)

Total: **93 tests**

## Running Tests

To run the entire test suite:
```r
devtools::test()
```

To run tests for a specific file:
```r
devtools::test_active_file("tests/testthat/test-ggplot_block.R")
```

To run a specific test:
```r
testthat::test_file("tests/testthat/test-ggplot_block.R", 
                    filter = "ggplot_block constructor")
```

## Test Structure

All tests follow the testing guide patterns from `dev/testing-guide.md`:

### Pattern 1: Expression Testing with expr_server
```r
test_that("block generates correct expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()
      
      # Test expression structure
      expr_result <- session$returned$expr()
      expr_str <- paste0(deparse(expr_result), collapse = "")
      expect_true(grepl("geom_point", expr_str))
      
      # Test state
      expect_equal(session$returned$state$type(), "point")
    }
  )
})
```

### Pattern 2: Plot Output Testing with block_server
```r
test_that("block creates valid plot object", {
  block <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()
      
      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomPoint")
    }
  )
})
```

## Test Coverage

### ggplot_block (test-ggplot_block.R)
- Constructor validation
- Expression generation for all chart types (point, bar, line, histogram, density, etc.)
- Chart type switching via UI
- Aesthetic mappings (color, fill, size, shape, etc.)
- Optional aesthetics with "(none)" values
- Dynamic UI updates
- Missing required fields handling
- Block server integration

### facet_block (test-facet_block.R)
- Constructor validation
- facet_wrap expression generation (single and multiple variables)
- facet_grid expression generation (rows, cols, both)
- Layout options (ncol, nrow, scales, labeller, direction)
- Empty facet handling
- Switching between wrap and grid modes
- Block server integration
- Input validation (requires ggplot object)

### grid_block (test-grid_block.R)
- Constructor validation
- wrap_plots expression generation (single and multiple plots)
- Layout configuration (ncol, nrow, guides)
- Annotations (title, subtitle, caption, tag_levels)
- Empty parameter handling
- State reactive updates
- UI input changes
- NULL input filtering
- Block server integration
- Input validation (requires at least 1 plot)

### theme_block (test-theme_block.R)
- Constructor validation
- Base theme selection (minimal, classic, gray, etc.)
- Auto mode (preserving upstream theme)
- Color customizations (panel_bg, plot_bg, grid_color)
- Typography options (base_size, base_family)
- Grid line visibility (show_major_grid, show_minor_grid)
- Panel border options
- Legend position
- Combined theme options
- State reactive updates
- UI input changes
- Empty/auto value handling
- Special gray theme handling
- Block server integration

## Dependencies

Tests require:
- testthat (>= 3.0.0)
- shiny (for testServer)
- blockr.core
- ggplot2
- patchwork (for grid_block tests)

All dependencies are already specified in DESCRIPTION.

## Notes

- All tests use `testServer()` as recommended in the testing guide
- No shinytest2 usage (per guide recommendations)
- Tests follow standard testthat conventions
- Expression tests verify both structure and state
- Block server tests verify actual plot output
- Tests cover edge cases (empty values, missing required fields, etc.)

## Troubleshooting

If tests fail:

1. **Check blockr.core version**: Ensure blockr.core >= 0.1.1
2. **Check expression format**: Use `deparse()` and `paste0(..., collapse = "")` to inspect
3. **Check state reactives**: Verify reactive values are accessible via `()`
4. **Check block_server args**: Ensure format is `list(x = block, data = list(data = function() df))`
5. **Adapt tests if needed**: Per instructions, adapt tests rather than R code if failures occur

## Future Enhancements

Potential additions to test suite:
- Integration tests combining multiple blocks
- Performance benchmarks
- Edge case coverage for unusual data types
- Visual regression tests (if needed, using shinytest2 sparingly)
