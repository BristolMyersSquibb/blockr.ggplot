# Test Coverage Summary

This document summarizes the comprehensive test suite added to the blockr.ggplot package.

## Overview

The test suite has been significantly expanded following the testing patterns described in the blockr testing vignette. All tests use `shiny::testServer()` to test blocks without spinning up full Shiny applications.

## Test Coverage by Block

### 1. ggplot_block (16 test cases)

**File:** `tests/testthat/test-ggplot_block.R`

**Test Categories:**
- Constructor tests (5 tests)
  - Basic constructor validation
  - Constructor with different chart types (point, bar, line, boxplot, violin, density, area, histogram)
  - Constructor with empty inputs
  - Constructor with specific options (position, bins, alpha)

- Server-side tests (11 tests)
  - Input widget updates (type, x, y, color, fill, position, bins)
  - State management and return values
  - Expression evaluation for different chart types:
    - Point charts
    - Bar charts
    - Histograms
    - Line charts
    - Boxplots
    - Density plots
  - Color and fill aesthetics
  - Empty optional aesthetics handling
  - "(none)" selections handling

### 2. facet_block (13 test cases)

**File:** `tests/testthat/test-facet_block.R`

**Test Categories:**
- Constructor tests (5 tests)
  - Basic constructor validation
  - Constructor with facet_type parameter (wrap/grid)
  - Constructor with facets parameter
  - Constructor with rows and cols parameters
  - Constructor with all layout options

- Server-side tests (8 tests)
  - Input widget updates (facet_type, facets, rows, cols, scales, labeller)
  - State management and return values
  - Expression evaluation with facet_wrap
  - Expression evaluation with facet_grid
  - Multiple facet variables in wrap
  - Scales options (fixed, free, free_x, free_y)
  - Layout options (ncol/nrow)
  - Empty facets handling

### 3. theme_block (15 test cases) - NEW

**File:** `tests/testthat/test-theme_block.R`

**Test Categories:**
- Constructor tests (4 tests)
  - Basic constructor validation
  - Constructor with various base themes
  - Constructor with color parameters
  - Constructor with all parameters

- Server-side tests (11 tests)
  - Input widget updates (base_theme, legend_position, grid options, font options)
  - State management and return values
  - Expression evaluation with:
    - Base themes (minimal, classic, gray, bw, etc.)
    - Legend position options
    - Grid visibility options
    - Panel border options
    - Font settings (size, family)
    - Background colors (panel, plot)
    - Grid colors
    - "Auto" value handling
    - Multiple options combined

### 4. grid_block (15 test cases) - NEW

**File:** `tests/testthat/test-grid_block.R`

**Test Categories:**
- Constructor tests (6 tests)
  - Basic constructor validation
  - Constructor with ncol/nrow parameters
  - Constructor with annotations (title, subtitle, caption)
  - Constructor with tag_levels options
  - Constructor with guides options
  - Constructor with all options

- Server-side tests (9 tests)
  - Input widget updates (ncol, nrow, title, subtitle, caption, tag_levels, guides)
  - State management and return values
  - Expression evaluation with:
    - Two plots
    - Three plots
    - Four plots
    - Single plot
    - Layout options (ncol, nrow)
    - Annotations (title, subtitle, caption)
    - Tag levels (A, a, 1, I, i)
    - Guides option (auto, collect, keep)
    - All options combined

## Testing Patterns Used

All tests follow the patterns described in the blockr testing vignette:

1. **Class validation** - Verify blocks are constructed with correct S3 classes
2. **Input widget testing** - Use `session$setInputs()` to simulate user input
3. **State management** - Verify `session$returned$state` contains correct reactive values
4. **Expression evaluation** - Use `eval(session$returned$expr())` to validate generated code
5. **Data passing** - Pass required data/plots via `args` parameter in `testServer()`
6. **Empty state handling** - Test blocks with empty/default values

## Test Metrics

| Block | Previous Tests | New Tests | Total Tests | Status |
|-------|----------------|-----------|-------------|--------|
| ggplot_block | 5 | 11 | 16 | Enhanced |
| facet_block | 5 | 8 | 13 | Enhanced |
| theme_block | 0 | 15 | 15 | NEW |
| grid_block | 0 | 15 | 15 | NEW |
| **TOTAL** | **10** | **49** | **59** | **+490%** |

## Coverage Improvements

### Before
- Only constructor tests
- No server-side testing
- No expression evaluation
- No state management testing
- 2 of 4 blocks tested

### After
- Complete constructor coverage
- Comprehensive server-side testing
- Expression evaluation for all chart types
- State management validation
- Input widget interaction testing
- All 4 major blocks tested

## Running the Tests

To run the test suite:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-ggplot_block.R")

# Run with coverage report
covr::package_coverage()
```

## Future Enhancements

Potential areas for additional testing:

1. **Integration tests** - Test blocks working together in pipelines
2. **Error handling** - More comprehensive error condition testing
3. **Edge cases** - Test with unusual data inputs
4. **UI rendering** - Test that UIs render without errors
5. **Performance tests** - Test with large datasets
6. **Validation tests** - Test dat_valid functions

## Notes

All tests are designed to run quickly without requiring a full Shiny app to be started. This makes the test suite fast and suitable for continuous integration.
