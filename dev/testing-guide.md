# Testing Guide for blockr.ggplot

## Overview

This document outlines the **TWO-TIER testing strategy** for blockr.ggplot. Understanding WHEN to use each tool is critical for fast, maintainable tests.

This guide is adapted from blockr.dplyr and applies the same principles to ggplot blocks.

## Testing Philosophy

1. **DEFAULT: Use testServer** - For all Shiny reactivity, UI interactions, and module logic
2. **Use unit tests for pure functions** - Expression builders, helpers, parsers
3. **Speed matters** - Fast tests = fast feedback = better development
4. **shinytest2 is almost never needed** - Only for pure visual UI checks with no server impact

---

## ⚠️ IMPORTANT: Two-Tier Strategy

**99% of your tests should be:**
- **Tier 1: Unit Tests** - Pure R functions (no Shiny)
- **Tier 2: testServer** - ALL Shiny reactivity and UI interactions

**shinytest2 is rarely needed** - See bottom of this guide for the exceptional cases.

---

## The Two Main Testing Layers

### Layer 1: Unit Tests (Pure Functions, No Shiny)

**Files**: Portions of test files for helper functions
**Speed**: ⚡⚡⚡ Very fast (~0.1-0.3s per test)

**What it CAN test**:
- ✅ Pure functions (helper functions, expression parsers, SVG generators)
- ✅ Constructor validation
- ✅ Expression generation correctness (without reactive context)
- ✅ Expression evaluation (execute the generated ggplot code)
- ✅ Edge cases (empty data, invalid inputs)
- ✅ Logic that doesn't need Shiny reactivity

**What it CANNOT test**:
- ❌ Reactive values and reactive contexts
- ❌ Shiny modules and moduleServer
- ❌ UI rendering or user interactions
- ❌ State management across reactive updates

**Example for blockr.ggplot**:
```r
test_that("ggplot_block constructor creates correct block class", {
  blk <- new_ggplot_block()
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("ggplot_block constructor with all parameters", {
  blk <- new_ggplot_block(
    type = "point",
    x = "mpg",
    y = "hp",
    color = "cyl",
    fill = "gear"
  )
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("facet_block handles empty inputs", {
  blk <- new_facet_block(
    facets = character(),
    rows = character(),
    cols = character()
  )
  expect_s3_class(
    blk,
    c("facet_block", "ggplot_transform_block", "transform_block", "block")
  )
})
```

---

### Layer 2: testServer (ALL Shiny Interactions - USE THIS)

**Files**: `test-ggplot_block.R`, `test-facet_block.R`, and other test files
**Speed**: ⚡⚡ Fast (~0.2-0.5s per test)

**What it CAN test**:
- ✅ Ggplot block reactive logic
- ✅ Reactive values and contexts
- ✅ State management
- ✅ Expression generation in reactive context
- ✅ Module server functions
- ✅ Plot rendering (by evaluating expressions)
- ✅ **UI interactions** (button clicks, input changes via `session$setInputs()`)
- ✅ **Dynamic UI updates** (showing/hiding aesthetic inputs based on chart type)
- ✅ **Reactive auto-updates** (column updates when data changes)
- ✅ **ALL edge cases and error handling**

**When to use**:
- Testing Shiny modules (moduleServer)
- Testing reactive behavior
- Testing UI input changes and their effects
- Testing dynamic UI visibility (aesthetic fields shown/hidden by chart type)
- Testing state management
- **This should be your default for testing ANY Shiny logic**

**Two critical patterns for blockr.ggplot**:

**Pattern 1: Testing Expression Generation (expr_server)**
```r
test_that("ggplot_block generates scatter plot expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,           # Test expr_server directly
    args = list(data = input_data),  # Plot blocks need data argument
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression structure
      expr_result <- result$expr()
      expr_str <- deparse(expr_result)
      expect_true(grepl("ggplot2::ggplot", expr_str))
      expect_true(grepl("geom_point", expr_str))
      expect_true(grepl("x = mpg", expr_str))
      expect_true(grepl("y = hp", expr_str))

      # Can also evaluate to verify plot creation
      plot_result <- eval(expr_result, envir = list(data = mtcars))
      expect_s3_class(plot_result, "ggplot")

      # Test state
      expect_true(is.reactive(result$state$type))
      expect_equal(result$state$type(), "point")
      expect_equal(result$state$x(), "mpg")
      expect_equal(result$state$y(), "hp")
    }
  )
})
```

**Pattern 2: Testing Plot Generation (block_server) - PREFERRED**
```r
test_that("ggplot_block creates valid scatter plot - testServer", {
  block <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),  # Use block_server
    {
      session$flushReact()
      result <- session$returned$result()  # Get actual plot object

      # Test the actual plot object
      expect_s3_class(result, "ggplot")

      # Check plot data
      expect_equal(nrow(result$data), nrow(mtcars))

      # Check aesthetics
      expect_true("x" %in% names(result$mapping))
      expect_true("y" %in% names(result$mapping))

      # Check geom layer
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomPoint")
    },
    args = list(x = block, data = list(data = function() mtcars))  # Different args format!
  )
})
```

**Pattern 3: Testing UI Interactions**
```r
test_that("ggplot_block updates chart type dynamically", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Initially point chart
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("geom_point", expr_str))

      # Change to bar chart via UI interaction
      session$setInputs(type = "bar")
      session$flushReact()

      # Verify expression updated
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("geom_bar|geom_col", expr_str))
      expect_false(grepl("geom_point", expr_str))

      # Verify state updated
      expect_equal(session$returned$state$type(), "bar")
    }
  )
})

test_that("facet_block switches between wrap and grid", {
  input_data <- reactive({
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp))
  })

  blk <- new_facet_block(facet_type = "wrap", facets = "cyl")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Initially facet_wrap
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("facet_wrap", expr_str))

      # Switch to grid
      session$setInputs(facet_type = "grid")
      session$setInputs(rows = "cyl")
      session$flushReact()

      # Verify expression updated
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("facet_grid", expr_str))
      expect_false(grepl("facet_wrap", expr_str))
    }
  )
})
```

**When to use each pattern**:
- **Use expr_server** when testing expression generation, reactive state updates, or UI logic
- **Use block_server** when testing actual plot rendering behavior (PREFERRED for plot correctness tests)

**Key differences**:
- `expr_server`: Returns `list(expr = reactive(...), state = list(...))` - test expression structure
- `block_server`: Returns `list(result = reactive(...), ...)` - test actual plot output
- `block_server` uses different args format: `list(x = block, data = list(data = function() df))`
- `block_server` is the same pattern used by the framework internally, so it's more reliable
- **DO NOT use manual `eval()`** - use `block_server` pattern instead when testing plot output

**Testing dynamic UI visibility**:
```r
test_that("ggplot_block hides y-axis for histogram", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "histogram", x = "mpg")

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Histogram should not require y-axis
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("geom_histogram", expr_str))
      expect_false(grepl("y = ", expr_str))

      # Verify y is "(none)" in state
      expect_equal(session$returned$state$y(), "(none)")
    }
  )
})

test_that("ggplot_block shows density_alpha for density plots", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(
    type = "density",
    x = "mpg",
    density_alpha = 0.7
  )

  testServer(
    blk$expr_server,
    args = list(data = input_data),
    {
      session$flushReact()

      # Density should use fixed alpha parameter
      expr_str <- deparse(session$returned$expr())
      expect_true(grepl("geom_density\\(alpha = 0.7\\)", expr_str))

      # Verify state
      expect_equal(session$returned$state$density_alpha(), 0.7)
    }
  )
})
```

---

## Note on shinytest2

**We don't use shinytest2.** All Shiny testing is done with testServer, which can simulate UI interactions via `session$setInputs()`.

### When would shinytest2 be useful? (Very rare cases)

ShinyTest2 performs **full browser-based testing** - it launches a real Shiny app in a headless browser (Chrome/Chromium) and simulates user clicks, typing, etc.

**Use shinytest2 ONLY for:**
- ✅ **Visual regression testing** - Catching CSS/layout changes via screenshots
- ✅ **Complex JavaScript interactions** - Custom JS widgets that testServer can't simulate
- ✅ **Browser-specific rendering bugs** - Issues that only appear in actual browser rendering
- ✅ **Third-party widget integration** - External JS libraries with complex DOM manipulation
- ✅ **Interactive plot features** - Advanced ggplot2 interactive features with plotly/ggiraph

**DO NOT use shinytest2 for:**
- ❌ Testing reactive logic (use testServer)
- ❌ Testing plot generation (use testServer with block_server)
- ❌ Testing module interactions (use testServer)
- ❌ Testing input changes and their effects (use testServer with session$setInputs)
- ❌ Testing button clicks, checkboxes, dropdowns (use testServer)
- ❌ Testing dynamic UI visibility (use testServer)

**Why we avoid shinytest2:**
- 🐌 **Very slow** (~2-5 seconds per test vs 0.2s with testServer)
- 🔧 **Brittle** - Breaks on minor UI changes, CSS updates, timing issues
- 🖥️ **Platform-dependent** - Requires Chrome/Chromium installation
- 🐛 **Harder to debug** - Failures are less clear than testServer errors

**Key insight:** If you can test it with `session$setInputs()` in testServer, that's always better than shinytest2.

---

## blockr.ggplot-Specific Testing Tips

### Testing Chart Type Switching
ggplot_block allows users to switch between multiple chart types (point, bar, line, histogram, etc.). Test that:
- Expression updates correctly when chart type changes
- Chart-specific options show/hide appropriately
- Aesthetic mappings are preserved when valid for the new chart type
- Invalid aesthetics are handled gracefully

### Testing Aesthetic Mappings
- Test that optional aesthetics work with `"(none)"` values
- Test that `allow_empty_state` is properly configured
- Test dynamic aesthetic visibility based on chart type
- Test factor conversions (shape, fill for histograms, etc.)

### Testing Facet Blocks
- Test both `facet_wrap` and `facet_grid` modes
- Test dynamic column updates from ggplot data
- Test layout preview generation
- Test empty facet variables (should return data unchanged)

### Testing Plot Transform Blocks
- Remember that ggplot transform blocks accept ggplot objects as input
- Test `dat_valid()` to ensure input validation works
- Test that the block adds layers to existing plots correctly

### Common Issues to Test For
1. **Empty/None values**: Test aesthetics set to `"(none)"` or `character()`
2. **Data-dependent UI**: Test that column selectors update when input data changes
3. **Expression parsing**: Test that `parse(text = glue::glue(...))` generates valid ggplot code
4. **Factor conversions**: Test that shape/fill conversions to factors work correctly
5. **Theme layering**: Test that theme_minimal() is applied correctly

---

## Example Test File Structure

```r
# test-ggplot_block.R

test_that("ggplot_block constructor", {
  # Unit test - fast constructor validation
  blk <- new_ggplot_block()
  expect_s3_class(blk, c("ggplot_block", "ggplot_block", "plot_block", "block"))
})

test_that("ggplot_block generates scatter plot", {
  # testServer - expression generation
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(blk$expr_server, args = list(data = input_data), {
    session$flushReact()
    expr_str <- deparse(session$returned$expr())
    expect_true(grepl("geom_point", expr_str))
  })
})

test_that("ggplot_block creates valid plot object", {
  # testServer - actual plot rendering
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

test_that("ggplot_block switches chart types", {
  # testServer - UI interaction
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(blk$expr_server, args = list(data = input_data), {
    session$flushReact()

    # Change chart type
    session$setInputs(type = "bar")
    session$flushReact()

    expr_str <- deparse(session$returned$expr())
    expect_true(grepl("geom_bar|geom_col", expr_str))
    expect_equal(session$returned$state$type(), "bar")
  })
})
```

---

## Summary

- **99% of tests should use testServer** - It handles all Shiny reactivity and UI interactions
- **Constructor tests are unit tests** - Fast validation of block creation
- **Never use shinytest2** unless you have a very specific visual/JS requirement
- **Use block_server pattern** for testing actual plot output (preferred over eval)
- **Test dynamic UI behavior** via session$setInputs() in testServer
- **Speed is critical** - Keep tests fast to maintain developer productivity
