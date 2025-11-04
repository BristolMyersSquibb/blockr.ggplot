# Test Verification Instructions

## Environment Setup

Since R is not available in the current development environment, these tests need to be verified in an R environment with the following setup:

```r
# Install dependencies
install.packages(c("devtools", "testthat", "shiny", "ggplot2", "patchwork"))

# Install blockr.core (from GitHub)
remotes::install_github("cynkra/blockr.core")

# Load the package
devtools::load_all()
```

## Running the Test Suite

### Full Test Suite
```r
# Run all tests
devtools::test()
```

### Individual Test Files
```r
# Test ggplot_block
testthat::test_file("tests/testthat/test-ggplot_block.R")

# Test facet_block
testthat::test_file("tests/testthat/test-facet_block.R")

# Test grid_block
testthat::test_file("tests/testthat/test-grid_block.R")

# Test theme_block
testthat::test_file("tests/testthat/test-theme_block.R")
```

### Watch Mode (for iterative testing)
```r
testthat::test_local(path = ".", reporter = "progress")
```

## Expected Behavior

All tests should pass without errors. If any tests fail:

### Per Instructions
> After creating a test, run it and make sure it passes. If it fails adapt the test, and never the code in R, to make the test pass.

This means:
1. ✅ Adapt the **test** to match actual behavior
2. ❌ Do NOT modify the **R code** in the blocks

### Common Adjustments

If tests fail, likely adjustments needed:

1. **Expression String Matching**: Adjust regex patterns to match actual generated expressions
2. **State Structure**: Verify reactive state structure matches implementation
3. **Block Server Args**: Ensure data structure matches what block_server expects
4. **Class Hierarchy**: Verify S3 class inheritance chain matches implementation

### Example Adjustment

If this test fails:
```r
expect_true(grepl("geom_point", expr_str))
```

Inspect the actual expression:
```r
# In test, add:
print(expr_str)
```

Then adjust the pattern to match the actual output.

## Test Pattern Verification

### Pattern 1: expr_server Tests
These should:
- Create reactive input data
- Call testServer with expr_server
- Verify expression structure via deparse/grepl
- Verify state reactive values

### Pattern 2: block_server Tests
These should:
- Create block with parameters
- Call testServer with block_server via get_s3_method
- Pass args as list(x = block, data = list(data = function() df))
- Verify actual plot object properties

## Debugging Failed Tests

If tests fail:

1. **Check package versions**:
   ```r
   packageVersion("blockr.core")
   packageVersion("ggplot2")
   packageVersion("shiny")
   ```

2. **Inspect block structure**:
   ```r
   blk <- new_ggplot_block()
   str(blk)
   names(blk)
   ```

3. **Test expr_server manually**:
   ```r
   input_data <- reactive(mtcars)
   result <- blk$expr_server("test", input_data)
   # Inspect result structure
   ```

4. **Check expression output**:
   ```r
   testServer(
     blk$expr_server,
     args = list(data = reactive(mtcars)),
     {
       session$flushReact()
       expr <- session$returned$expr()
       print(deparse(expr))
       print(class(expr))
     }
   )
   ```

## Success Criteria

Tests are successful when:
- ✅ All 93 tests pass
- ✅ No errors or warnings
- ✅ Coverage includes all major functionality
- ✅ Tests follow guide patterns correctly

## Next Steps After Verification

Once tests pass:
1. Run full test suite in CI/CD pipeline
2. Add test coverage reporting (covr package)
3. Integrate with automated checks
4. Document any test-specific quirks or known issues

## Known Limitations

Based on the testing guide:
- Some expression patterns may vary slightly from expectations
- Block server tests depend on internal blockr.core implementation
- Expression deparse output can vary with R version/formatting
- Reactive context in testServer has some limitations

## Contact

If tests fail and adjustments are needed beyond simple pattern matching, consult:
- Testing guide: `dev/testing-guide.md`
- blockr.core documentation
- Issue tracker for blockr.ggplot
