# Claude Development Notes for blockr.ggplot

## Block Design Patterns

### UI/Server Patterns for blockr.ggplot

#### Complete Field Handling Pattern

This is the MANDATORY pattern for handling all fields in blockr.ggplot blocks. Follow these patterns exactly to ensure robust behavior with empty/unselected fields.

##### 1. Reactive Values Initialization

**Required Fields (must always have a value):**
```r
# Constructor might receive character() or actual value
r_x <- reactiveVal(x)  # Just pass through - will be populated by updateSelectInput
```

**Optional SelectInput Fields (color, shape, fill, etc):**
```r
# Initialize with "(none)" if empty
r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
```

**Special Optional Fields (y in bar chart = count when empty):**
```r
# Use "(none)" pattern like other optional fields
r_y <- reactiveVal(if (length(y) == 0) "(none)" else y)
```

**TextInput Fields - AVOID FOR NOW:**
```r
# WARNING: TextInput fields like title cause mysterious rendering issues
# AVOID implementing title/label fields until root cause is found
# See "Known Issues" section below
```

##### 2. Update Functions Pattern

**Update functions in observeEvent(cols()) are only needed for column-dependent inputs!**

The observeEvent(cols()) handler updates inputs when the available columns change.

```r
observeEvent(
  cols(),
  {
    # ONLY update column-dependent SelectInputs
    # NEVER filter columns by type - let ggplot2 handle type validation
    updateSelectInput(
      session,
      inputId = "x",
      choices = cols(),
      selected = r_x()  # NEVER check length of reactive value!
    )
    
    updateSelectInput(
      session,
      inputId = "y", 
      choices = c("(none)", cols()),
      selected = r_y()
    )
    
    updateSelectInput(
      session,
      inputId = "color",
      choices = c("(none)", cols()),
      selected = r_color()
    )
    
    # DON'T update non-column-dependent inputs here:
    # - Static selectInputs (position with fixed choices)
    # - TextInputs (title, labels)
    # - CheckboxInputs (flip_coords, show_legend)
    # - SliderInputs (alpha, size)
    # These work fine with just their observeEvent handlers
  }
)
```

**Non-column-dependent inputs only need:**
1. Initial value in UI
2. Reactive value initialization
3. observeEvent to update reactive from input
4. Include in state list

##### 3. UI Initialization

**Required SelectInput:**
```r
selectInput(
  inputId = NS(id, "x"),
  label = "X-axis",
  choices = x,  # Can be character() - updateSelectInput will populate
  selected = x
)
```

**Optional SelectInput:**
```r
selectInput(
  inputId = NS(id, "color"),
  label = "Color By",
  choices = c("(none)", color),
  selected = if (length(color) == 0) "(none)" else color
)
```

**TextInput - AVOID:**
```r
# DO NOT implement textInput fields like title - they cause rendering issues
# Focus on selectInput and checkboxInput which work reliably
```

##### 4. State List - MANDATORY

**ALL reactive values MUST be included in the state list:**

```r
list(
  expr = reactive({ ... }),
  state = list(
    x = r_x,
    y = r_y,
    color = r_color,
    fill = r_fill,
    position = r_position,
    flip_coords = r_flip_coords,
    alpha = r_alpha
    # EVERY argument must be here (except avoided ones like title)!
  )
)
```

##### 5. Expression Building Checks

**Required fields - No validation needed:**
```r
# Assume they always have values
aes_parts <- c(glue::glue("x = {r_x()}"))
```

**Optional SelectInputs - Use != "(none)" check:**
```r
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}
```

**TextInputs - AVOID FOR NOW:**
```r
# DO NOT implement textInput fields - they cause mysterious rendering issues
# Focus on working patterns: selectInput with "(none)" and checkboxInput
```

#### Reactive Values and Input ID Naming
- Use `r_*` prefix for all reactive values (e.g., `r_x`, `r_color`)
- Use simple names for input IDs (e.g., `x`, `color`, not `xcol` or `colorcol`)
- Input IDs in UI must match observer names in server

#### Expression Building Pattern
Always use dynamic aesthetic building:
```r
aes_parts <- c(glue::glue("x = {r_x()}"))
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}
aes_text <- paste(aes_parts, collapse = ", ")
```

## BLOCK COMPLETENESS CHECKLIST

Use this checklist to ensure all blocks are feature-complete and follow consistent patterns:

### 🎨 **UI/UX Standards**
- [ ] **Bootstrap grid layout**: Use `div(class="row")` and `div(class="col-md-6")` structure
- [ ] **Section header**: Add `h4("Block Name Configuration")` 
- [ ] **Organized controls**: Group related inputs logically (aesthetics together, options together)
- [ ] **Professional styling**: Use `class="m-3"` for margins, align controls properly
- [ ] **Help text**: Add `helpText()` to explain complex or non-obvious functionality

### 🔧 **Technical Implementation**
- [ ] **Validation**: Add required field validation with blank plot fallback
- [ ] **Modern patterns**: Use `c("(none)", field)` for optional aesthetics, not old conditional patterns
- [ ] **Complete state**: ALL constructor parameters in state list (except avoided ones like title)
- [ ] **Reactive values**: Use `r_*` prefix, proper initialization with `"(none)"` or appropriate defaults
- [ ] **Update functions**: Only column-dependent inputs need updates in `observeEvent(cols())`
- [ ] **Expression building**: Use `glue::glue()` pattern with conditional aesthetic building
- [ ] **No column type filtering**: Use `cols()` for all column choices, let ggplot2 handle type validation

### ✨ **Feature Completeness**
- [ ] **Core aesthetics**: Implement all relevant aesthetics for the plot type
- [ ] **Advanced options**: Add meaningful customization (position, flip, smooth, etc.)
- [ ] **Optional parameters**: Support both basic and advanced use cases
- [ ] **Proper defaults**: Sensible default values that work out of the box
- [ ] **Error handling**: Graceful degradation when fields are empty/invalid

### 📝 **Documentation Standards**
- [ ] **Constructor docs**: Complete `@param` documentation for all parameters
- [ ] **Clear labels**: Descriptive UI labels that explain what each control does
- [ ] **Consistent naming**: Follow established parameter naming conventions
- [ ] **Help text**: Explain non-obvious behavior (e.g., "empty Y-axis means count")

### 🚫 **Avoid These Patterns**
- [ ] **No textInput fields**: Avoid title/text inputs (see Known Issues)
- [ ] **No old initialization**: Don't use `if (length(x) == 0) "(none)" else x` in UI choices
- [ ] **No missing validation**: Always validate required fields
- [ ] **No tagList UI**: Use proper Bootstrap layout instead of simple tagList
- [ ] **No column type filtering**: Never use `numeric_cols`, `factor_cols` - use `cols()` for all choices

Use this checklist when creating new blocks or updating existing ones to ensure consistency and completeness across all blockr.ggplot blocks.

## TESTING REQUIREMENTS

### 📋 **Test Coverage for All Blocks**

Every block must have comprehensive tests following the established pattern:

```r
test_that("block_name_block constructor", {
  # Test basic constructor
  blk <- new_block_name_block()
  expect_s3_class(blk, c("block_name_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with parameters
  blk <- new_block_name_block(x = "col1", y = "col2")
  expect_s3_class(blk, c("block_name_block", "ggplot_block", "plot_block", "block"))
  
  # Test constructor with all parameters
  blk <- new_block_name_block(x = "col1", y = "col2", color = "col3", ...)
  expect_s3_class(blk, c("block_name_block", "ggplot_block", "plot_block", "block"))
})
```

### 🧪 **Required Test Cases**

1. **Constructor tests**: Basic, with parameters, with all parameters
2. **Parameter variation tests**: Empty, single, multiple values  
3. **Structure tests**: Required components, class hierarchy
4. **Expression generation tests**: Block can generate valid expressions
5. **Edge case tests**: character(0) inputs, boundary values

### ▶️ **Running Tests**

```bash
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "scatter_plot_block")
```

All tests must pass before considering a block complete. Tests serve as both quality assurance and documentation of expected behavior.

## KNOWN ISSUES - AVOID THESE PATTERNS

### Title/TextInput Fields Issue

**Problem:** Adding title fields (or any textInput fields) to blocks causes plots to not render at all.

**Symptoms:**
- Block UI appears correctly
- All other fields work fine  
- Plot area remains blank/empty
- No errors in R console

**What we've tried:**
- Different reactive value initialization (`""`, `NULL`, `character()`)
- With/without `isTruthy()` checks
- With/without UI textInput elements
- Various state list configurations

**Root cause:** Unknown. The issue appears to be:
- blockr.core requires all constructor parameters in state list
- Having textInput-related reactive values in state breaks rendering
- But removing them from state causes blockr errors about missing required state

**Current solution:** 
- **DO NOT implement title/textInput fields in any blocks**
- Use other input types (selectInput, checkboxInput, sliderInput) which work reliably
- Remove `title` parameter from all block constructors

**Future investigation needed:**
- Deep dive into blockr.core state management
- Test with minimal textInput examples
- Check if specific parameter names (like "title") are reserved

## IMPORTANT RULES - NEVER BREAK THESE

### DESCRIPTION File Rules
**NEVER ADD `Author:` and `Maintainer:` FIELDS TO DESCRIPTION FILES**

- Modern R packages use `Authors@R` field only
- The traditional `Author:` and `Maintainer:` fields are OBSOLETE and unnecessary
- R CMD check might complain but these fields should NOT be added
- Only use the `Authors@R` field with `person()` function calls

**Correct format:**
```
Authors@R: c(
    person(given = "Nicolas",
           family = "Bennett", 
           role = c("aut", "cre"),
           email = "nicolas@cynkra.com"),
    person(given = "Christoph",
           family = "Sax",
           role = "aut", 
           email = "christoph@cynkra.com"))
```

**NEVER add these obsolete fields:**
```
Author: Nicolas Bennett [aut, cre], Christoph Sax [aut]  # DON'T DO THIS
Maintainer: Nicolas Bennett <nicolas@cynkra.com>         # DON'T DO THIS
```

## Development Commands

### Test package
```bash
R CMD check --no-manual --no-build-vignettes .
```

### Test specific blocks
```r
library(blockr.ggplot)
blockr.core::serve(
  new_scatter_plot_block(x = "wt", y = "mpg", color = "cyl"),
  data = list(data = mtcars)
)
```

## Architecture Notes

### Plot Block Implementation
- All plot blocks inherit from `plot_block` via `new_plot_block()`
- ggplot2 objects are handled specially in `block_eval.ggplot_block()`
- Plots are rendered via `block_output.ggplot_block()` with proper ggplot2 support

### Block Types Created
1. Enhanced: boxplot_block, histogram_block
2. New Core Charts: bar_chart_block, line_chart_block, scatter_plot_block, pie_chart_block, area_chart_block
3. Statistical: heatmap_block, density_plot_block, violin_plot_block

### Dependencies
- Imports: shiny, blockr.core, ggplot2, dplyr, glue
- All blocks follow blockr.dplyr patterns for UI consistency

## Expression Building Pattern

### ALWAYS use parse(text = glue::glue()) pattern for expressions

**This is the MANDATORY pattern for all blockr.ggplot expressions - NEVER use bquote()**

### Why parse/glue over bquote?

#### Ecosystem Consistency
- **All blockr.dplyr blocks** use `parse(text = glue::glue())` pattern
- **Consistent codebase** makes maintenance easier across all blockr packages
- **Team familiarity** - developers expect this pattern throughout blockr

#### Code Readability & Maintainability
- **Natural syntax**: Write code that looks like actual R code
- **String interpolation**: Use familiar `{variable}` syntax instead of `.(variable)`
- **No complex list management**: No need for separate substitution lists with `as.name()`
- **Debugging friendly**: Can easily `cat()` or `print()` the generated text for debugging

#### Technical Advantages
- **Simpler conditional logic**: Easier to build complex expressions with if/else
- **Flexible composition**: Easy to concatenate multiple parts of expressions
- **Standard R patterns**: Uses well-established `parse()` and string interpolation
- **Less error-prone**: Fewer opportunities for variable substitution mistakes

### Implementation Patterns

**Simple expression:**
```r
expr = reactive({
  text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()})) + ggplot2::geom_point()")
  parse(text = text)[[1]]
})
```

**Conditional aesthetics:**
```r
expr = reactive({
  # Build aesthetics conditionally
  aes_parts <- c(glue::glue("x = {x_col()}"), glue::glue("y = {y_col()}"))
  if (isTruthy(color_col())) {
    aes_parts <- c(aes_parts, glue::glue("colour = {color_col()}"))
  }
  aes_text <- paste(aes_parts, collapse = ", ")
  
  # Build plot
  plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_point()")
  parse(text = plot_text)[[1]]
})
```

**Complex composition with geom arguments:**
```r
expr = reactive({
  # Build aesthetics and geom args separately
  aes_text <- glue::glue("x = {x_col()}, y = {y_col()}")
  geom_args <- glue::glue("alpha = {alpha_val()}, size = {size_val()}")
  
  # Compose final expression
  plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_point({geom_args})")
  
  # Add layers conditionally
  if (add_smooth_val()) {
    plot_text <- glue::glue("({plot_text}) + ggplot2::geom_smooth()")
  }
  
  parse(text = plot_text)[[1]]
})
```

### DEPRECATED: bquote() pattern (DO NOT USE)
```r
# DON'T DO THIS - This pattern is DEPRECATED
expr = reactive({
  bquote(
    ggplot2::ggplot(data, ggplot2::aes(x = .(x), y = .(y), colour = .(col))) + 
      ggplot2::geom_point(),
    list(x = as.name(x_col()), y = as.name(y_col()), col = as.name(color_col()))
  )
})
```

### Migration Strategy
All existing blocks have been converted from bquote to parse/glue pattern. When creating new blocks:

1. **Start with simple case**: Basic aesthetics using `glue::glue()`
2. **Add conditionals**: Use if/else for optional aesthetics or geom arguments  
3. **Build incrementally**: Compose complex expressions by concatenating simpler parts
4. **Test frequently**: Print the generated text to verify correctness
5. **Follow examples**: Use existing converted blocks as templates