# Claude Development Notes for blockr.ggplot

## Block Design Patterns

### UI/Server Patterns for blockr.ggplot

#### Reactive Values Naming
- Use `r_*` prefix for all reactive values (e.g., `r_x`, `r_color`, not `x_col`)
- Keep names concise and meaningful

#### Input ID Naming
- Use simple names without suffixes (e.g., `x`, `color`, not `xcol` or `colorcol`)
- Input IDs in UI must match observer names in server

#### Optional vs Required Fields

**Required Fields:**
- No "(none)" option in choices
- Use standard validation: `if (!isTruthy(r_x())) return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))`
- Examples: x-axis in bar charts, x/y in scatter plots

**Optional Aesthetic Fields (color, shape, fill):**
- Initialize empty parameters to "(none)": `r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)`
- Use "(none)" string literal for "not selected" state
- Check with string comparison: `if (r_color() != "(none)") { ... }`
- UI choices: `c("(none)", cols())` in updateSelectInput
- Initial UI can be empty, will be populated by updateSelectInput

**Special Optional Fields:**
- Y-axis in bar charts: Empty means "count", not "(none)"
- Use empty string "" and `isTruthy()` check for these cases

#### Expression Building Pattern
Always use dynamic aesthetic building:
```r
aes_parts <- c(glue::glue("x = {r_x()}"))
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}
aes_text <- paste(aes_parts, collapse = ", ")
```

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