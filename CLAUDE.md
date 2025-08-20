# Claude Development Notes for blockr.ggplot

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
- Imports: shiny, blockr.core, ggplot2, dplyr
- All blocks follow blockr.dplyr patterns for UI consistency