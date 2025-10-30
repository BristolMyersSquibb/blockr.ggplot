# Screenshot Generation for blockr.ggplot

This directory contains tools for generating screenshots of all blockr.ggplot blocks for use in pkgdown documentation and vignettes.

## Files

- `validate-screenshot.R`: Functions for generating screenshots of individual blocks or batches
- `generate_all.R`: Script that generates screenshots for all blocks at once

## Usage

### Generate all screenshots

From the root of the blockr.ggplot package:

```r
source("dev/screenshots/generate_all.R")
```

This will:
- Generate 14 screenshots for all block variations
- Save them to `man/figures/` directory
- Show progress for each screenshot

### Generate a screenshot for a single block

```r
source("dev/screenshots/validate-screenshot.R")

# Example: Generate screenshot for scatter plot
result <- validate_block_screenshot(
  block = new_ggplot_block(
    chart_type = "point",
    x = "mpg",
    y = "hp",
    color = "cyl"
  ),
  data = mtcars,
  filename = "my-scatter-plot.png",
  output_dir = "man/figures"
)

if (result$success) {
  cat("Screenshot saved to:", result$path, "\n")
} else {
  cat("Failed:", result$error, "\n")
}
```

## Block Coverage

The screenshot generation covers all blockr.ggplot blocks:

### ggplot Block (10 variations)
- **scatter** (`chart_type = "point"`) - Scatter plot with color and size aesthetics
- **bar** (`chart_type = "bar"`) - Bar chart with fill aesthetic
- **line** (`chart_type = "line"`) - Line chart with color by group
- **boxplot** (`chart_type = "boxplot"`) - Box plot with fill aesthetic
- **violin** (`chart_type = "violin"`) - Violin plot with fill aesthetic
- **density** (`chart_type = "density"`) - Density plot with alpha blending
- **area** (`chart_type = "area"`) - Area chart with alpha blending
- **histogram** (`chart_type = "histogram"`) - Histogram with fill aesthetic
- **pie** (`chart_type = "pie"`) - Pie chart
- **donut** (`chart_type = "pie"` with `donut_hole`) - Donut chart

### Facet Block (2 variations)
- **wrap** (`facet_type = "wrap"`) - Flexible grid layout
- **grid** (`facet_type = "grid"`) - Rows × columns matrix

### Theme Block
- Theme customization with 20+ available themes

### Grid Block
- Multi-plot composition using patchwork

## Adding a new chart type

To add a new chart type screenshot:

1. Open `generate_all.R`
2. Add a new validation block with your chart configuration
3. Use descriptive filename (e.g., `block-ggplot-yourtype.png`)
4. Run the script

Example:

```r
cat("15/15 - ggplot block: scatter3d\n")
validate_block_screenshot(
  block = new_ggplot_block(
    chart_type = "scatter3d",
    x = "mpg",
    y = "hp",
    z = "wt"
  ),
  data = mtcars,
  filename = "block-ggplot-scatter3d.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 2,
  verbose = FALSE
)
```

## Requirements

- `shinytest2` package: `install.packages("shinytest2")`
- `blockr.core` package
- `blockr.ggplot` package (loaded from development if available)
- `ggplot2` and dependencies

## Output

Screenshots are saved to `man/figures/` with consistent naming:
- `block-ggplot-scatter.png`
- `block-ggplot-bar.png`
- `block-ggplot-line.png`
- `block-facet.png`
- `block-facet-grid.png`
- `block-theme.png`
- `block-grid.png`
- etc.

These are referenced in:
- Package vignettes (`vignettes/blockr-ggplot-showcase.Rmd`)
- README files
- pkgdown documentation

## Technical Details

The screenshot system uses `shinytest2` (not `webshot2`) for more reliable screenshot capture. Each block is:

1. Wrapped in a temporary Shiny app
2. Launched with `shinytest2::AppDriver`
3. Given time to render (default 2 seconds)
4. Captured at 800×600 pixels
5. Saved to `man/figures/`

The system automatically loads the package from the development directory to ensure latest changes are captured.
