# Changelog

## blockr.ggplot 0.1.0

Initial CRAN release.

### Features

#### Visualization Blocks

- [`new_ggplot_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_ggplot_block.md):
  Universal ggplot block with selectable chart types
  - Scatter plots, bar charts, line charts, pie charts
  - Boxplots, violin plots, histograms, density plots, area charts
  - Dynamic aesthetic mapping (x, y, color, fill, size, shape, etc.)
  - Position adjustments and chart-specific options

#### Customization Blocks

- [`new_theme_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_theme_block.md):
  Apply and customize ggplot2 themes
  - Support for built-in themes and extension packages (cowplot,
    ggthemes, ggpubr)
  - Background colors, typography, grid lines, legend position
  - Color palette selection (viridis scales)
- [`new_facet_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_facet_block.md):
  Add faceting to plots
  - facet_wrap and facet_grid support
  - Layout controls (ncol, nrow, scales, direction)
  - Visual preview of facet arrangement

#### Layout Blocks

- [`new_grid_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_grid_block.md):
  Combine multiple plots using patchwork
  - Grid layout with ncol/nrow controls
  - Plot annotations (title, subtitle, caption)
  - Auto-tagging (A, B, C or 1, 2, 3)
  - Legend collection options

### Documentation

- Full documentation for all exported functions
- Package website with examples
