# Changelog

## blockr.ggplot (development version)

### Design system

- The blocks use blockr.ui for their controls and tokens, and no longer
  import blockr.dplyr. The vendored settings-band and DrilldownConfig
  copies are gone; their unscoped rules restyled every gear tray on a
  board.
- ggplot block: chart-type tiles and the mapping stay on the face, the
  tiles in the accent tint. “Add mapping” opens the shared menu. The
  rest is in the gear tray, where “Confidence band” shows only while a
  trend line is on, bins and opacity are number fields, and fixed-set
  selects show their label only.
- facet block: Wrap or Grid and the facet columns on the face, the rest
  in the gear. Direction is Across or Down.
- theme block: base theme, legend and palettes on the face. The gear has
  colour fields (a swatch and the hex value, opening the browser’s
  picker) and Auto/Show/Hide segmented controls for grid lines and
  border.
- grid block: the face is the plot; layout, the layout preview and the
  titles are in the gear.
- The layout previews are drawn with the design tokens and follow the
  dark scheme. The plot image stays light in the dark scheme.

### Improvements

- The ggplot, facet and theme blocks now build their expressions as
  language objects
  ([`bquote()`](https://rdrr.io/r/base/bquote.html)/[`call()`](https://rdrr.io/r/base/call.html)/[`as.call()`](https://rdrr.io/r/base/call.html))
  instead of assembling and re-parsing strings, and refer to their input
  as `.(data)`. Generated code names the upstream block directly
  (`plot + ggplot2::theme_bw()`) rather than wrapping it in
  `with(list(data = plot), ...)`, and non-syntactic column names are
  handled by [`as.name()`](https://rdrr.io/r/base/name.html) rather than
  manual backticking. This drops the `glue` dependency.
- Text inputs commit on Enter or blur with an “Enter ↵” confirm chip
  instead of auto-submitting on a 300ms debounce.
- The facet and grid blocks no longer show a yellow warning banner when
  unconfigured: the facet “Facet by” field carries the amber
  required-empty cue instead, and the layout preview waits until there
  is something to lay out.

## blockr.ggplot 0.1.0

CRAN release: 2025-12-18

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
