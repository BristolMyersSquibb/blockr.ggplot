# Changelog

## blockr.ggplot (development version)

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
- Text inputs in the settings band (grid block title, subtitle, caption)
  now commit on Enter or blur with an “Enter ↵” confirm chip instead of
  auto-submitting on a 300ms debounce, following the design-system
  text-commit convention (shared drilldown engine, re-vendored from
  blockr.viz).
- Design-token fallback fixes: slider accent and preview-status colors
  now fall back to the canonical design-system values (`#2563eb`
  primary, `#16a34a` success, `#b45309` warning text, `#dc2626` danger).
- The facet and grid blocks no longer show a yellow warning banner when
  unconfigured: the facet “Facet by” field carries the amber
  required-empty cue instead, and both previews show a quiet muted
  one-line hint (design-system convention).

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
