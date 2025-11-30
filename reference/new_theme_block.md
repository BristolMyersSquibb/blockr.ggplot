# Theme customization block for ggplot2 plots

A block that applies advanced theme customizations to ggplot2 objects.
Allows fine-grained control over backgrounds, fonts, grid lines, and
more. Empty/NULL values will use the base theme's defaults.

## Usage

``` r
new_theme_block(
  panel_bg = "",
  plot_bg = "",
  base_size = NA_real_,
  base_family = "auto",
  show_major_grid = "auto",
  show_minor_grid = "auto",
  grid_color = "",
  show_panel_border = "auto",
  legend_position = "auto",
  base_theme = "auto",
  ...
)
```

## Arguments

- panel_bg:

  Panel background color (default "" uses base theme default)

- plot_bg:

  Plot background color (default "" uses base theme default)

- base_size:

  Base font size in points (default NA uses base theme default)

- base_family:

  Font family: "auto", "sans", "serif", or "mono" (default "auto"
  preserves upstream font)

- show_major_grid:

  Show major grid lines: "auto", "show", "hide" (default "auto" uses
  base theme default)

- show_minor_grid:

  Show minor grid lines: "auto", "show", "hide" (default "auto" uses
  base theme default)

- grid_color:

  Grid line color (default "" uses base theme default)

- show_panel_border:

  Show panel border: "auto", "show", "hide" (default "auto" uses base
  theme default)

- legend_position:

  Legend position: "auto", "right", "left", "top", "bottom", "none"
  (default "auto" preserves upstream position)

- base_theme:

  Base ggplot2 theme: "auto", "minimal", "classic", "gray", "bw", etc.
  (default "auto" preserves upstream theme)

- ...:

  Forwarded to
  [`new_transform_block`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)
