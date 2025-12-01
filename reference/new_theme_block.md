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
  palette_fill = "auto",
  palette_colour = "auto",
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

- palette_fill:

  Color palette for fill aesthetic: "auto" (keep upstream), "viridis",
  "magma", "plasma", "inferno", "cividis", or "ggplot2" (default "auto"
  preserves upstream palette)

- palette_colour:

  Color palette for colour aesthetic: "auto" (keep upstream), "viridis",
  "magma", "plasma", "inferno", "cividis", or "ggplot2" (default "auto"
  preserves upstream palette)

- ...:

  Forwarded to
  [`new_transform_block`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A ggplot transform block object of class `theme_block`.

## Examples

``` r
# Create a theme block with classic theme
new_theme_block(base_theme = "classic")
#> <theme_block<ggplot_transform_block<block>>>
#> Name: "Theme"
#> Data inputs: "data"
#> Initial block state:
#>  $ panel_bg         : chr ""
#>  $ plot_bg          : chr ""
#>  $ base_size        : num NA
#>  $ base_family      : chr "auto"
#>  $ show_major_grid  : chr "auto"
#>  $ show_minor_grid  : chr "auto"
#>  $ grid_color       : chr ""
#>  $ show_panel_border: chr "auto"
#>  $ legend_position  : chr "auto"
#>  $ base_theme       : chr "classic"
#>  $ palette_fill     : chr "auto"
#>  $ palette_colour   : chr "auto"
#> Constructor: blockr.ggplot::new_theme_block()

# Create a theme block with custom settings
new_theme_block(
  base_theme = "minimal",
  legend_position = "bottom",
  base_size = 14
)
#> <theme_block<ggplot_transform_block<block>>>
#> Name: "Theme"
#> Data inputs: "data"
#> Initial block state:
#>  $ panel_bg         : chr ""
#>  $ plot_bg          : chr ""
#>  $ base_size        : num 14
#>  $ base_family      : chr "auto"
#>  $ show_major_grid  : chr "auto"
#>  $ show_minor_grid  : chr "auto"
#>  $ grid_color       : chr ""
#>  $ show_panel_border: chr "auto"
#>  $ legend_position  : chr "bottom"
#>  $ base_theme       : chr "minimal"
#>  $ palette_fill     : chr "auto"
#>  $ palette_colour   : chr "auto"
#> Constructor: blockr.ggplot::new_theme_block()

if (interactive()) {
  library(blockr.core)
  # Theme block requires a ggplot input
  serve(new_theme_block())
}
```
