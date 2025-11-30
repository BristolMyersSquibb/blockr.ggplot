# Grid Block

Combines multiple ggplot objects using patchwork::wrap_plots(). Variadic
block that accepts 1 or more ggplot inputs with automatic alignment.
Supports layout control (ncol, nrow) and annotations (title, subtitle,
auto-tags).

## Usage

``` r
new_grid_block(
  ncol = character(),
  nrow = character(),
  title = character(),
  subtitle = character(),
  caption = character(),
  tag_levels = character(),
  guides = "auto",
  ...
)
```

## Arguments

- ncol:

  Number of columns in grid layout (default: NULL for auto)

- nrow:

  Number of rows in grid layout (default: NULL for auto)

- title:

  Overall plot title (default: "")

- subtitle:

  Overall plot subtitle (default: "")

- caption:

  Overall plot caption (default: "")

- tag_levels:

  Auto-tagging style: 'A', 'a', '1', 'I', 'i', or NULL (default: NULL)

- guides:

  Legend handling: 'auto', 'collect', or 'keep' (default: 'auto')

- ...:

  Forwarded to
  [`new_ggplot_transform_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_ggplot_transform_block.md)
