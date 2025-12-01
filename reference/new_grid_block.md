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

## Value

A ggplot transform block object of class `grid_block`.

## Examples

``` r
# Create a grid block with 2 columns
new_grid_block(ncol = "2")
#> <grid_block<rbind_block<ggplot_transform_block<block>>>>
#> Name: "Grid"
#> Indefinite arity
#> Initial block state:
#>  $ ncol      : chr "2"
#>  $ nrow      : chr(0)
#>  $ title     : chr(0)
#>  $ subtitle  : chr(0)
#>  $ caption   : chr(0)
#>  $ tag_levels: chr(0)
#>  $ guides    : chr "auto"
#> Constructor: blockr.ggplot::new_grid_block()

# Create a grid block with title
new_grid_block(title = "My Combined Plots", ncol = "2")
#> <grid_block<rbind_block<ggplot_transform_block<block>>>>
#> Name: "Grid"
#> Indefinite arity
#> Initial block state:
#>  $ ncol      : chr "2"
#>  $ nrow      : chr(0)
#>  $ title     : chr "My Combined Plots"
#>  $ subtitle  : chr(0)
#>  $ caption   : chr(0)
#>  $ tag_levels: chr(0)
#>  $ guides    : chr "auto"
#> Constructor: blockr.ggplot::new_grid_block()

if (interactive()) {
  library(blockr.core)
  # Grid block requires multiple ggplot inputs
  serve(new_grid_block())
}
```
