# Facet Block

Applies faceting to a ggplot object using facet_wrap() or facet_grid().
Accepts a single ggplot input and adds faceting based on data columns.

## Usage

``` r
new_facet_block(
  facet_type = "wrap",
  facets = character(),
  rows = character(),
  cols = character(),
  ncol = character(),
  nrow = character(),
  scales = "fixed",
  labeller = "label_value",
  dir = "h",
  space = "fixed",
  ...
)
```

## Arguments

- facet_type:

  Type of faceting: "wrap" or "grid" (default: "wrap")

- facets:

  Column(s) to facet by for facet_wrap (character vector)

- rows:

  Column(s) for row facets in facet_grid (character vector)

- cols:

  Column(s) for column facets in facet_grid (character vector)

- ncol:

  Number of columns for facet_wrap (default: NULL for auto)

- nrow:

  Number of rows for facet_wrap (default: NULL for auto)

- scales:

  Scale behavior: "fixed", "free", "free_x", "free_y" (default: "fixed")

- labeller:

  Labeller function: "label_value", "label_both", "label_parsed"
  (default: "label_value")

- dir:

  Direction for facet_wrap: "h" (horizontal) or "v" (vertical) (default:
  "h")

- space:

  Space behavior for facet_grid: "fixed", "free_x", "free_y" (default:
  "fixed")

- ...:

  Forwarded to
  [`new_ggplot_transform_block()`](https://bristolmyerssquibb.github.io/blockr.ggplot/reference/new_ggplot_transform_block.md)

## Value

A ggplot transform block object of class `facet_block`.

## Examples

``` r
# Create a facet wrap block
new_facet_block(facet_type = "wrap", facets = "cyl")
#> <facet_block<ggplot_transform_block<block>>>
#> Name: "Facet"
#> Data inputs: "data"
#> Initial block state:
#>  $ facet_type: chr "wrap"
#>  $ facets    : chr "cyl"
#>  $ rows      : chr(0)
#>  $ cols      : chr(0)
#>  $ ncol      : chr(0)
#>  $ nrow      : chr(0)
#>  $ scales    : chr "fixed"
#>  $ labeller  : chr "label_value"
#>  $ dir       : chr "h"
#>  $ space     : chr "fixed"
#> Constructor: blockr.ggplot::new_facet_block()

# Create a facet grid block
new_facet_block(facet_type = "grid", rows = "cyl", cols = "gear")
#> <facet_block<ggplot_transform_block<block>>>
#> Name: "Facet"
#> Data inputs: "data"
#> Initial block state:
#>  $ facet_type: chr "grid"
#>  $ facets    : chr(0)
#>  $ rows      : chr "cyl"
#>  $ cols      : chr "gear"
#>  $ ncol      : chr(0)
#>  $ nrow      : chr(0)
#>  $ scales    : chr "fixed"
#>  $ labeller  : chr "label_value"
#>  $ dir       : chr "h"
#>  $ space     : chr "fixed"
#> Constructor: blockr.ggplot::new_facet_block()

if (interactive()) {
  library(blockr.core)
  # Facet block requires a ggplot input
  serve(new_facet_block())
}
```
