# Universal ggplot block with selectable visualization types

A flexible block that allows users to select from various ggplot2 geoms
and dynamically shows relevant aesthetics for the selected
visualization.

## Usage

``` r
new_ggplot_block(
  type = "point",
  x = character(),
  y = character(),
  color = character(),
  fill = character(),
  size = character(),
  shape = character(),
  linetype = character(),
  group = character(),
  alpha = character(),
  density_alpha = 0.8,
  position = "stack",
  bins = 30,
  donut = FALSE,
  ...
)
```

## Arguments

- type:

  Initial chart type (default "point"). Options: "point", "bar", "line",
  "boxplot", "violin", "density", "area", "histogram", "pie"

- x:

  Column for x-axis

- y:

  Column for y-axis

- color:

  Column for color aesthetic

- fill:

  Column for fill aesthetic

- size:

  Column for size aesthetic

- shape:

  Column for shape aesthetic

- linetype:

  Column for linetype aesthetic

- group:

  Column for group aesthetic

- alpha:

  Column for alpha aesthetic (variable transparency)

- density_alpha:

  Fixed alpha value for density plots (default 0.8)

- position:

  Position adjustment for certain geoms

- bins:

  Number of bins for histogram

- donut:

  Whether to create donut chart when type is "pie" (default FALSE)

- ...:

  Forwarded to
  [`new_plot_block`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_plot_block.html)

## Value

A plot block object of class `ggplot_block`.

## Examples

``` r
# Create a scatter plot block
new_ggplot_block(type = "point", x = "mpg", y = "hp")
#> <ggplot_block<ggplot_transform_block<block>>>
#> Name: "Ggplot"
#> Data inputs: "data"
#> Initial block state:
#>  $ type         : chr "point"
#>  $ x            : chr "mpg"
#>  $ y            : chr "hp"
#>  $ color        : chr(0)
#>  $ fill         : chr(0)
#>  $ size         : chr(0)
#>  $ shape        : chr(0)
#>  $ linetype     : chr(0)
#>  $ group        : chr(0)
#>  $ alpha        : chr(0)
#>  $ density_alpha: num 0.8
#>  $ position     : chr "stack"
#>  $ bins         : num 30
#>  $ donut        : logi FALSE
#> Constructor: blockr.ggplot::new_ggplot_block()

# Create a bar chart block
new_ggplot_block(type = "bar", x = "cyl")
#> <ggplot_block<ggplot_transform_block<block>>>
#> Name: "Ggplot"
#> Data inputs: "data"
#> Initial block state:
#>  $ type         : chr "bar"
#>  $ x            : chr "cyl"
#>  $ y            : chr(0)
#>  $ color        : chr(0)
#>  $ fill         : chr(0)
#>  $ size         : chr(0)
#>  $ shape        : chr(0)
#>  $ linetype     : chr(0)
#>  $ group        : chr(0)
#>  $ alpha        : chr(0)
#>  $ density_alpha: num 0.8
#>  $ position     : chr "stack"
#>  $ bins         : num 30
#>  $ donut        : logi FALSE
#> Constructor: blockr.ggplot::new_ggplot_block()

if (interactive()) {
  library(blockr.core)
  serve(new_ggplot_block(), list(data = mtcars))
}
```
