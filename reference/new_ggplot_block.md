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
  smoother = "none",
  smoother_se = TRUE,
  y_trans = "identity",
  y_zero = FALSE,
  title = character(),
  subtitle = character(),
  caption = character(),
  xlab = character(),
  ylab = character(),
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

- smoother:

  Trend line drawn over the data: "none" (default), "lm" (straight
  least-squares fit) or "loess". Point and line charts only.

- smoother_se:

  Draw the smoother's confidence band (default TRUE, ggplot2's own
  default). Ignored when `smoother` is "none".

- y_trans:

  Y-axis transform: "identity" (default), "log10" or "sqrt". The
  transform is applied BEFORE the stat, so a smoother or a boxplot
  summarises on the transformed scale.

- y_zero:

  Extend the y axis to include zero (default FALSE). Ignored under a
  log10 transform, which has no zero.

- title:

  Plot title ("" = none)

- subtitle:

  Plot subtitle ("" = none)

- caption:

  Plot caption ("" = none)

- xlab:

  X-axis label ("" = the column name, ggplot2's default)

- ylab:

  Y-axis label ("" = the column name, ggplot2's default)

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
#>  $ smoother     : chr "none"
#>  $ smoother_se  : logi TRUE
#>  $ y_trans      : chr "identity"
#>  $ y_zero       : logi FALSE
#>  $ title        : chr(0)
#>  $ subtitle     : chr(0)
#>  $ caption      : chr(0)
#>  $ xlab         : chr(0)
#>  $ ylab         : chr(0)
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
#>  $ smoother     : chr "none"
#>  $ smoother_se  : logi TRUE
#>  $ y_trans      : chr "identity"
#>  $ y_zero       : logi FALSE
#>  $ title        : chr(0)
#>  $ subtitle     : chr(0)
#>  $ caption      : chr(0)
#>  $ xlab         : chr(0)
#>  $ ylab         : chr(0)
#> Constructor: blockr.ggplot::new_ggplot_block()

if (interactive()) {
  library(blockr.core)
  serve(new_ggplot_block(), list(data = mtcars))
}
```
