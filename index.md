# blockr.ggplot

blockr.ggplot provides interactive blocks for data visualization. Create
scatter plots, bar charts, line charts, and more through visual
interfaces with real-time preview.

## Overview

blockr.ggplot is part of the blockr ecosystem and provides visualization
blocks using ggplot2.

## Installation

``` r

install.packages("blockr.ggplot")
```

Or install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.ggplot")
```

## Getting Started

Create and launch an empty dashboard:

``` r

library(blockr.ggplot)
serve(new_board())
```

This opens a visual interface in your web browser. Add blocks using the
“+” button, connect them by dragging, and configure each block through
its settings. Visualizations update in real-time as you build your
workflow.

## Available Blocks

blockr.ggplot provides visualization blocks using the ggplot block with
9 chart types, plus 3 composition blocks:

### Chart Types

- [scatter](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  relationships between continuous variables
- [bar](https://blockr.site/docs/blocks/blockr.ggplot#ggplot): compare
  values across categories
- [line](https://blockr.site/docs/blocks/blockr.ggplot#ggplot): trends
  over time or sequences
- [boxplot](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  distribution statistics across groups
- [violin](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  distribution shapes with density
- [density](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  smooth probability distributions
- [area](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  cumulative magnitude over time
- [histogram](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  frequency distributions
- [pie/donut](https://blockr.site/docs/blocks/blockr.ggplot#ggplot):
  proportions of a whole

### Composition

- [facet](https://blockr.site/docs/blocks/blockr.ggplot#facet): split
  plots into panels by category
- [grid](https://blockr.site/docs/blocks/blockr.ggplot#grid): combine
  multiple plots into dashboards
- [theme](https://blockr.site/docs/blocks/blockr.ggplot#theme): apply
  professional styling

The [block reference on
blockr.site](https://blockr.site/docs/blocks/blockr.ggplot) lists every
block and its arguments.

## Learn More

The [blockr.ggplot
website](https://bristolmyerssquibb.github.io/blockr.ggplot/) includes
the function reference. For information on the workflow engine, see
[blockr.core](https://bristolmyerssquibb.github.io/blockr.core/).
