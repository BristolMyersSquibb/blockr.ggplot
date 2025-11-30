# ggplot transform block constructor

Creates a specialized block for ggplot2-based visualizations. This block
returns ggplot objects as data, allowing ggplot blocks to be chained
together (e.g., for combining plots with patchwork). Custom output
methods ensure plots are displayed properly rather than as data tables.

## Usage

``` r
new_ggplot_transform_block(server, ui, class, ctor = sys.parent(), ...)
```

## Arguments

- server:

  Server function for the block

- ui:

  UI function for the block

- class:

  Character vector of CSS classes for the block

- ctor:

  Constructor environment (default
  [`sys.parent()`](https://rdrr.io/r/base/sys.parent.html))

- ...:

  Additional arguments forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html)

## Value

A `ggplot_transform_block` object
