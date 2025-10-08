#' ggplot transform block constructor
#'
#' Creates a specialized block for ggplot2-based visualizations.
#' This block returns ggplot objects as data, allowing ggplot blocks to be
#' chained together (e.g., for combining plots with patchwork). Custom output
#' methods ensure plots are displayed properly rather than as data tables.
#'
#' @param server Server function for the block
#' @param ui UI function for the block
#' @param class Character vector of CSS classes for the block
#' @param ctor Constructor environment (default `sys.parent()`)
#' @param ... Additional arguments forwarded to [blockr.core::new_block()]
#'
#' @return A `ggplot_transform_block` object
#' @export
new_ggplot_transform_block <- function(
  server,
  ui,
  class,
  ctor = sys.parent(),
  ...
) {
  blockr.core::new_block(
    server,
    ui,
    c(class, "ggplot_transform_block"),
    ctor,
    ...
  )
}

#' @export
block_ui.ggplot_transform_block <- function(id, x, ...) {
  tagList(plotOutput(NS(id, "result")))
}

#' @export
block_output.ggplot_transform_block <- function(x, result, session) {
  renderPlot(print(result), bg = "transparent")
}
