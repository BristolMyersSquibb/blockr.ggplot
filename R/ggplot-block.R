#' ggplot block constructor
#'
#' Creates a specialized plot block for ggplot2-based visualizations.
#' This is the base constructor for all ggplot2 blocks in blockr.ggplot.
#'
#' @param server Server function for the block
#' @param ui UI function for the block
#' @param class Character vector of CSS classes for the block
#' @param ctor Constructor environment (default `sys.parent()`)
#' @param ... Additional arguments forwarded to [blockr.core::new_plot_block()]
#'
#' @return A `ggplot_block` object
#' @export
new_ggplot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_plot_block(server, ui, c(class, "ggplot_block"), ctor, ...)
}
