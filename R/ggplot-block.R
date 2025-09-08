#' @export
new_ggplot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_plot_block(server, ui, c(class, "ggplot_block"), ctor, ...)
}
