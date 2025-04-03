new_ggplot_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "ggplot_block"), ctor, ...)
}

#' @export
block_ui.ggplot_block <- function(id, x, ...) {
  tagList(
    plotOutput(NS(id, "result"))
  )
}

#' @export
block_output.ggplot_block <- function(x, result, session) {
  renderPlot(result)
}
