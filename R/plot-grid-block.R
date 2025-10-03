#' Plot Grid Block
#'
#' Combines multiple ggplot objects in a single row using cowplot::plot_grid().
#' Variadic block that accepts 2 or more ggplot inputs.
#'
#' @param ... Forwarded to [new_ggplot_transform_block()]
#' @export
new_plot_grid_block <- function(...) {
  new_ggplot_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {

          arg_names <- reactive(
            set_names(names(...args), blockr.core:::dot_args_names(...args))
          )

          list(
            expr = reactive(
              bquote(
                cowplot::plot_grid(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
                splice = TRUE
              )
            ),
            state = list()
          )
        }
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 2L)
    },
    allow_empty_state = TRUE,
    class = "plot_grid_block",
    ...
  )
}
