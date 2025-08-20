#' Toy Scatter plot block constructor
#'
#' Minimal ggplot2 scatter plot that exactly mimics blockr.core's scatter plot pattern.
#' For testing ggplot2 integration with blockr plot infrastructure.
#'
#' @param x,y Columns to place on respective axes
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_toy_scatter_plot_block <- function(x = character(), y = character(), ...) {
  new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))

          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = cols(),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "ycol",
                choices = cols(),
                selected = y_col()
              )
            }
          )

          list(
            expr = reactive({
              text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()})) + ggplot2::geom_point()")
              parse(text = text)[[1]]
            }),
            state = list(x = x_col, y = y_col)
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "xcol"),
          label = "X-axis",
          choices = x,
          selected = x
        ),
        selectInput(
          inputId = NS(id, "ycol"),
          label = "Y-axis",
          choices = y,
          selected = y
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    class = "toy_scatter_plot_block",
    ...
  )
}