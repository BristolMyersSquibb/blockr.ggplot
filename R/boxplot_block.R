#' Boxplot block constructor
#'
#' This block draws a boxplot using [ggplot2::geom_boxplot()].
#'
#' @param x,color Columns to use for [ggplot2::aes()]
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_boxplot_block <- function(x = character(), color = character(), ...) {
	new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          color_col <- reactiveVal(color)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$colcol, color_col(input$colcol))

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
                inputId = "colcol",
                choices = c("", cols()),
                selected = color_col()
              )
            }
          )

          list(
            expr = reactive(
              bquote(
                ggplot2::ggplot(data, ggplot2::aes(..(aes))) +
                  geom_boxplot(),
                list(
                  aes = if (isTruthy(color_col())) {
                    list(x = x_col(), col = color_col())
                  } else {
                    list(x = x_col())
                  }
                ),
                splice = TRUE
              )
            ),
            state = list(x = x_col, color = color_col)
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
          inputId = NS(id, "colcol"),
          label = "Color",
          choices = color,
          selected = color
        )
      )
    },
    class = "boxplot_block",
    ...
  )
}
