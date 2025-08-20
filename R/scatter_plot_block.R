#' Scatter plot block constructor
#'
#' This block creates scatter plots using [ggplot2::geom_point()].
#' Supports basic aesthetic mappings.
#'
#' @param x Column for x-axis 
#' @param y Column for y-axis
#' @param color Column for color aesthetic (optional)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_scatter_plot_block <- function(x = character(), y = character(), color = character(), ...) {
  new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(color)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colorcol, color_col(input$colorcol))

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
              updateSelectInput(
                session,
                inputId = "colorcol",
                choices = c("", cols()),
                selected = color_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build aesthetics conditionally
              if (!is.null(color_col()) && color_col() != "") {
                # Include color aesthetic
                text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()}, colour = {color_col()})) + ggplot2::geom_point()")
              } else {
                # Basic x, y only
                text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()})) + ggplot2::geom_point()")
              }
              parse(text = text)[[1]]
            }),
            state = list(x = x_col, y = y_col, color = color_col)
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
        ),
        selectInput(
          inputId = NS(id, "colorcol"),
          label = "Color By",
          choices = color,
          selected = color
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    class = "scatter_plot_block",
    ...
  )
}