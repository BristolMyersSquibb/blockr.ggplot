#' Scatter plot block constructor
#'
#' This block creates scatter plots using [ggplot2::geom_point()].
#' Supports comprehensive aesthetic mappings and customization options.
#'
#' @param x Column for x-axis 
#' @param y Column for y-axis
#' @param color Column for color aesthetic (optional)
#' @param alpha Transparency level (0-1, default 0.7)
#' @param add_smooth Whether to add a trendline (default FALSE)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_scatter_plot_block <- function(x = character(), y = character(), color = character(), 
                                   alpha = 0.7, add_smooth = FALSE, ...) {
  new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(color)
          alpha_val <- reactiveVal(alpha)
          add_smooth_val <- reactiveVal(add_smooth)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$alpha, alpha_val(input$alpha))
          observeEvent(input$add_smooth, add_smooth_val(input$add_smooth))

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
                text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()}, colour = {color_col()})) + ggplot2::geom_point(alpha = {alpha_val()})")
              } else {
                # Basic x, y only
                text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {x_col()}, y = {y_col()})) + ggplot2::geom_point(alpha = {alpha_val()})")
              }
              
              # Add smooth/trendline if requested
              if (add_smooth_val()) {
                text <- glue::glue("({text}) + ggplot2::geom_smooth()")
              }
              
              parse(text = text)[[1]]
            }),
            state = list(x = x_col, y = y_col, color = color_col, alpha = alpha_val, add_smooth = add_smooth_val)
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
        ),
        sliderInput(
          inputId = NS(id, "alpha"),
          label = "Transparency",
          min = 0.1,
          max = 1.0,
          value = alpha,
          step = 0.1
        ),
        checkboxInput(
          inputId = NS(id, "add_smooth"),
          label = "Add Trendline",
          value = add_smooth
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