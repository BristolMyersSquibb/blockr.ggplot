#' Scatter plot block constructor
#'
#' This block creates scatter plots using [ggplot2::geom_point()].
#' Supports comprehensive aesthetic mappings and customization options.
#'
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color aesthetic (optional)
#' @param shape Column for shape aesthetic (optional)
#' @param alpha Transparency level (0-1, default 0.7)
#' @param add_smooth Whether to add a trendline (default FALSE)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_scatter_plot_block <- function(x = character(), y = character(), color = character(),
                                   shape = character(), alpha = 0.7, add_smooth = FALSE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(if (length(color) == 0) "(none)" else color)
          shape_col <- reactiveVal(if (length(shape) == 0) "(none)" else shape)
          alpha_val <- reactiveVal(alpha)
          add_smooth_val <- reactiveVal(add_smooth)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$shapecol, shape_col(input$shapecol))
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
                choices = c("(none)", cols()),
                selected = color_col()
              )
              updateSelectInput(
                session,
                inputId = "shapecol",
                choices = c("(none)", cols()),
                selected = shape_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build aesthetics dynamically
              aes_parts <- c(
                glue::glue("x = {x_col()}"),
                glue::glue("y = {y_col()}")
              )
              
              # Add optional aesthetics if not "(none)"
              if (color_col() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {color_col()}"))
              }
              
              if (shape_col() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("shape = {shape_col()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build plot
              text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_point(alpha = {alpha_val()})")

              # Add smooth/trendline if requested
              if (add_smooth_val()) {
                text <- glue::glue("({text}) + ggplot2::geom_smooth()")
              }

              parse(text = text)[[1]]
            }),
            state = list(x = x_col, y = y_col, color = color_col, shape = shape_col, alpha = alpha_val, add_smooth = add_smooth_val)
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
          choices = if (length(color) == 0) "(none)" else color,
          selected = if (length(color) == 0) "(none)" else color
        ),
        selectInput(
          inputId = NS(id, "shapecol"),
          label = "Shape By",
          choices = if (length(shape) == 0) "(none)" else shape,
          selected = if (length(shape) == 0) "(none)" else shape
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