#' Histogram block constructor
#'
#' This block draws a histogram using [ggplot2::geom_histogram()]. Supports customizable
#' aesthetics including x-axis variable, binning options, color/fill, and styling.
#'
#' @param x Column for x-axis (numeric variable)
#' @param fill Column for fill aesthetic (optional)
#' @param color Column for color (outline) aesthetic (optional)
#' @param bins Number of bins (default 30)
#' @param title Plot title (optional)
#' @param alpha Transparency level (default 0.7)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_histogram_block <- function(x = character(), fill = character(),
                                color = character(), bins = 30,
                                title = character(), alpha = 0.7, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          fill_col <- reactiveVal(fill)
          color_col <- reactiveVal(color)
          bins_val <- reactiveVal(bins)
          plot_title <- reactiveVal(title)
          alpha_val <- reactiveVal(alpha)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$colcol, color_col(input$colcol))
          observeEvent(input$bins, bins_val(input$bins))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$alpha, alpha_val(input$alpha))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = c("", numeric_cols),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "fillcol",
                choices = c("", cols()),
                selected = fill_col()
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
            expr = reactive({
              # Build basic plot text - only x is required for histogram
              if (!isTruthy(x_col())) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {x_col()}"))
              if (isTruthy(fill_col())) {
                aes_parts <- c(aes_parts, glue::glue("fill = {fill_col()}"))
              }
              if (isTruthy(color_col())) {
                aes_parts <- c(aes_parts, glue::glue("colour = {color_col()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build geom arguments
              geom_args <- c(
                glue::glue("bins = {bins_val()}"),
                glue::glue("alpha = {alpha_val()}")
              )
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build basic plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_histogram({geom_args_text})")
              
              # Add title if specified
              if (isTruthy(plot_title())) {
                plot_text <- glue::glue('({plot_text}) + ggplot2::labs(title = "{plot_title()}")')
              }
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = x_col, 
              fill = fill_col,
              color = color_col,
              bins = bins_val,
              title = plot_title,
              alpha = alpha_val
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Histogram Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "xcol"),
              label = "X-axis (Numeric)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Fill By",
              choices = fill,
              selected = fill
            )
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "colcol"),
              label = "Color By",
              choices = color,
              selected = color
            ),
            numericInput(
              inputId = NS(id, "bins"),
              label = "Number of Bins",
              value = bins,
              min = 5,
              max = 100,
              step = 1
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-8",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            )
          ),
          div(
            class = "col-md-4",
            sliderInput(
              inputId = NS(id, "alpha"),
              label = "Transparency",
              value = alpha,
              min = 0.1,
              max = 1.0,
              step = 0.1
            )
          )
        )
      )
    },
    class = "histogram_block",
    ...
  )
}
