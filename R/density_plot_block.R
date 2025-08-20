#' Density plot block constructor
#'
#' This block creates density plots using [ggplot2::geom_density()]. Perfect for
#' visualizing the distribution of continuous variables with smooth curves.
#'
#' @param x Column for x-axis (numeric variable)
#' @param fill Column for fill aesthetic (for multiple density curves, optional)
#' @param color Column for color aesthetic (optional)
#' @param alpha Transparency level (default 0.5)
#' @param title Plot title (optional)
#' @param adjust Bandwidth adjustment for density calculation (default 1)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_density_plot_block <- function(x = character(), fill = character(),
                                  color = character(), alpha = 0.5,
                                  title = character(), adjust = 1, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          fill_col <- reactiveVal(fill)
          color_col <- reactiveVal(color)
          alpha_val <- reactiveVal(alpha)
          plot_title <- reactiveVal(title)
          adjust_val <- reactiveVal(adjust)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$alpha, alpha_val(input$alpha))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$adjust, adjust_val(input$adjust))

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
                inputId = "colorcol",
                choices = c("", cols()),
                selected = color_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build basic plot text
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
                glue::glue("alpha = {alpha_val()}"),
                glue::glue("adjust = {adjust_val()}")
              )
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build basic plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_density({geom_args_text})")
              
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
              alpha = alpha_val,
              title = plot_title,
              adjust = adjust_val
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Density Plot Configuration"),
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
              label = "Fill By (Multiple Densities)",
              choices = fill,
              selected = fill
            ),
            selectInput(
              inputId = NS(id, "colorcol"),
              label = "Color By",
              choices = color,
              selected = color
            )
          ),
          div(
            class = "col-md-6",
            sliderInput(
              inputId = NS(id, "alpha"),
              label = "Transparency",
              value = alpha,
              min = 0.1,
              max = 1.0,
              step = 0.1
            ),
            sliderInput(
              inputId = NS(id, "adjust"),
              label = "Bandwidth Adjustment",
              value = adjust,
              min = 0.1,
              max = 3.0,
              step = 0.1
            ),
            helpText("Higher values = smoother curves")
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-12",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            )
          )
        )
      )
    },
    class = "density_plot_block",
    ...
  )
}