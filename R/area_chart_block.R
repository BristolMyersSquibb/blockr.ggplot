#' Area chart block constructor
#'
#' This block creates area charts using [ggplot2::geom_area()]. Perfect for 
#' showing cumulative values over time with stacking support.
#'
#' @param x Column for x-axis (often time/date variable)
#' @param y Column for y-axis (numeric variable)
#' @param fill Column for fill aesthetic (for stacking multiple areas)
#' @param position Area position: "stack", "fill" (default "stack")
#' @param alpha Transparency level (default 0.7)
#' @param title Plot title (optional)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_area_chart_block <- function(x = character(), y = character(),
                                fill = character(), position = "stack",
                                alpha = 0.7, title = character(), ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          fill_col <- reactiveVal(fill)
          position_val <- reactiveVal(position)
          alpha_val <- reactiveVal(alpha)
          plot_title <- reactiveVal(title)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$position, position_val(input$position))
          observeEvent(input$alpha, alpha_val(input$alpha))
          observeEvent(input$title, plot_title(input$title))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              date_numeric_cols <- cols()[sapply(data(), function(x) {
                is.numeric(x) || inherits(x, c("Date", "POSIXt"))
              })]
              
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = c("", date_numeric_cols),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "ycol",
                choices = c("", numeric_cols),
                selected = y_col()
              )
              updateSelectInput(
                session,
                inputId = "fillcol",
                choices = c("", cols()),
                selected = fill_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build aesthetics
              aes_list <- list()
              if (isTruthy(x_col())) aes_list$x <- x_col()
              if (isTruthy(y_col())) aes_list$y <- y_col()
              if (isTruthy(fill_col())) aes_list$fill <- fill_col()
              
              # Build geom arguments
              area_args <- list()
              area_args$alpha <- alpha_val()
              if (isTruthy(fill_col())) {
                area_args$position <- position_val()
              }
              
              # Build the plot expression
              plot_expr <- bquote(
                ggplot2::ggplot(data, ggplot2::aes(..(aes_mapping))) +
                  ggplot2::geom_area(..(area_arguments)),
                list(
                  aes_mapping = lapply(aes_list, as.name),
                  area_arguments = area_args
                ),
                splice = TRUE
              )
              
              # Add title if specified
              if (isTruthy(plot_title())) {
                plot_expr <- bquote(
                  ..(plot_base) + ggplot2::labs(title = .(title_text)),
                  list(plot_base = plot_expr, title_text = plot_title()),
                  splice = TRUE
                )
              }
              
              plot_expr
            }),
            state = list(
              x = x_col,
              y = y_col,
              fill = fill_col,
              position = position_val,
              alpha = alpha_val,
              title = plot_title
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Area Chart Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "xcol"),
              label = "X-axis (Numeric/Date)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Y-axis (Numeric)",
              choices = y,
              selected = y
            )
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Stack/Fill By",
              choices = fill,
              selected = fill
            ),
            selectInput(
              inputId = NS(id, "position"),
              label = "Area Position",
              choices = list(
                "Stacked" = "stack",
                "Filled (100%)" = "fill"
              ),
              selected = position
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
    class = "area_chart_block",
    ...
  )
}