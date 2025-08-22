#' Pie chart block constructor
#'
#' This block creates pie charts using [ggplot2::geom_col()] with polar coordinates.
#' Supports categorical data visualization with optional donut chart style.
#'
#' @param x Column for categories (required)
#' @param y Column for values (optional - if not provided, uses count)
#' @param fill Column for fill colors (optional, defaults to x)
#' @param donut Whether to create a donut chart (default FALSE)
#' @param show_labels Whether to show percentage labels (default TRUE)
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @export
new_pie_chart_block <- function(x = character(), y = character(),
                               fill = character(), donut = FALSE,
                               show_labels = TRUE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_y <- reactiveVal(if (length(y) == 0) "(none)" else y)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_donut <- reactiveVal(donut)
          r_show_labels <- reactiveVal(show_labels)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$donut, r_donut(input$donut))
          observeEvent(input$show_labels, r_show_labels(input$show_labels))

          observeEvent(
            cols(),
            {
              # Never filter columns by type - let ggplot2 handle type validation
              updateSelectInput(
                session,
                inputId = "x",
                choices = cols(),
                selected = r_x()
              )
              updateSelectInput(
                session,
                inputId = "y",
                choices = c("(none)", cols()),
                selected = r_y()
              )
              updateSelectInput(
                session,
                inputId = "fill",
                choices = c("(none)", cols()),
                selected = r_fill()
              )
            }
          )

          list(
            expr = reactive({
              # Validate required field
              if (!isTruthy(r_x()) || length(r_x()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Determine y variable and data preparation
              if (r_y() != "(none)") {
                y_var <- r_y()
                data_prep <- glue::glue("data <- data[!is.na(data${r_x()}), ]")
              } else {
                y_var <- "n"
                data_prep <- glue::glue('data <- data[!is.na(data${r_x()}), ]; data <- data %>% dplyr::count({r_x()}, name = "n")')
              }
              
              # Use fill if specified, otherwise use x
              fill_var <- if (r_fill() != "(none)") r_fill() else r_x()
              
              # Build aesthetics
              aes_text <- glue::glue('x = "", y = {y_var}, fill = {fill_var}')
              
              # Build basic plot
              plot_text <- glue::glue('ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_col(width = 1) + ggplot2::coord_polar("y", start = 0) + ggplot2::theme_void()')
              
              # Add donut hole if requested
              if (r_donut()) {
                plot_text <- glue::glue("({plot_text}) + ggplot2::xlim(c(-1, 1.5))")
              }
              
              # Add labels if requested
              if (r_show_labels()) {
                if (r_y() != "(none)") {
                  # Custom y values
                  label_text <- glue::glue('paste0(round({r_y()} / sum({r_y()}) * 100, 1), "%")')
                } else {
                  # Count values
                  label_text <- 'paste0(round(n / sum(n) * 100, 1), "%")'
                }
                plot_text <- glue::glue("({plot_text}) + ggplot2::geom_text(ggplot2::aes(label = {label_text}), position = ggplot2::position_stack(vjust = 0.5))")
              }
              
              # Combine data preparation and plot
              final_text <- glue::glue("{{ {data_prep}; {plot_text} }}")
              
              parse(text = final_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              fill = r_fill,
              donut = r_donut,
              show_labels = r_show_labels
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Pie Chart Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "x"),
              label = "Categories",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "y"),
              label = "Values",
              choices = c("(none)", y),
              selected = if (length(y) == 0) "(none)" else y
            ),
            helpText("Categories is required. If Values is '(none)', will count occurrences.")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fill"),
              label = "Color By",
              choices = c("(none)", fill),
              selected = if (length(fill) == 0) "(none)" else fill
            ),
            div(
              style = "margin-top: 25px;",
              checkboxInput(
                inputId = NS(id, "donut"),
                label = "Donut Chart Style",
                value = donut
              ),
              checkboxInput(
                inputId = NS(id, "show_labels"),
                label = "Show Percentage Labels",
                value = show_labels
              )
            )
          )
        )
      )
    },
    class = "pie_chart_block",
    allow_empty_state = c("y", "fill"),  # y is optional (empty = count), fill is optional
    ...
  )
}
