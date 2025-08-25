#' Pie chart block constructor
#'
#' This block creates pie charts using [ggplot2::geom_col()] with
#' polar coordinates.
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
new_pie_chart_block <- function(
  x = character(),
  y = character(),
  fill = character(),
  donut = FALSE,
  show_labels = TRUE,
  ...
) {
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
              # Let ggplot2 handle type validation
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
              if (!isTruthy(r_x())) {
                return(quote(
                  ggplot2::ggplot() +
                    ggplot2::geom_blank()
                ))
              }

              # Build aesthetics - use bar chart pattern
              aes_parts <- c(glue::glue("x = {r_x()}"))

              # Choose geom based on y variable (like bar chart)
              if (r_y() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("y = {r_y()}"))
                geom_func <- "geom_col"
              } else {
                geom_func <- "geom_bar"
              }

              # Use fill if specified, otherwise use x
              fill_var <- if (r_fill() != "(none)") r_fill() else r_x()
              aes_parts <- c(aes_parts, glue::glue("fill = {fill_var}"))

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build basic plot with pie transformation
              plot_text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "ggplot2::{geom_func}(width = 1) + ",
                "ggplot2::coord_polar('y', start = 0) + ",
                "ggplot2::theme_void()"
              )

              # Add donut hole if requested
              if (r_donut()) {
                plot_text <- glue::glue(
                  "({plot_text}) + ggplot2::xlim(c(-1, 1.5))"
                )
              }

              # Add labels if requested
              if (r_show_labels()) {
                if (r_y() != "(none)") {
                  # Custom y values
                  label_text <- glue::glue(
                    "paste0(round({r_y()} / sum({r_y()}) * 100, 1), '%')"
                  )
                } else {
                  # Use ..count.. for automatic counting
                  label_text <- paste0(
                    "paste0(round(..count.. / sum(..count..) * 100, 1), '%')"
                  )
                }
                plot_text <- glue::glue(
                  "({plot_text}) + ggplot2::geom_text(",
                  "ggplot2::aes(label = {label_text}), ",
                  "stat = 'count', ",
                  "position = ggplot2::position_stack(vjust = 0.5))"
                )
              }

              parse(text = plot_text)[[1]]
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
        class = "block-container",

        # Add responsive CSS
        block_responsive_css(),

        # Set container query context
        block_container_script(),

        # Shared grid for all controls
        div(
          class = "block-form-grid",

          # Data Section
          div(
            class = "block-section",
            tags$h4("Data"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "x"),
                  label = "Categories",
                  choices = x,
                  selected = x,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "y"),
                  label = "Values",
                  choices = c("(none)", y),
                  selected = if (length(y) == 0) "(none)" else y,
                  width = "100%"
                )
              ),
              div(
                class = "block-help-text",
                helpText(
                  "Categories is required. If Values is '(none)', ",
                  "will count occurrences."
                )
              )
            )
          ),

          # Aesthetics Section
          div(
            class = "block-section",
            tags$h4("Aesthetics"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "fill"),
                  label = "Color By",
                  choices = c("(none)", fill),
                  selected = if (length(fill) == 0) "(none)" else fill,
                  width = "100%"
                )
              )
            )
          ),

          # Options Section
          div(
            class = "block-section",
            tags$h4("Options"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                checkboxInput(
                  inputId = NS(id, "donut"),
                  label = "Donut Chart Style",
                  value = donut
                )
              ),
              div(
                class = "block-input-wrapper",
                checkboxInput(
                  inputId = NS(id, "show_labels"),
                  label = "Show Percentage Labels",
                  value = show_labels
                )
              )
            )
          )
        )
      )
    },
    class = "pie_chart_block",
    # y is optional (empty = count), fill is optional
    allow_empty_state = c("y", "fill"),
    ...
  )
}
