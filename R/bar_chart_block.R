#' Bar chart block constructor
#'
#' This block creates bar charts using [ggplot2::geom_col()] or [ggplot2::geom_bar()].
#' Supports grouped and stacked bar charts with customizable aesthetics.
#'
#' @param x Column for x-axis
#' @param y Column for y-axis (optional - if not provided, uses count)
#' @param fill Column for fill aesthetic (for grouping/stacking)
#' @param position Bar position: "stack", "dodge", "fill" (default "stack")
#' @param title Plot title (optional)
#' @param flip_coords Whether to flip coordinates (horizontal bars, default FALSE)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_bar_chart_block <- function(x = character(), y = character(),
                               fill = character(), position = "stack",
                               flip_coords = FALSE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)  # Required field
          r_y <- reactiveVal(if (length(y) == 0) "(none)" else y)  # Optional: "(none)" means count
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_position <- reactiveVal(position)
          r_flip_coords <- reactiveVal(flip_coords)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$position, r_position(input$position))
          observeEvent(input$flip_coords, r_flip_coords(input$flip_coords))

          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "x",
                choices = cols(),  # Any column can be used as categorical
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
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {r_x()}"))
              if (r_y() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("y = {r_y()}"))
                geom_func <- "geom_col"
              } else {
                geom_func <- "geom_bar"
              }
              if (r_fill() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("fill = {r_fill()}"))
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build geom arguments
              geom_args <- ""
              if (r_fill() != "(none)") {
                geom_args <- glue::glue('position = "{r_position()}"')
              }

              # Build basic plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::{geom_func}({geom_args})")

              # Add coordinate flip if requested
              if (r_flip_coords()) {
                plot_text <- glue::glue("({plot_text}) + ggplot2::coord_flip()")
              }

              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              fill = r_fill,
              position = r_position,
              flip_coords = r_flip_coords
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Bar Chart Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "x"),
              label = "X-axis",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "y"),
              label = "Y-axis (optional)",
              choices = c("(none)", y),
              selected = if (length(y) == 0) "(none)" else y
            ),
            helpText("If Y-axis is empty, will count occurrences of X-axis values")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fill"),
              label = "Group/Stack By",
              choices = c("(none)", fill),
              selected = if (length(fill) == 0) "(none)" else fill
            ),
            selectInput(
              inputId = NS(id, "position"),
              label = "Bar Position",
              choices = list(
                "Stacked" = "stack",
                "Grouped (Side-by-side)" = "dodge",
                "Filled (100%)" = "fill"
              ),
              selected = position
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-4",
            checkboxInput(
              inputId = NS(id, "flip_coords"),
              label = "Horizontal Bars",
              value = flip_coords
            )
          )
        )
      )
    },
    class = "bar_chart_block",
    ...
  )
}