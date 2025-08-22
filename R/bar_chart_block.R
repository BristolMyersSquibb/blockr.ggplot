#' Bar chart block constructor
#'
#' This block creates bar charts using [ggplot2::geom_col()] or
#' [ggplot2::geom_bar()].
#' Supports grouped and stacked bar charts with customizable aesthetics.
#'
#' @param x Column for x-axis
#' @param y Column for y-axis (optional - if not provided, uses count)
#' @param fill Column for fill aesthetic (for grouping/stacking)
#' @param color Column for color aesthetic (outline color, optional)
#' @param position Bar position: "stack", "dodge", "fill" (default "stack")
#' @param alpha Transparency level (0-1, default 1.0)
#' @param flip_coords Whether to flip coordinates for horizontal bars
#'   (default FALSE)
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @export
new_bar_chart_block <- function(x = character(), y = character(),
                                fill = character(), color = character(),
                                position = "stack",
                                alpha = 1.0, flip_coords = FALSE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)  # Required field
          # Optional: "(none)" means count
          r_y <- reactiveVal(if (length(y) == 0) "(none)" else y)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_position <- reactiveVal(position)
          r_alpha <- reactiveVal(alpha)
          r_flip_coords <- reactiveVal(flip_coords)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$position, r_position(input$position))
          observeEvent(input$alpha, r_alpha(input$alpha))
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
              updateSelectInput(
                session,
                inputId = "color",
                choices = c("(none)", cols()),
                selected = r_color()
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
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build geom arguments
              geom_args <- c()
              if (r_fill() != "(none)") {
                geom_args <- c(
                  geom_args,
                  glue::glue('position = "{r_position()}"')
                )
              }
              geom_args <- c(
                geom_args,
                glue::glue("alpha = {r_alpha()}")
              )
              geom_args_text <- paste(geom_args, collapse = ", ")

              # Build basic plot
              plot_text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "ggplot2::{geom_func}({geom_args_text})"
              )

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
              color = r_color,
              position = r_position,
              alpha = r_alpha,
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
            helpText(
              "If Y-axis is empty, will count occurrences of X values"
            )
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
              inputId = NS(id, "color"),
              label = "Outline Color By",
              choices = c("(none)", color),
              selected = if (length(color) == 0) "(none)" else color
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
            class = "col-md-6",
            sliderInput(
              inputId = NS(id, "alpha"),
              label = "Transparency",
              min = 0.1,
              max = 1.0,
              value = alpha,
              step = 0.1
            )
          ),
          div(
            class = "col-md-6",
            div(
              style = "margin-top: 25px;",
              checkboxInput(
                inputId = NS(id, "flip_coords"),
                label = "Horizontal Bars",
                value = flip_coords
              )
            )
          )
        )
      )
    },
    class = "bar_chart_block",
    # y is optional (empty = count), fill and color are optional
    allow_empty_state = c("y", "fill", "color"),
    ...
  )
}
