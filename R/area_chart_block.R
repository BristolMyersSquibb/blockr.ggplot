#' Area chart block constructor
#'
#' This block creates area charts using [ggplot2::geom_area()]. Perfect for 
#' showing cumulative values over time with stacking support.
#'
#' @param x Column for x-axis (required)
#' @param y Column for y-axis (required) 
#' @param fill Column for fill aesthetic (optional, for stacking multiple areas)
#' @param color Column for color aesthetic (outline color, optional)
#' @param position Area position: "stack", "fill" (default "stack")
#' @param alpha Transparency level (default 0.7)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_area_chart_block <- function(x = character(), y = character(),
                                fill = character(), color = character(), position = "stack",
                                alpha = 0.7, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_y <- reactiveVal(y)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_position <- reactiveVal(position)
          r_alpha <- reactiveVal(alpha)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$position, r_position(input$position))
          observeEvent(input$alpha, r_alpha(input$alpha))

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
                choices = cols(),
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
              # Validate required fields
              if (!isTruthy(r_x()) || length(r_x()) == 0 || !isTruthy(r_y()) || length(r_y()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {r_x()}"), glue::glue("y = {r_y()}"))
              if (r_fill() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("fill = {r_fill()}"))
              }
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build geom arguments
              geom_args <- c(glue::glue("alpha = {r_alpha()}"))
              if (r_fill() != "(none)") {
                geom_args <- c(geom_args, glue::glue('position = "{r_position()}"'))
              }
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_area({geom_args_text})")
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              fill = r_fill,
              color = r_color,
              position = r_position,
              alpha = r_alpha
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
              inputId = NS(id, "x"),
              label = "X-axis",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "y"),
              label = "Y-axis",
              choices = y,
              selected = y
            ),
            helpText("Both X and Y axes are required for area charts")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fill"),
              label = "Fill/Stack By",
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
              label = "Area Position",
              choices = list(
                "Stacked" = "stack",
                "Filled (100%)" = "fill"
              ),
              selected = position
            ),
            div(
              style = "margin-top: 25px;",
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
      )
    },
    class = "area_chart_block",
    allow_empty_state = c("fill", "color"),  # fill and color are optional
    ...
  )
}
