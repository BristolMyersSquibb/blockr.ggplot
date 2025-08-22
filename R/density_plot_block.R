#' Density plot block constructor
#'
#' This block creates density plots using [ggplot2::geom_density()]. Perfect for
#' visualizing the distribution of continuous variables with smooth curves.
#'
#' @param x Column for x-axis (required)
#' @param fill Column for fill aesthetic (optional, for multiple density curves)
#' @param color Column for color aesthetic (optional)
#' @param alpha Transparency level (default 0.5)
#' @param adjust Bandwidth adjustment for density calculation (default 1)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_density_plot_block <- function(x = character(), fill = character(),
                                  color = character(), alpha = 0.5,
                                  adjust = 1, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_alpha <- reactiveVal(alpha)
          r_adjust <- reactiveVal(adjust)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$alpha, r_alpha(input$alpha))
          observeEvent(input$adjust, r_adjust(input$adjust))

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
              # Validate required field
              if (!isTruthy(r_x()) || length(r_x()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {r_x()}"))
              if (r_fill() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("fill = {r_fill()}"))
              }
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build geom arguments
              geom_args <- c(
                glue::glue("alpha = {r_alpha()}"),
                glue::glue("adjust = {r_adjust()}")
              )
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_density({geom_args_text})")
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              fill = r_fill,
              color = r_color,
              alpha = r_alpha,
              adjust = r_adjust
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
              inputId = NS(id, "x"),
              label = "X-axis",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "fill"),
              label = "Fill By",
              choices = c("(none)", fill),
              selected = if (length(fill) == 0) "(none)" else fill
            ),
            selectInput(
              inputId = NS(id, "color"),
              label = "Color By",
              choices = c("(none)", color),
              selected = if (length(color) == 0) "(none)" else color
            ),
            helpText("X-axis is required for density plots")
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
        )
      )
    },
    class = "density_plot_block",
    allow_empty_state = c("fill", "color"),  # Both fill and color are optional
    ...
  )
}
