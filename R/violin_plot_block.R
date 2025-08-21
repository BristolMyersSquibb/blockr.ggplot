#' Violin plot block constructor
#'
#' This block creates violin plots using [ggplot2::geom_violin()]. Shows the 
#' probability density of data at different values, similar to boxplots but with 
#' more detailed shape information.
#'
#' @param x Column for x-axis (required)
#' @param y Column for y-axis (required)
#' @param fill Column for fill aesthetic (optional)
#' @param color Column for color aesthetic (optional)
#' @param alpha Transparency level (0-1, default 1.0)
#' @param trim Whether to trim the tails (default TRUE)
#' @param scale Scaling method: "area", "count", "width" (default "area")
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_violin_plot_block <- function(x = character(), y = character(),
                                 fill = character(), color = character(),
                                 alpha = 1.0, trim = TRUE, scale = "area", ...) {
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
          r_alpha <- reactiveVal(alpha)
          r_trim <- reactiveVal(trim)
          r_scale <- reactiveVal(scale)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$alpha, r_alpha(input$alpha))
          observeEvent(input$trim, r_trim(input$trim))
          observeEvent(input$scale, r_scale(input$scale))

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
              geom_args <- c(
                glue::glue("alpha = {r_alpha()}"),
                glue::glue("trim = {r_trim()}"),
                glue::glue('scale = "{r_scale()}"')
              )
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_violin({geom_args_text})")
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              fill = r_fill,
              color = r_color,
              alpha = r_alpha,
              trim = r_trim,
              scale = r_scale
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Violin Plot Configuration"),
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
            helpText("Both X and Y axes are required for violin plots")
          ),
          div(
            class = "col-md-6",
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
            selectInput(
              inputId = NS(id, "scale"),
              label = "Scaling Method",
              choices = list(
                "Equal Area" = "area",
                "Count-based" = "count", 
                "Equal Width" = "width"
              ),
              selected = scale
            ),
            sliderInput(
              inputId = NS(id, "alpha"),
              label = "Transparency",
              min = 0.1,
              max = 1.0,
              value = alpha,
              step = 0.1
            ),
            div(
              style = "margin-top: 10px;",
              checkboxInput(
                inputId = NS(id, "trim"),
                label = "Trim Tails",
                value = trim
              )
            )
          )
        )
      )
    },
    class = "violin_plot_block",
    allow_empty_state = c("fill", "color"),  # Both fill and color are optional
    ...
  )
}