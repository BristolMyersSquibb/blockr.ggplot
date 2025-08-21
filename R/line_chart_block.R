#' Line chart block constructor
#'
#' This block creates line charts using [ggplot2::geom_line()] and optionally [ggplot2::geom_point()].
#' Perfect for time series data and trend visualization with grouping support.
#'
#' @param x Column for x-axis (often time/date variable)
#' @param y Column for y-axis (numeric variable)
#' @param color Column for color aesthetic (for multiple lines)
#' @param linetype Column for linetype aesthetic (optional)
#' @param size Line size (default 1)
#' @param show_points Whether to show points on the line (default TRUE)
#' @param title Plot title (optional)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_line_chart_block <- function(x = character(), y = character(),
                                color = character(), linetype = character(),
                                size = 1, show_points = TRUE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_y <- reactiveVal(y)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_linetype <- reactiveVal(if (length(linetype) == 0) "(none)" else linetype)
          r_size <- reactiveVal(size)
          r_show_points <- reactiveVal(show_points)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$linetype, r_linetype(input$linetype))
          observeEvent(input$size, r_size(input$size))
          observeEvent(input$show_points, r_show_points(input$show_points))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              date_numeric_cols <- cols()[sapply(data(), function(x) {
                is.numeric(x) || inherits(x, c("Date", "POSIXt"))
              })]
              
              updateSelectInput(
                session,
                inputId = "x",
                choices = date_numeric_cols,
                selected = r_x()
              )
              updateSelectInput(
                session,
                inputId = "y",
                choices = numeric_cols,
                selected = r_y()
              )
              updateSelectInput(
                session,
                inputId = "color",
                choices = c("(none)", cols()),
                selected = r_color()
              )
              updateSelectInput(
                session,
                inputId = "linetype",
                choices = c("(none)", cols()),
                selected = r_linetype()
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
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }
              if (r_linetype() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("linetype = {r_linetype()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build basic plot with line
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_line(size = {r_size()})")
              
              # Add points if requested
              if (r_show_points()) {
                plot_text <- glue::glue("({plot_text}) + ggplot2::geom_point()")
              }
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              color = r_color,
              linetype = r_linetype,
              size = r_size,
              show_points = r_show_points
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Line Chart Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "x"),
              label = "X-axis (Time/Numeric)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "y"),
              label = "Y-axis (Numeric)",
              choices = y,
              selected = y
            ),
            helpText("Line charts work best with time series or sequential data")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "color"),
              label = "Color By (Multiple Lines)",
              choices = c("(none)", color),
              selected = if (length(color) == 0) "(none)" else color
            ),
            selectInput(
              inputId = NS(id, "linetype"),
              label = "Line Type By",
              choices = c("(none)", linetype),
              selected = if (length(linetype) == 0) "(none)" else linetype
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-6",
            numericInput(
              inputId = NS(id, "size"),
              label = "Line Thickness",
              value = size,
              min = 0.1,
              max = 3,
              step = 0.1
            )
          ),
          div(
            class = "col-md-6",
            div(
              style = "margin-top: 25px;",
              checkboxInput(
                inputId = NS(id, "show_points"),
                label = "Show Points on Lines",
                value = show_points
              )
            )
          )
        )
      )
    },
    class = "line_chart_block",
    ...
  )
}