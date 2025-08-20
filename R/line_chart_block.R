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
                                size = 1, show_points = TRUE,
                                title = character(), ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(color)
          linetype_col <- reactiveVal(linetype)
          size_val <- reactiveVal(size)
          show_points_val <- reactiveVal(show_points)
          plot_title <- reactiveVal(title)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$linetypecol, linetype_col(input$linetypecol))
          observeEvent(input$size, size_val(input$size))
          observeEvent(input$show_points, show_points_val(input$show_points))
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
                inputId = "colorcol",
                choices = c("", cols()),
                selected = color_col()
              )
              updateSelectInput(
                session,
                inputId = "linetypecol",
                choices = c("", cols()),
                selected = linetype_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build basic plot text
              if (!isTruthy(x_col()) || !isTruthy(y_col())) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {x_col()}"), glue::glue("y = {y_col()}"))
              if (isTruthy(color_col())) {
                aes_parts <- c(aes_parts, glue::glue("colour = {color_col()}"))
              }
              if (isTruthy(linetype_col())) {
                aes_parts <- c(aes_parts, glue::glue("linetype = {linetype_col()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build basic plot with line
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_line(size = {size_val()})")
              
              # Add points if requested
              if (show_points_val()) {
                plot_text <- glue::glue("({plot_text}) + ggplot2::geom_point()")
              }
              
              # Add title if specified
              if (isTruthy(plot_title())) {
                plot_text <- glue::glue('({plot_text}) + ggplot2::labs(title = "{plot_title()}")')
              }
              
              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = x_col,
              y = y_col,
              color = color_col,
              linetype = linetype_col,
              size = size_val,
              show_points = show_points_val,
              title = plot_title
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
              inputId = NS(id, "colorcol"),
              label = "Color By (Multiple Lines)",
              choices = color,
              selected = color
            ),
            selectInput(
              inputId = NS(id, "linetypecol"),
              label = "Line Type By",
              choices = linetype,
              selected = linetype
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-6",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            )
          ),
          div(
            class = "col-md-3",
            numericInput(
              inputId = NS(id, "size"),
              label = "Line Size",
              value = size,
              min = 0.1,
              max = 3,
              step = 0.1
            )
          ),
          div(
            class = "col-md-3",
            checkboxInput(
              inputId = NS(id, "show_points"),
              label = "Show Points",
              value = show_points
            )
          )
        )
      )
    },
    class = "line_chart_block",
    ...
  )
}