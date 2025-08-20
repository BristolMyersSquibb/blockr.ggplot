#' Bar chart block constructor
#'
#' This block creates bar charts using [ggplot2::geom_col()] or [ggplot2::geom_bar()].
#' Supports grouped and stacked bar charts with customizable aesthetics.
#'
#' @param x Column for x-axis (categorical variable)
#' @param y Column for y-axis (numeric variable, optional - if not provided, uses count)
#' @param fill Column for fill aesthetic (for grouping/stacking)
#' @param position Bar position: "stack", "dodge", "fill" (default "stack")
#' @param title Plot title (optional)
#' @param flip_coords Whether to flip coordinates (horizontal bars, default FALSE)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_bar_chart_block <- function(x = character(), y = character(),
                               fill = character(), position = "stack",
                               title = character(), flip_coords = FALSE, ...) {
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
          plot_title <- reactiveVal(title)
          flip_coords_val <- reactiveVal(flip_coords)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$position, position_val(input$position))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$flip_coords, flip_coords_val(input$flip_coords))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              factor_cols <- cols()[sapply(data(), function(x) is.factor(x) || is.character(x))]
              
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = c("", factor_cols),
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
              # Build basic plot text
              if (!isTruthy(x_col())) return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              
              # Build aesthetics
              aes_parts <- c(glue::glue("x = {x_col()}"))
              if (isTruthy(y_col())) {
                aes_parts <- c(aes_parts, glue::glue("y = {y_col()}"))
                geom_func <- "geom_col"
              } else {
                geom_func <- "geom_bar"
              }
              if (isTruthy(fill_col())) {
                aes_parts <- c(aes_parts, glue::glue("fill = {fill_col()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build geom arguments
              geom_args <- ""
              if (isTruthy(fill_col())) {
                geom_args <- glue::glue('position = "{position_val()}"')
              }
              
              # Build basic plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::{geom_func}({geom_args})")
              
              # Add coordinate flip if requested
              if (flip_coords_val()) {
                plot_text <- glue::glue("({plot_text}) + ggplot2::coord_flip()")
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
              fill = fill_col,
              position = position_val,
              title = plot_title,
              flip_coords = flip_coords_val
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
              inputId = NS(id, "xcol"),
              label = "X-axis (Categorical)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Y-axis (Numeric, optional)",
              choices = y,
              selected = y
            ),
            helpText("If Y-axis is empty, will count occurrences of X-axis values")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Group/Stack By",
              choices = fill,
              selected = fill
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