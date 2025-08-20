#' Violin plot block constructor
#'
#' This block creates violin plots using [ggplot2::geom_violin()]. Shows the 
#' probability density of data at different values, similar to boxplots but with 
#' more detailed shape information.
#'
#' @param x Column for x-axis (categorical variable)
#' @param y Column for y-axis (numeric variable)
#' @param fill Column for fill aesthetic (optional)
#' @param color Column for color aesthetic (optional)
#' @param title Plot title (optional)
#' @param trim Whether to trim the tails (default TRUE)
#' @param scale Scaling method: "area", "count", "width" (default "area")
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_violin_plot_block <- function(x = character(), y = character(),
                                 fill = character(), color = character(),
                                 title = character(), trim = TRUE,
                                 scale = "area", ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          fill_col <- reactiveVal(fill)
          color_col <- reactiveVal(color)
          plot_title <- reactiveVal(title)
          trim_val <- reactiveVal(trim)
          scale_val <- reactiveVal(scale)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$trim, trim_val(input$trim))
          observeEvent(input$scale, scale_val(input$scale))

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
              updateSelectInput(
                session,
                inputId = "colorcol",
                choices = c("", cols()),
                selected = color_col()
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
              if (isTruthy(fill_col())) {
                aes_parts <- c(aes_parts, glue::glue("fill = {fill_col()}"))
              }
              if (isTruthy(color_col())) {
                aes_parts <- c(aes_parts, glue::glue("colour = {color_col()}"))
              }
              
              aes_text <- paste(aes_parts, collapse = ", ")
              
              # Build geom arguments
              geom_args <- c(
                glue::glue("trim = {trim_val()}"),
                glue::glue('scale = "{scale_val()}"')
              )
              
              geom_args_text <- paste(geom_args, collapse = ", ")
              
              # Build basic plot
              plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_violin({geom_args_text})")
              
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
              color = color_col,
              title = plot_title,
              trim = trim_val,
              scale = scale_val
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
              inputId = NS(id, "xcol"),
              label = "X-axis (Categorical)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Y-axis (Numeric)",
              choices = y,
              selected = y
            ),
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Fill By",
              choices = fill,
              selected = fill
            )
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "colorcol"),
              label = "Color By",
              choices = color,
              selected = color
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
            checkboxInput(
              inputId = NS(id, "trim"),
              label = "Trim Tails",
              value = trim
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-12",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            )
          )
        )
      )
    },
    class = "violin_plot_block",
    ...
  )
}