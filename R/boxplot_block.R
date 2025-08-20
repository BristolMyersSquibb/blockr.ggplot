#' Boxplot block constructor
#'
#' This block draws a boxplot using [ggplot2::geom_boxplot()]. Supports customizable
#' aesthetics including x-axis, y-axis grouping, color/fill, and styling options.
#'
#' @param x Column for x-axis (categorical variable)
#' @param y Column for y-axis (numeric variable, optional for single variable boxplots)
#' @param color Column for color aesthetic (optional)
#' @param fill Column for fill aesthetic (optional)
#' @param title Plot title (optional)
#' @param show_outliers Whether to show outliers (default TRUE)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_boxplot_block <- function(x = character(), y = character(), 
                             color = character(), fill = character(),
                             title = character(), show_outliers = TRUE, ...) {
	new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(color)
          fill_col <- reactiveVal(fill)
          plot_title <- reactiveVal(title)
          show_outliers_val <- reactiveVal(show_outliers)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colcol, color_col(input$colcol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$show_outliers, show_outliers_val(input$show_outliers))

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
                inputId = "colcol",
                choices = c("", cols()),
                selected = color_col()
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
              # Basic validation - need at least x
              if (is.null(x_col()) || x_col() == "") {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build basic aesthetics
              if (!is.null(y_col()) && y_col() != "") {
                # Two variable boxplot
                aes_call <- substitute(
                  ggplot2::aes(x = X, y = Y),
                  list(X = as.name(x_col()), Y = as.name(y_col()))
                )
              } else {
                # Single variable boxplot
                aes_call <- substitute(
                  ggplot2::aes(x = "", y = X),
                  list(X = as.name(x_col()))
                )
              }
              
              # Add fill aesthetic if specified
              if (!is.null(fill_col()) && fill_col() != "") {
                if (!is.null(y_col()) && y_col() != "") {
                  aes_call <- substitute(
                    ggplot2::aes(x = X, y = Y, fill = FILL),
                    list(X = as.name(x_col()), Y = as.name(y_col()), FILL = as.name(fill_col()))
                  )
                } else {
                  aes_call <- substitute(
                    ggplot2::aes(x = "", y = X, fill = FILL),
                    list(X = as.name(x_col()), FILL = as.name(fill_col()))
                  )
                }
              }
              
              # Build boxplot
              if (show_outliers_val()) {
                plot_expr <- substitute(
                  ggplot2::ggplot(data, AES) + ggplot2::geom_boxplot(),
                  list(AES = aes_call)
                )
              } else {
                plot_expr <- substitute(
                  ggplot2::ggplot(data, AES) + ggplot2::geom_boxplot(outlier.shape = NA),
                  list(AES = aes_call)
                )
              }
              
              # Add title if specified
              if (!is.null(plot_title()) && plot_title() != "") {
                plot_expr <- substitute(
                  PLOT + ggplot2::labs(title = TITLE),
                  list(PLOT = plot_expr, TITLE = plot_title())
                )
              }
              
              plot_expr
            }),
            state = list(
              x = x_col, 
              y = y_col,
              color = color_col, 
              fill = fill_col,
              title = plot_title,
              show_outliers = show_outliers_val
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Boxplot Configuration"),
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
            )
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "colcol"),
              label = "Color By",
              choices = color,
              selected = color
            ),
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Fill By",
              choices = fill,
              selected = fill
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
              inputId = NS(id, "show_outliers"),
              label = "Show Outliers",
              value = show_outliers
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    class = "boxplot_block",
    ...
  )
}
