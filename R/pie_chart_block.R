#' Pie chart block constructor
#'
#' This block creates pie charts using [ggplot2::geom_col()] with polar coordinates.
#' Supports categorical data visualization with optional donut chart style.
#'
#' @param x Column for categories (categorical variable)
#' @param y Column for values (numeric variable, optional - if not provided, uses count)
#' @param fill Column for fill colors (optional, defaults to x)
#' @param donut Whether to create a donut chart (default FALSE)
#' @param title Plot title (optional)
#' @param show_labels Whether to show percentage labels (default TRUE)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_pie_chart_block <- function(x = character(), y = character(),
                               fill = character(), donut = FALSE,
                               title = character(), show_labels = TRUE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          fill_col <- reactiveVal(fill)
          donut_val <- reactiveVal(donut)
          plot_title <- reactiveVal(title)
          show_labels_val <- reactiveVal(show_labels)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$donut, donut_val(input$donut))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$show_labels, show_labels_val(input$show_labels))

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
              # Prepare data for pie chart
              if (isTruthy(y_col())) {
                # Use provided y values
                data_prep_expr <- bquote(
                  data <- data[!is.na(.(as.name(x_col()))), ]
                )
              } else {
                # Count occurrences
                data_prep_expr <- bquote({
                  data <- data[!is.na(.(as.name(x_col()))), ]
                  data <- data %>% 
                    dplyr::count(.(as.name(x_col())), name = "n")
                })
              }
              
              # Build aesthetics
              aes_list <- list()
              if (isTruthy(x_col())) aes_list$x <- ""  # Empty x for pie chart
              if (isTruthy(y_col())) {
                aes_list$y <- y_col()
              } else {
                aes_list$y <- "n"  # Use count
              }
              
              # Use fill_col if specified, otherwise use x_col
              fill_var <- if (isTruthy(fill_col())) fill_col() else x_col()
              if (isTruthy(fill_var)) aes_list$fill <- fill_var
              
              # Build the plot expression
              plot_expr <- bquote(
                ggplot2::ggplot(data, ggplot2::aes(..(aes_mapping))) +
                  ggplot2::geom_col(width = 1) +
                  ggplot2::coord_polar("y", start = 0) +
                  ggplot2::theme_void(),
                list(
                  aes_mapping = lapply(aes_list, function(x) if(x == "") "" else as.name(x))
                ),
                splice = TRUE
              )
              
              # Add donut hole if requested
              if (donut_val()) {
                plot_expr <- bquote(
                  ..(plot_base) + ggplot2::xlim(c(-1, 1.5)),
                  list(plot_base = plot_expr),
                  splice = TRUE
                )
              }
              
              # Add labels if requested
              if (show_labels_val()) {
                if (isTruthy(y_col())) {
                  # Custom y values
                  plot_expr <- bquote(
                    ..(plot_base) + 
                      ggplot2::geom_text(
                        ggplot2::aes(label = paste0(round(.(as.name(y_col())) / sum(.(as.name(y_col()))) * 100, 1), "%")),
                        position = ggplot2::position_stack(vjust = 0.5)
                      ),
                    list(plot_base = plot_expr),
                    splice = TRUE
                  )
                } else {
                  # Count values
                  plot_expr <- bquote(
                    ..(plot_base) + 
                      ggplot2::geom_text(
                        ggplot2::aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
                        position = ggplot2::position_stack(vjust = 0.5)
                      ),
                    list(plot_base = plot_expr),
                    splice = TRUE
                  )
                }
              }
              
              # Add title if specified
              if (isTruthy(plot_title())) {
                plot_expr <- bquote(
                  ..(plot_base) + ggplot2::labs(title = .(title_text)),
                  list(plot_base = plot_expr, title_text = plot_title()),
                  splice = TRUE
                )
              }
              
              # Combine data prep and plot
              if (isTruthy(y_col())) {
                final_expr <- bquote({
                  ..(data_prep)
                  ..(plot_code)
                }, list(data_prep = data_prep_expr, plot_code = plot_expr))
              } else {
                final_expr <- bquote({
                  ..(data_prep)
                  ..(plot_code)
                }, list(data_prep = data_prep_expr, plot_code = plot_expr))
              }
              
              final_expr
            }),
            state = list(
              x = x_col,
              y = y_col,
              fill = fill_col,
              donut = donut_val,
              title = plot_title,
              show_labels = show_labels_val
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Pie Chart Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "xcol"),
              label = "Categories (Categorical)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Values (Numeric, optional)",
              choices = y,
              selected = y
            ),
            helpText("If Values is empty, will count occurrences of Categories")
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Color By (optional)",
              choices = fill,
              selected = fill
            ),
            div(
              class = "mt-3",
              checkboxInput(
                inputId = NS(id, "donut"),
                label = "Donut Chart Style",
                value = donut
              ),
              checkboxInput(
                inputId = NS(id, "show_labels"),
                label = "Show Percentage Labels",
                value = show_labels
              )
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
    class = "pie_chart_block",
    ...
  )
}