#' Heatmap block constructor
#'
#' This block creates heatmaps using [ggplot2::geom_tile()]. Perfect for 
#' visualizing 2D categorical data or correlation matrices.
#'
#' @param x Column for x-axis (categorical variable)
#' @param y Column for y-axis (categorical variable)
#' @param fill Column for fill/color intensity (numeric variable)
#' @param title Plot title (optional)
#' @param show_values Whether to show values as text on tiles (default FALSE)
#' @param color_palette Color palette: "viridis", "plasma", "inferno", "magma", "blues" (default "viridis")
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_heatmap_block <- function(x = character(), y = character(),
                             fill = character(), title = character(),
                             show_values = FALSE, color_palette = "viridis", ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          fill_col <- reactiveVal(fill)
          plot_title <- reactiveVal(title)
          show_values_val <- reactiveVal(show_values)
          color_palette_val <- reactiveVal(color_palette)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$fillcol, fill_col(input$fillcol))
          observeEvent(input$title, plot_title(input$title))
          observeEvent(input$show_values, show_values_val(input$show_values))
          observeEvent(input$color_palette, color_palette_val(input$color_palette))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              factor_cols <- cols()[sapply(data(), function(x) is.factor(x) || is.character(x))]
              all_cols <- cols()
              
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = c("", all_cols),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "ycol",
                choices = c("", all_cols),
                selected = y_col()
              )
              updateSelectInput(
                session,
                inputId = "fillcol",
                choices = c("", numeric_cols),
                selected = fill_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build basic plot text
              if (!isTruthy(x_col()) || !isTruthy(y_col()) || !isTruthy(fill_col())) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }
              
              # Build aesthetics
              aes_text <- glue::glue("x = {x_col()}, y = {y_col()}, fill = {fill_col()}")
              
              # Build basic plot with theme
              plot_text <- glue::glue(
                'ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ',
                'ggplot2::geom_tile(color = "white") + ',
                'ggplot2::theme_minimal() + ',
                'ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), panel.grid = ggplot2::element_blank())'
              )
              
              # Add color scale based on palette choice
              color_scale <- switch(color_palette_val(),
                "viridis" = "ggplot2::scale_fill_viridis_c()",
                "plasma" = 'ggplot2::scale_fill_viridis_c(option = "plasma")',
                "inferno" = 'ggplot2::scale_fill_viridis_c(option = "inferno")',
                "magma" = 'ggplot2::scale_fill_viridis_c(option = "magma")',
                "blues" = 'ggplot2::scale_fill_gradient(low = "white", high = "steelblue")',
                "ggplot2::scale_fill_viridis_c()"  # default
              )
              
              plot_text <- glue::glue("({plot_text}) + {color_scale}")
              
              # Add text values if requested
              if (show_values_val()) {
                plot_text <- glue::glue('({plot_text}) + ggplot2::geom_text(ggplot2::aes(label = round({fill_col()}, 2)), color = "black", size = 3)')
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
              title = plot_title,
              show_values = show_values_val,
              color_palette = color_palette_val
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Heatmap Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-4",
            selectInput(
              inputId = NS(id, "xcol"),
              label = "X-axis",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Y-axis", 
              choices = y,
              selected = y
            )
          ),
          div(
            class = "col-md-4",
            selectInput(
              inputId = NS(id, "fillcol"),
              label = "Fill Values (Numeric)",
              choices = fill,
              selected = fill
            ),
            selectInput(
              inputId = NS(id, "color_palette"),
              label = "Color Palette",
              choices = list(
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Inferno" = "inferno",
                "Magma" = "magma",
                "Blues" = "blues"
              ),
              selected = color_palette
            )
          ),
          div(
            class = "col-md-4",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            ),
            checkboxInput(
              inputId = NS(id, "show_values"),
              label = "Show Values on Tiles",
              value = show_values
            )
          )
        )
      )
    },
    class = "heatmap_block",
    ...
  )
}