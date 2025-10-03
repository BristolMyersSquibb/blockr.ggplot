# DEPRECATED: This block is kept for demonstration purposes only
# and will be removed in a future version.
# Please use new_chart_block() instead for all chart types.

#' Heatmap block constructor
#'
#' This block creates heatmaps using [ggplot2::geom_tile()]. Perfect for
#' visualizing 2D categorical data or correlation matrices.
#'
#' @param x Column for x-axis (required)
#' @param y Column for y-axis (required)
#' @param fill Column for fill/color intensity (required)
#' @param show_values Whether to show values as text on tiles (default FALSE)
#' @param color_palette Color palette: "viridis", "plasma", "inferno",
#'   "magma", "blues" (default "viridis")
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @export
new_heatmap_block <- function(
  x = character(),
  y = character(),
  fill = character(),
  show_values = FALSE,
  color_palette = "viridis",
  ...
) {
  .Deprecated("new_chart_block")

  new_ggplot_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_y <- reactiveVal(y)
          r_fill <- reactiveVal(fill)
          r_show_values <- reactiveVal(show_values)
          r_color_palette <- reactiveVal(color_palette)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$show_values, r_show_values(input$show_values))
          observeEvent(
            input$color_palette,
            r_color_palette(input$color_palette)
          )

          observeEvent(
            cols(),
            {
              # Let ggplot2 handle type validation
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
                choices = cols(),
                selected = r_fill()
              )
            }
          )

          list(
            expr = reactive({
              # Validate required fields
              if (
                !isTruthy(r_x()) ||
                  length(r_x()) == 0 ||
                  !isTruthy(r_y()) ||
                  length(r_y()) == 0 ||
                  !isTruthy(r_fill()) ||
                  length(r_fill()) == 0
              ) {
                return(quote(
                  ggplot2::ggplot() +
                    ggplot2::geom_blank()
                ))
              }

              # Build aesthetics
              aes_text <- glue::glue(
                "x = {r_x()}, y = {r_y()}, fill = {r_fill()}"
              )

              # Build basic plot with theme
              plot_text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "ggplot2::geom_tile(color = \"white\") + ",
                "ggplot2::theme_minimal() + ",
                "ggplot2::theme(axis.text.x = ",
                "ggplot2::element_text(angle = 45, hjust = 1), ",
                "panel.grid = ggplot2::element_blank())"
              )

              # Add color scale based on palette choice
              color_scale <- switch(
                r_color_palette(),
                "viridis" = "ggplot2::scale_fill_viridis_c()",
                "plasma" = 'ggplot2::scale_fill_viridis_c(option = "plasma")',
                "inferno" = 'ggplot2::scale_fill_viridis_c(option = "inferno")',
                "magma" = 'ggplot2::scale_fill_viridis_c(option = "magma")',
                "blues" = paste0(
                  "ggplot2::scale_fill_gradient(",
                  "low = 'white', high = 'steelblue')"
                ),
                "ggplot2::scale_fill_viridis_c()" # default
              )

              plot_text <- glue::glue("({plot_text}) + {color_scale}")

              # Add text values if requested
              if (r_show_values()) {
                plot_text <- glue::glue(
                  "({plot_text}) + ggplot2::geom_text(",
                  "ggplot2::aes(label = round({r_fill()}, 2)), ",
                  "color = 'black', size = 3)"
                )
              }

              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              fill = r_fill,
              show_values = r_show_values,
              color_palette = r_color_palette
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "block-container",

        # Add responsive CSS
        block_responsive_css(),

        # Set container query context
        block_container_script(),

        # Shared grid for all controls
        div(
          class = "block-form-grid",

          # Data Section
          div(
            class = "block-section",
            tags$h4("Data"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "x"),
                  label = "X-axis",
                  choices = x,
                  selected = x,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "y"),
                  label = "Y-axis",
                  choices = y,
                  selected = y,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "fill"),
                  label = "Fill Values",
                  choices = fill,
                  selected = fill,
                  width = "100%"
                )
              ),
              div(
                class = "block-help-text",
                helpText(
                  "All three fields (X, Y, Fill) are required for heatmaps"
                )
              )
            )
          ),

          # Options Section
          div(
            class = "block-section",
            tags$h4("Options"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
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
                  selected = color_palette,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                checkboxInput(
                  inputId = NS(id, "show_values"),
                  label = "Show Values on Tiles",
                  value = show_values
                )
              )
            )
          )
        )
      )
    },
    class = "heatmap_block",
    ...
  )
}
