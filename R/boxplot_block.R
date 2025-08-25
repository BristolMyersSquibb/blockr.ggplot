#' Boxplot block constructor
#'
#' This block draws a boxplot using [ggplot2::geom_boxplot()].
#' Supports customizable aesthetics including x-axis, y-axis grouping,
#' color/fill, and styling options.
#'
#' @param x Column for x-axis (categorical variable)
#' @param y Column for y-axis (numeric variable, optional for single
#'   variable boxplots)
#' @param color Column for color aesthetic (optional)
#' @param fill Column for fill aesthetic (optional)
#' @param alpha Transparency level (0-1, default 1.0)
#' @param show_outliers Whether to show outliers (default TRUE)
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @export
new_boxplot_block <- function(x = character(), y = character(),
                              color = character(), fill = character(),
                              alpha = 1.0, show_outliers = TRUE, ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(if (length(x) == 0) "(none)" else x)
          r_y <- reactiveVal(y)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_alpha <- reactiveVal(alpha)
          r_show_outliers <- reactiveVal(show_outliers)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$alpha, r_alpha(input$alpha))
          observeEvent(
            input$show_outliers,
            r_show_outliers(input$show_outliers)
          )

          observeEvent(
            cols(),
            {
              # Let ggplot2 handle type validation
              updateSelectInput(
                session,
                inputId = "x",
                choices = c("(none)", cols()),
                selected = if (
                  r_x() %in% c("(none)", cols())
                ) r_x() else "(none)"
              )
              updateSelectInput(
                session,
                inputId = "y",
                choices = cols(),
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
                inputId = "fill",
                choices = c("(none)", cols()),
                selected = r_fill()
              )
            }
          )

          list(
            expr = reactive({
              # Validate required field
              if (!isTruthy(r_y()) || length(r_y()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }

              # Build aesthetics
              if (r_x() != "(none)") {
                # Two variable boxplot (x = categorical, y = numeric)
                aes_parts <- c(
                  glue::glue("x = {r_x()}"),
                  glue::glue("y = {r_y()}")
                )
              } else {
                # Single variable boxplot (no grouping)
                aes_parts <- c('x = ""', glue::glue("y = {r_y()}"))
              }

              # Add optional aesthetics
              if (r_color() != "(none)") {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("colour = {r_color()}")
                )
              }
              if (r_fill() != "(none)") {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("fill = {r_fill()}")
                )
              }

              aes_text <- paste(aes_parts, collapse = ", ")
              # Build boxplot with outlier setting and alpha
              if (r_show_outliers()) {
                plot_text <- glue::glue(
                  "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                  "ggplot2::geom_boxplot(alpha = {r_alpha()})"
                )
              } else {
                plot_text <- glue::glue(
                  "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                  "ggplot2::geom_boxplot(alpha = {r_alpha()}, ",
                  "outlier.shape = NA)"
                )
              }

              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              color = r_color,
              fill = r_fill,
              alpha = r_alpha,
              show_outliers = r_show_outliers
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
                  label = "Group By (Categorical)",
                  choices = c("(none)", x),
                  selected = if (length(x) == 0) "(none)" else x,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "y"),
                  label = "Values (Numeric)",
                  choices = y,
                  selected = y,
                  width = "100%"
                )
              ),
              div(
                class = "block-help-text",
                helpText("Group By is optional - leave as '(none)' for single boxplot")
              )
            )
          ),

          # Aesthetics Section
          div(
            class = "block-section",
            tags$h4("Aesthetics"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "color"),
                  label = "Color By",
                  choices = c("(none)", color),
                  selected = if (length(color) == 0) "(none)" else color,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "fill"),
                  label = "Fill By",
                  choices = c("(none)", fill),
                  selected = if (length(fill) == 0) "(none)" else fill,
                  width = "100%"
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
                sliderInput(
                  inputId = NS(id, "alpha"),
                  label = "Transparency",
                  min = 0.1,
                  max = 1.0,
                  value = alpha,
                  step = 0.1,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                checkboxInput(
                  inputId = NS(id, "show_outliers"),
                  label = "Show Outliers",
                  value = show_outliers
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(is.data.frame(data) || is.matrix(data))
    },
    class = "boxplot_block",
    # x is optional (none = single boxplot), color and fill are optional
    allow_empty_state = c("x", "color", "fill"),
    ...
  )
}
