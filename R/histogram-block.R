#' Histogram block constructor
#'
#' This block draws a histogram using [ggplot2::geom_histogram()]. Supports
#' customizable aesthetics including x-axis variable, binning options,
#' color/fill, and styling.
#'
#' @param x Column for x-axis (numeric variable)
#' @param fill Column for fill aesthetic (optional)
#' @param color Column for color (outline) aesthetic (optional)
#' @param bins Number of bins (default 30)
#' @param alpha Transparency level (default 0.7)
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @export
new_histogram_block <- function(
  x = character(),
  fill = character(),
  color = character(),
  bins = 30,
  alpha = 0.7,
  ...
) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_bins <- reactiveVal(bins)
          r_alpha <- reactiveVal(alpha)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$bins, r_bins(input$bins))
          observeEvent(input$alpha, r_alpha(input$alpha))

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
                inputId = "fill",
                choices = c("(none)", cols()),
                selected = if (length(fill) == 0) "(none)" else fill
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
              # Validate required field
              if (!isTruthy(r_x())) {
                return(quote(
                  ggplot2::ggplot() +
                    ggplot2::geom_blank()
                ))
              }

              # Build aesthetics
              aes_parts <- c(glue::glue("x = {r_x()}"))
              if (r_fill() != "(none)") {
                # Convert to factor for discrete colors
                aes_parts <- c(
                  aes_parts,
                  glue::glue("fill = as.factor({r_fill()})")
                )
              }
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build geom arguments
              geom_args <- c(
                glue::glue("bins = {r_bins()}"),
                glue::glue("alpha = {r_alpha()}")
              )

              # Add position = "identity" when fill aesthetic is present
              has_fill <- (r_fill() != "(none)") ||
                (length(fill) > 0 && fill != "")
              if (has_fill) {
                geom_args <- c(geom_args, 'position = "identity"')
              }

              geom_args_text <- paste(geom_args, collapse = ", ")

              # Build basic plot
              plot_text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "ggplot2::geom_histogram({geom_args_text})"
              )

              parse(text = plot_text)[[1]]
            }),
            state = list(
              x = r_x,
              fill = r_fill,
              color = r_color,
              bins = r_bins,
              alpha = r_alpha
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
                  label = "X-axis (Numeric)",
                  choices = x,
                  selected = x,
                  width = "100%"
                )
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
                  inputId = NS(id, "fill"),
                  label = "Fill By",
                  choices = c("(none)", fill),
                  selected = if (length(fill) == 0) "(none)" else fill,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "color"),
                  label = "Color By",
                  choices = c("(none)", color),
                  selected = if (length(color) == 0) "(none)" else color,
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
                numericInput(
                  inputId = NS(id, "bins"),
                  label = "Number of Bins",
                  value = bins,
                  min = 5,
                  max = 100,
                  step = 1,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                sliderInput(
                  inputId = NS(id, "alpha"),
                  label = "Transparency",
                  value = alpha,
                  min = 0.1,
                  max = 1.0,
                  step = 0.1,
                  width = "100%"
                )
              )
            )
          )
        )
      )
    },
    class = "histogram_block",
    allow_empty_state = c("fill", "color"), # Optional aesthetics
    ...
  )
}
