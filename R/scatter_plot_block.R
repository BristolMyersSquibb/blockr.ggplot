#' Scatter plot block constructor
#'
#' This block creates scatter plots using [ggplot2::geom_point()].
#' Supports comprehensive aesthetic mappings and customization options.
#'
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color aesthetic (optional)
#' @param shape Column for shape aesthetic (optional)
#' @param size Column for size aesthetic (optional)
#' @param alpha Transparency level (0-1, default 0.7)
#' @param add_smooth Whether to add a trendline (default FALSE)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_scatter_plot_block <- function(
  x = character(),
  y = character(),
  color = character(),
  shape = character(),
  size = character(),
  alpha = 0.7,
  add_smooth = FALSE,
  ...
) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          r_x <- reactiveVal(x)
          r_y <- reactiveVal(y)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_shape <- reactiveVal(if (length(shape) == 0) "(none)" else shape)
          r_size <- reactiveVal(if (length(size) == 0) "(none)" else size)
          r_alpha <- reactiveVal(alpha)
          r_add_smooth <- reactiveVal(add_smooth)

          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$shape, r_shape(input$shape))
          observeEvent(input$size, r_size(input$size))
          observeEvent(input$alpha, r_alpha(input$alpha))
          observeEvent(input$add_smooth, r_add_smooth(input$add_smooth))

          observeEvent(
            cols(),
            {
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
                inputId = "color",
                choices = c("(none)", cols()),
                selected = r_color()
              )
              updateSelectInput(
                session,
                inputId = "shape",
                choices = c("(none)", cols()),
                selected = r_shape()
              )
              updateSelectInput(
                session,
                inputId = "size",
                choices = c("(none)", cols()),
                selected = r_size()
              )
            }
          )

          # Create simple reactiveValues for conditions
          rv_cond <- reactiveValues(
            error = character(),
            warning = character(),
            message = character()
          )

          # Simple observer for trendline message
          observeEvent(
            r_add_smooth(),
            {
              if (r_add_smooth()) {
                rv_cond$message <- "Trendline added to plot"
              } else {
                rv_cond$message <- character()
              }
            },
            ignoreNULL = FALSE
          )

          # Capture ggplot2 messages during plot evaluation
          observeEvent(
            {
              # Create dependency on plot inputs
              list(
                r_x(),
                r_y(),
                r_color(),
                r_shape(),
                r_size(),
                r_alpha(),
                r_add_smooth()
              )
            },
            {
              # Only run if we have valid x and y
              if (
                isTruthy(r_x()) &&
                  isTruthy(r_y()) &&
                  length(r_x()) > 0 &&
                  length(r_y()) > 0
              ) {
                tryCatch(
                  {
                    # Build the same expression as the main reactive
                    aes_parts <- c(
                      glue::glue("x = {r_x()}"),
                      glue::glue("y = {r_y()}")
                    )

                    if (r_color() != "(none)") {
                      aes_parts <- c(
                        aes_parts,
                        glue::glue("colour = {r_color()}")
                      )
                    }
                    if (r_shape() != "(none)") {
                      aes_parts <- c(
                        aes_parts,
                        glue::glue("shape = {r_shape()}")
                      )
                    }
                    if (r_size() != "(none)") {
                      aes_parts <- c(aes_parts, glue::glue("size = {r_size()}"))
                    }

                    aes_text <- paste(aes_parts, collapse = ", ")
                    text <- glue::glue(
                      "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                      "ggplot2::geom_point(alpha = {r_alpha()})"
                    )

                    if (r_add_smooth()) {
                      text <- glue::glue("({text}) + ggplot2::geom_smooth()")
                    }

                    # Evaluate with message capture
                    captured_messages <- character()
                    withCallingHandlers(
                      eval(parse(text = text)[[1]], list(data = data())),
                      message = function(m) {
                        captured_messages <<- c(captured_messages, m$message)
                        invokeRestart("muffleMessage")
                      }
                    )

                    # Update conditions with captured messages
                    if (length(captured_messages) > 0) {
                      # Clean up messages (remove newlines, etc.)
                      clean_messages <- gsub("\n$", "", captured_messages)
                      rv_cond$message <- c(
                        rv_cond$message[
                          rv_cond$message != "Trendline added to plot"
                        ],
                        clean_messages
                      )
                    } else {
                      # Keep only non-ggplot messages if no ggplot messages
                      rv_cond$message <- rv_cond$message[
                        rv_cond$message == "Trendline added to plot"
                      ]
                    }
                  },
                  error = function(e) {
                    # Don't let ggplot errors break the observer
                  }
                )
              }
            },
            ignoreInit = TRUE
          )

          list(
            expr = reactive({
              # Validate required fields
              if (
                !isTruthy(r_x()) ||
                  length(r_x()) == 0 ||
                  !isTruthy(r_y()) ||
                  length(r_y()) == 0
              ) {
                return(quote(
                  ggplot2::ggplot() +
                    ggplot2::geom_blank()
                ))
              }

              # Build aesthetics dynamically
              aes_parts <- c(
                glue::glue("x = {r_x()}"),
                glue::glue("y = {r_y()}")
              )

              # Add optional aesthetics if not "(none)"
              if (r_color() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }

              if (r_shape() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("shape = {r_shape()}"))
              }

              if (r_size() != "(none)") {
                aes_parts <- c(aes_parts, glue::glue("size = {r_size()}"))
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build plot
              text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "ggplot2::geom_point(alpha = {r_alpha()})"
              )

              # Add smooth/trendline if requested
              if (r_add_smooth()) {
                text <- glue::glue("({text}) + ggplot2::geom_smooth()")
              }

              parse(text = text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              color = r_color,
              shape = r_shape,
              size = r_size,
              alpha = r_alpha,
              add_smooth = r_add_smooth
            ),
            cond = rv_cond
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

        # Shared grid for all controls (consistent columns across sections)
        div(
          class = "block-form-grid",

          # Axes Section
          div(
            class = "block-section",
            tags$h4("Axes"),
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
                class = "block-help-text",
                p("Both X and Y axes are required for scatter plots")
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
                  inputId = NS(id, "shape"),
                  label = "Shape By",
                  choices = c("(none)", shape),
                  selected = if (length(shape) == 0) "(none)" else shape,
                  width = "100%"
                )
              ),
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "size"),
                  label = "Size By",
                  choices = c("(none)", size),
                  selected = if (length(size) == 0) "(none)" else size,
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
                  label = "Point Transparency",
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
                  inputId = NS(id, "add_smooth"),
                  label = "Add Trendline",
                  value = add_smooth
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
    class = "scatter_plot_block",
    allow_empty_state = c("color", "shape", "size"), # All optional aesthetics
    ...
  )
}
