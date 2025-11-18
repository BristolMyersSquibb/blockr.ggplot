#' Universal ggplot block with selectable visualization types
#'
#' A flexible block that allows users to select from various ggplot2 geoms
#' and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "point"). Options: "point", "bar",
#'   "line", "boxplot", "violin", "density", "area", "histogram", "pie"
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color aesthetic
#' @param fill Column for fill aesthetic
#' @param size Column for size aesthetic
#' @param shape Column for shape aesthetic
#' @param linetype Column for linetype aesthetic
#' @param group Column for group aesthetic
#' @param alpha Column for alpha aesthetic (variable transparency)
#' @param density_alpha Fixed alpha value for density plots (default 0.8)
#' @param position Position adjustment for certain geoms
#' @param bins Number of bins for histogram
#' @param donut Whether to create donut chart when type is "pie" (default FALSE)
#' @param ... Forwarded to \code{\link[blockr.core]{new_plot_block}}
#'
#' @export
new_ggplot_block <- function(
  type = "point",
  x = character(),
  y = character(),
  color = character(),
  fill = character(),
  size = character(),
  shape = character(),
  linetype = character(),
  group = character(),
  alpha = character(),
  density_alpha = 0.8,
  position = "stack",
  bins = 30,
  donut = FALSE,
  ...
) {
  ui <- function(id) {
    tagList(
      shinyjs::useShinyjs(), # Need shinyjs for dynamic UI
      block_collapisble_section_css(id),
      div(
        class = "block-container",
        block_responsive_css(),
        block_chart_type_css(),
        block_container_script(),
        div(
          class = "block-form-grid",
          div(
            class = "block-section",
            tags$h4("Chart Type"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper chart-type-selector",
                style = "grid-column: 1 / -1;", # Span full width
                shinyWidgets::radioGroupButtons(
                  inputId = NS(id, "type"),
                  label = NULL,
                  choiceNames = list(
                    tags$div(icon("braille"), tags$span("Scatter")),
                    tags$div(icon("chart-bar"), tags$span("Bar")),
                    tags$div(icon("chart-line"), tags$span("Line")),
                    tags$div(icon("th-large"), tags$span("Box")),
                    tags$div(icon("chart-pie"), tags$span("Pie")),
                    tags$div(icon("chart-column"), tags$span("Histogram")),
                    tags$div(icon("signal"), tags$span("Density")),
                    tags$div(icon("water"), tags$span("Violin")),
                    tags$div(icon("chart-area"), tags$span("Area"))
                  ),
                  choiceValues = c(
                    "point",
                    "bar",
                    "line",
                    "boxplot",
                    "pie",
                    "histogram",
                    "density",
                    "violin",
                    "area"
                  ),
                  selected = type,
                  status = "light",
                  size = "sm",
                  justified = FALSE,
                  individual = FALSE,
                  checkIcon = list(
                    yes = tags$i(
                      class = "fa fa-check",
                      style = "display: none;"
                    ),
                    no = tags$i(style = "display: none;")
                  )
                )
              ),
              div(
                class = "block-help-text",
                style = "margin-top: -8px;",
                p("Click an icon to change the visualization type")
              )
            )
          ),

          # Unified Aesthetic Mapping Section
          div(
            class = "block-section",
            tags$h4(
              style = paste(
                "display: flex; align-items: center;",
                "justify-content: space-between;"
              ),
              "Mappings",
              tags$small(
                tags$span("*", style = "color: #dc3545; font-weight: bold;"),
                " Required field",
                style = paste(
                  "font-size: 0.7em; color: #6c757d;",
                  "font-weight: normal;"
                )
              )
            ),
            div(
              class = "block-section-grid",
              # Primary axes - X and Y
              div(
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "x"),
                  label = make_aesthetic_label("X-axis", "x", type),
                  choices = NULL,
                  width = "100%"
                )
              ),
              div(
                id = NS(id, "y"),
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "y"),
                  label = make_aesthetic_label("Y-axis", "y", type),
                  choices = NULL,
                  width = "100%"
                )
              ),
              # Core aesthetic mappings
              div(
                id = NS(id, "color"),
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "color"),
                  label = make_aesthetic_label("Color By", "color", type),
                  choices = NULL,
                  width = "100%"
                )
              ),
              div(
                id = NS(id, "fill"),
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "fill"),
                  label = make_aesthetic_label("Fill By", "fill", type),
                  choices = NULL,
                  width = "100%"
                )
              ),
              div(
                id = NS(id, "size"),
                class = "block-input-wrapper",
                selectInput(
                  inputId = NS(id, "size"),
                  label = make_aesthetic_label("Size By", "size", type),
                  choices = NULL,
                  width = "100%"
                )
              )
            )
          ),

          # Advanced Options Toggle
          block_collapsible_section_div(id),

          # Advanced Options Section (Collapsible)
          div(
            id = NS(id, "advanced-options"),

            # Advanced Aesthetic Mappings
            div(
              class = "block-section",
              tags$h4("Advanced Mappings"),
              div(
                class = "block-section-grid",
                div(
                  id = NS(id, "shape"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "shape"),
                    label = make_aesthetic_label("Shape By", "shape", type),
                    choices = NULL,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "linetype"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "linetype"),
                    label = make_aesthetic_label(
                      "Line Type By",
                      "linetype",
                      type
                    ),
                    choices = NULL,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "group"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "group"),
                    label = make_aesthetic_label("Group By", "group", type),
                    choices = NULL,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "alpha"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "alpha"),
                    label = make_aesthetic_label("Alpha By", "alpha", type),
                    choices = NULL,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "density_alpha"),
                  class = "block-input-wrapper",
                  sliderInput(
                    inputId = NS(id, "density_alpha"),
                    label = "Transparency (Alpha)",
                    min = 0,
                    max = 1,
                    value = density_alpha,
                    step = 0.05,
                    width = "100%"
                  )
                )
              )
            ),

            # Advanced Options
            div(
              class = "block-section",
              tags$h4("Chart-Specific Options"),
              div(
                class = "block-section-grid",
                div(
                  id = NS(id, "position"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "position"),
                    label = "Position",
                    choices = c("stack", "dodge", "fill"),
                    selected = position,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "bins"),
                  class = "block-input-wrapper",
                  numericInput(
                    inputId = NS(id, "bins"),
                    label = "Number of Bins",
                    value = bins,
                    min = 1,
                    max = 100,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "donut"),
                  class = "block-input-wrapper",
                  checkboxInput(
                    inputId = NS(id, "donut"),
                    label = "Donut Chart Style",
                    value = donut
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  server <- function(id, data) {
    moduleServer(
      id,
      function(input, output, session) {
        cols <- reactive(colnames(data()))

        observeEvent(cols(), {
          updateSelectInput(
            session,
            inputId = "x",
            choices = cols()
          )
          updateSelectInput(
            session,
            inputId = "y",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "color",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "fill",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "size",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "shape",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "linetype",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "group",
            choices = c("(none)", cols())
          )
          updateSelectInput(
            session,
            inputId = "alpha",
            choices = c("(none)", cols())
          )
        })

        # Dynamic UI visibility based on chart type
        observe({
          current_type <- input$type
          chart_config <- chart_aesthetics()[[current_type]]

          if (!is.null(chart_config)) {
            all_aesthetics <- c(
              "y",
              "color",
              "fill",
              "size",
              "shape",
              "linetype",
              "group",
              "alpha"
            )
            valid_aesthetics <- c(
              chart_config$required,
              chart_config$optional
            )
            # x is always shown
            valid_aesthetics <- valid_aesthetics[valid_aesthetics != "x"]

            # Hide/show aesthetic inputs based on validity
            for (aes in all_aesthetics) {
              # Special handling for alpha: density uses fixed alpha,
              # others use variable alpha
              if (aes == "alpha") {
                if (current_type == "density") {
                  shinyjs::hide("alpha") # Hide variable alpha selector
                  shinyjs::show("density_alpha") # Show fixed alpha slider
                } else if (aes %in% valid_aesthetics) {
                  shinyjs::show("alpha") # Show variable alpha selector
                  shinyjs::hide("density_alpha") # Hide fixed alpha slider
                } else {
                  shinyjs::hide("alpha")
                  shinyjs::hide("density_alpha")
                }
              } else if (aes == "group" && current_type == "density") {
                # For density plots, hide group input
                # (it's set automatically to match fill)
                shinyjs::hide("group")
              } else {
                if (aes %in% valid_aesthetics) {
                  shinyjs::show(aes)
                } else {
                  shinyjs::hide(aes)
                }
              }
            }

            # Update labels to show required indicators dynamically
            # X is always required for all chart types
            updateSelectInput(
              session,
              inputId = "x",
              label = if ("x" %in% chart_config$required) {
                tags$span(
                  tags$strong("X-axis"),
                  tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                )
              } else {
                "X-axis"
              }
            )

            # Y label - check if required for this chart type
            if ("y" %in% valid_aesthetics) {
              updateSelectInput(
                session,
                inputId = "y",
                label = if ("y" %in% chart_config$required) {
                  tags$span(
                    tags$strong("Y-axis"),
                    tags$span(
                      "*",
                      style = "color: #dc3545; margin-left: 2px;"
                    )
                  )
                } else {
                  "Y-axis"
                }
              )
            }

            # Update other aesthetic labels (all optional for current geoms)
            for (aes_field in c(
              "color",
              "fill",
              "size",
              "shape",
              "linetype",
              "group",
              "alpha"
            )) {
              if (aes_field %in% valid_aesthetics) {
                label_text <- switch(
                  aes_field,
                  color = "Color By",
                  fill = "Fill By",
                  size = "Size By",
                  shape = "Shape By",
                  linetype = "Line Type By",
                  group = "Group By",
                  alpha = "Alpha By"
                )

                updateSelectInput(
                  session,
                  inputId = aes_field,
                  label = if (aes_field %in% chart_config$required) {
                    tags$span(
                      tags$strong(label_text),
                      tags$span(
                        "*",
                        style = "color: #dc3545; margin-left: 2px;"
                      )
                    )
                  } else {
                    label_text
                  }
                )
              }
            }

            # Handle chart-specific options
            if ("position" %in% names(chart_config$specific)) {
              shinyjs::show("position")
              updateSelectInput(
                session,
                inputId = "position",
                choices = chart_config$specific$position,
                selected = input$position
              )
            } else {
              shinyjs::hide("position")
            }

            if (isTRUE(chart_config$specific$bins)) {
              shinyjs::show("bins")
            } else {
              shinyjs::hide("bins")
            }

            # Show donut checkbox only for pie charts
            if (current_type == "pie") {
              shinyjs::show("donut")
            } else {
              shinyjs::hide("donut")
            }
          }
        })

        list(
          expr = reactive({
            current_type <- input$type
            chart_config <- chart_aesthetics()[[current_type]]

            # Validate required fields
            if (!isTruthy(input$x) || length(input$x) == 0) {
              return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
            }

            # Check if y is required and missing
            if (
              "y" %in%
                chart_config$required &&
                (input$y == "(none)" || !isTruthy(input$y))
            ) {
              return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
            }

            # Build aesthetics
            aes_parts <- c(glue::glue("x = {input$x}"))

            # Add y if not "(none)" and valid for this chart
            if (
              input$y != "(none)" &&
                "y" %in% c(chart_config$required, chart_config$optional)
            ) {
              aes_parts <- c(aes_parts, glue::glue("y = {input$y}"))
            }

            # Add optional aesthetics if valid and not "(none)"
            if (input$color != "(none)" && "color" %in% chart_config$optional) {
              aes_parts <- c(aes_parts, glue::glue("colour = {input$color}"))
            }
            if (input$fill != "(none)" && "fill" %in% chart_config$optional) {
              # For histograms, bars, and density, convert to factor
              # for discrete colors
              if (
                current_type %in%
                  c("histogram", "bar", "boxplot", "violin", "density") &&
                  isTruthy(input$fill)
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("fill = as.factor({input$fill})")
                )
              } else {
                aes_parts <- c(aes_parts, glue::glue("fill = {input$fill}"))
              }
            }
            if (input$size != "(none)" && "size" %in% chart_config$optional) {
              aes_parts <- c(aes_parts, glue::glue("size = {input$size}"))
            }
            if (
              input$shape != "(none)" &&
                "shape" %in% chart_config$optional &&
                isTruthy(input$shape)
            ) {
              # Shape requires discrete/factor variables
              aes_parts <- c(
                aes_parts,
                glue::glue("shape = as.factor({input$shape})")
              )
            }
            if (
              input$linetype != "(none)" &&
                "linetype" %in% chart_config$optional
            ) {
              aes_parts <- c(
                aes_parts,
                glue::glue("linetype = {input$linetype}")
              )
            }
            # For density plots, always set group to match fill
            # This ensures proper grouping for statistical transformation
            if (current_type == "density") {
              if (input$fill != "(none)" && isTruthy(input$fill)) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("group = as.factor({input$fill})")
                )
              }
            } else if (
              input$group != "(none)" && "group" %in% chart_config$optional
            ) {
              # For non-density plots, use user-specified group if provided
              aes_parts <- c(aes_parts, glue::glue("group = {input$group}"))
            }
            # Alpha: for density plots, use fixed alpha parameter,
            # not aesthetic mapping
            if (
              current_type != "density" &&
                input$alpha != "(none)" &&
                "alpha" %in% chart_config$optional
            ) {
              aes_parts <- c(aes_parts, glue::glue("alpha = {input$alpha}"))
            }

            aes_text <- paste(aes_parts, collapse = ", ")

            # Build chart-specific call
            if (current_type == "bar") {
              if (input$y == "(none)") {
                geom_call <- glue::glue(
                  "ggplot2::geom_bar(position = '{input$position}')"
                )
              } else {
                geom_call <- glue::glue(
                  "ggplot2::geom_col(position = '{input$position}')"
                )
              }
            } else if (current_type == "histogram") {
              geom_call <- glue::glue(
                "ggplot2::geom_histogram(bins = {input$bins}, ",
                "position = '{input$position}')"
              )
            } else if (current_type == "point") {
              geom_call <- "ggplot2::geom_point()"
            } else if (current_type == "line") {
              geom_call <- "ggplot2::geom_line()"
            } else if (current_type == "boxplot") {
              geom_call <- "ggplot2::geom_boxplot()"
            } else if (current_type == "violin") {
              geom_call <- "ggplot2::geom_violin()"
            } else if (current_type == "density") {
              # Use fixed alpha value for density plots
              geom_call <- glue::glue(
                "ggplot2::geom_density(alpha = {input$density_alpha})"
              )
            } else if (current_type == "area") {
              geom_call <- "ggplot2::geom_area()"
            } else if (current_type == "pie") {
              # PIE CHART: Special handling required

              # Override x aesthetic: empty string for pie, numeric for donut
              if (input$donut) {
                # Numeric for donut (allows xlim)
                aes_parts[1] <- "x = 2"
              } else {
                # Empty string for regular pie
                aes_parts[1] <- 'x = ""'
              }

              # Ensure fill aesthetic uses the category column
              # (from x or fill)
              fill_added <- FALSE
              for (i in seq_along(aes_parts)) {
                if (grepl("^fill = ", aes_parts[i])) {
                  fill_added <- TRUE
                  break
                }
              }
              if (!fill_added) {
                # Use x column for fill if no fill aesthetic specified
                aes_parts <- c(aes_parts, glue::glue("fill = {input$x}"))
              }

              # Rebuild aes_text with modified parts
              aes_text <- paste(aes_parts, collapse = ", ")

              # Choose geom based on y
              if (input$y != "(none)") {
                geom_call <- "ggplot2::geom_col(width = 1)"
              } else {
                geom_call <- "ggplot2::geom_bar(width = 1)"
              }
            } else {
              # Fallback
              geom_call <- "ggplot2::geom_point()"
            }

            text <- glue::glue(
              "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
              "{geom_call}"
            )

            # Add theme_minimal() as default for all charts
            if (current_type == "pie") {
              # Pie charts: add polar coordinates and theme
              text <- glue::glue(
                "({text}) + ggplot2::coord_polar('y', start = 0) + ",
                "ggplot2::theme_minimal()"
              )

              # Add donut hole if requested
              if (input$donut) {
                text <- glue::glue("({text}) + ggplot2::xlim(c(0.2, 2.5))")
              }

              # For better pie chart appearance, remove axis elements
              text <- glue::glue(
                "({text}) + ggplot2::theme(",
                "axis.title = ggplot2::element_blank(), ",
                "axis.text = ggplot2::element_blank(), ",
                "axis.ticks = ggplot2::element_blank())"
              )
            } else {
              # Regular charts: apply theme_minimal()
              text <- glue::glue("({text}) + ggplot2::theme_minimal()")
            }

            parse(text = text)[[1]]
          }),
          state = list(
            type = reactive(input$type),
            x = reactive(input$x),
            y = reactive(input$y),
            color = reactive(input$color),
            fill = reactive(input$fill),
            size = reactive(input$size),
            shape = reactive(input$shape),
            linetype = reactive(input$linetype),
            group = reactive(input$group),
            alpha = reactive(input$alpha),
            density_alpha = reactive(input$density_alpha),
            position = reactive(input$position),
            bins = reactive(input$bins),
            donut = reactive(input$donut)
          )
        )
      }
    )
  }

  new_ggplot_transform_block(
    server = server,
    ui = ui,
    class = "ggplot_block",
    allow_empty_state = c(
      "y",
      "color",
      "fill",
      "size",
      "shape",
      "linetype",
      "group",
      "alpha"
    ),
    ...
  )
}
