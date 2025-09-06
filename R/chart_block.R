#' Universal chart block with selectable visualization types
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
#' @param alpha Transparency level (0-1, default 1.0)
#' @param position Position adjustment for certain geoms
#' @param bins Number of bins for histogram
#' @param theme ggplot2 theme to apply (default "minimal"). Options: "minimal",
#'   "classic", "dark", "light", "gray"
#' @param donut Whether to create donut chart when type is "pie" (default FALSE)
#' @param ... Forwarded to [new_plot_block()]
#'
#' @export
new_chart_block <- function(
  type = "point",
  x = character(),
  y = character(),
  color = character(),
  fill = character(),
  size = character(),
  shape = character(),
  linetype = character(),
  group = character(),
  alpha = 1.0,
  position = "stack",
  bins = 30,
  theme = "minimal",
  donut = FALSE,
  ...
) {
  # Define which aesthetics are valid for each chart type
  chart_aesthetics <- list(
    point = list(
      required = c("x", "y"),
      optional = c("color", "shape", "size", "alpha", "fill"),
      specific = list()
    ),
    bar = list(
      required = c("x"),
      optional = c("y", "fill", "color", "alpha"),
      specific = list(position = c("stack", "dodge", "fill"))
    ),
    line = list(
      required = c("x", "y"),
      optional = c("color", "linetype", "alpha", "group"),
      specific = list()
    ),
    boxplot = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    violin = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    density = list(
      required = c("x"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    area = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    histogram = list(
      required = c("x"),
      optional = c("fill", "color", "alpha"),
      specific = list(bins = TRUE)
    ),
    pie = list(
      required = c("x"), # x is categories, but rendered as x = ""
      optional = c("y", "fill", "alpha"),
      specific = list() # Could add donut = TRUE/FALSE later
    )
  )

  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          # Initialize reactive values
          r_type <- reactiveVal(type)
          r_x <- reactiveVal(x)
          r_y <- reactiveVal(if (length(y) == 0) "(none)" else y)
          r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
          r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
          r_size <- reactiveVal(if (length(size) == 0) "(none)" else size)
          r_shape <- reactiveVal(if (length(shape) == 0) "(none)" else shape)
          r_linetype <- reactiveVal(
            if (length(linetype) == 0) "(none)" else linetype
          )
          r_group <- reactiveVal(if (length(group) == 0) "(none)" else group)
          r_alpha <- reactiveVal(alpha)
          r_position <- reactiveVal(position)
          r_bins <- reactiveVal(bins)
          r_theme <- reactiveVal(theme)
          r_donut <- reactiveVal(donut)

          # Observe input changes
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(input$y))
          observeEvent(input$color, r_color(input$color))
          observeEvent(input$fill, r_fill(input$fill))
          observeEvent(input$size, r_size(input$size))
          observeEvent(input$shape, r_shape(input$shape))
          observeEvent(input$linetype, r_linetype(input$linetype))
          observeEvent(input$group, r_group(input$group))
          observeEvent(input$alpha, r_alpha(input$alpha))
          observeEvent(input$position, r_position(input$position))
          observeEvent(input$bins, r_bins(input$bins))
          observeEvent(input$theme, r_theme(input$theme))
          observeEvent(input$donut, r_donut(input$donut))

          # Update column-dependent inputs
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
                choices = c("(none)", cols()),
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
              updateSelectInput(
                session,
                inputId = "size",
                choices = c("(none)", cols()),
                selected = r_size()
              )
              updateSelectInput(
                session,
                inputId = "shape",
                choices = c("(none)", cols()),
                selected = r_shape()
              )
              updateSelectInput(
                session,
                inputId = "linetype",
                choices = c("(none)", cols()),
                selected = r_linetype()
              )
              updateSelectInput(
                session,
                inputId = "group",
                choices = c("(none)", cols()),
                selected = r_group()
              )
            }
          )

          # Dynamic UI visibility based on chart type
          observe({
            current_type <- r_type()
            chart_config <- chart_aesthetics[[current_type]]

            if (!is.null(chart_config)) {
              all_aesthetics <- c(
                "y",
                "color",
                "fill",
                "size",
                "shape",
                "linetype",
                "group"
              )
              valid_aesthetics <- c(
                chart_config$required,
                chart_config$optional
              )
              # x is always shown
              valid_aesthetics <- valid_aesthetics[valid_aesthetics != "x"]

              # Hide/show aesthetic inputs based on validity
              for (aes in all_aesthetics) {
                if (aes %in% valid_aesthetics) {
                  shinyjs::show(aes)
                } else {
                  shinyjs::hide(aes)
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
                "group"
              )) {
                if (aes_field %in% valid_aesthetics) {
                  label_text <- switch(
                    aes_field,
                    color = "Color By",
                    fill = "Fill By",
                    size = "Size By",
                    shape = "Shape By",
                    linetype = "Line Type By",
                    group = "Group By"
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
                  selected = r_position()
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
              current_type <- r_type()
              chart_config <- chart_aesthetics[[current_type]]

              # Validate required fields
              if (!isTruthy(r_x()) || length(r_x()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }

              # Check if y is required and missing
              if (
                "y" %in%
                  chart_config$required &&
                  (r_y() == "(none)" || !isTruthy(r_y()))
              ) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }

              # Build aesthetics
              aes_parts <- c(glue::glue("x = {r_x()}"))

              # Add y if not "(none)" and valid for this chart
              if (
                r_y() != "(none)" &&
                  "y" %in% c(chart_config$required, chart_config$optional)
              ) {
                aes_parts <- c(aes_parts, glue::glue("y = {r_y()}"))
              }

              # Add optional aesthetics if valid and not "(none)"
              if (r_color() != "(none)" && "color" %in% chart_config$optional) {
                aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
              }
              if (r_fill() != "(none)" && "fill" %in% chart_config$optional) {
                aes_parts <- c(aes_parts, glue::glue("fill = {r_fill()}"))
              }
              if (r_size() != "(none)" && "size" %in% chart_config$optional) {
                aes_parts <- c(aes_parts, glue::glue("size = {r_size()}"))
              }
              if (r_shape() != "(none)" && "shape" %in% chart_config$optional) {
                aes_parts <- c(aes_parts, glue::glue("shape = {r_shape()}"))
              }
              if (
                r_linetype() != "(none)" &&
                  "linetype" %in% chart_config$optional
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("linetype = {r_linetype()}")
                )
              }
              if (r_group() != "(none)" && "group" %in% chart_config$optional) {
                aes_parts <- c(aes_parts, glue::glue("group = {r_group()}"))
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build chart-specific call
              geom_args <- glue::glue("alpha = {r_alpha()}")

              if (current_type == "bar") {
                if (r_y() == "(none)") {
                  geom_call <- glue::glue(
                    "ggplot2::geom_bar(position = '{r_position()}', ",
                    "{geom_args})"
                  )
                } else {
                  geom_call <- glue::glue(
                    "ggplot2::geom_col(position = '{r_position()}', ",
                    "{geom_args})"
                  )
                }
              } else if (current_type == "histogram") {
                geom_call <- glue::glue(
                  "ggplot2::geom_histogram(bins = {r_bins()}, {geom_args})"
                )
              } else if (current_type == "point") {
                geom_call <- glue::glue("ggplot2::geom_point({geom_args})")
              } else if (current_type == "line") {
                geom_call <- glue::glue("ggplot2::geom_line({geom_args})")
              } else if (current_type == "boxplot") {
                geom_call <- glue::glue("ggplot2::geom_boxplot({geom_args})")
              } else if (current_type == "violin") {
                geom_call <- glue::glue("ggplot2::geom_violin({geom_args})")
              } else if (current_type == "density") {
                geom_call <- glue::glue("ggplot2::geom_density({geom_args})")
              } else if (current_type == "area") {
                geom_call <- glue::glue("ggplot2::geom_area({geom_args})")
              } else if (current_type == "pie") {
                # PIE CHART: Special handling required

                # Override x aesthetic: empty string for pie, numeric for donut
                if (r_donut()) {
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
                  aes_parts <- c(aes_parts, glue::glue("fill = {r_x()}"))
                }

                # Rebuild aes_text with modified parts
                aes_text <- paste(aes_parts, collapse = ", ")

                # Choose geom based on y
                if (r_y() != "(none)") {
                  geom_call <- glue::glue(
                    "ggplot2::geom_col(width = 1, {geom_args})"
                  )
                } else {
                  geom_call <- glue::glue(
                    "ggplot2::geom_bar(width = 1, {geom_args})"
                  )
                }
              } else {
                # Fallback
                geom_call <- glue::glue("ggplot2::geom_point({geom_args})")
              }

              text <- glue::glue(
                "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
                "{geom_call}"
              )

              # Add theme based on selection
              if (current_type == "pie") {
                # Pie charts: add polar coordinates first, then apply
                # selected theme
                text <- glue::glue(
                  "({text}) + ggplot2::coord_polar('y', start = 0)"
                )

                # Apply selected theme (pie charts can use any theme)
                if (r_theme() == "minimal") {
                  text <- glue::glue("({text}) + ggplot2::theme_minimal()")
                } else if (r_theme() == "classic") {
                  text <- glue::glue("({text}) + ggplot2::theme_classic()")
                } else if (r_theme() == "gray") {
                  text <- glue::glue("({text}) + ggplot2::theme_gray()")
                }

                # Add donut hole if requested
                if (r_donut()) {
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
                # Regular charts: apply selected theme
                if (r_theme() == "minimal") {
                  text <- glue::glue("({text}) + ggplot2::theme_minimal()")
                } else if (r_theme() == "classic") {
                  text <- glue::glue("({text}) + ggplot2::theme_classic()")
                } else if (r_theme() == "gray") {
                  text <- glue::glue("({text}) + ggplot2::theme_gray()")
                }
              }

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y,
              color = r_color,
              fill = r_fill,
              size = r_size,
              shape = r_shape,
              linetype = r_linetype,
              group = r_group,
              alpha = r_alpha,
              position = r_position,
              bins = r_bins,
              theme = r_theme,
              donut = r_donut
            )
          )
        }
      )
    },
    function(id) {
      # Helper function to create aesthetic labels with required indicators
      make_aesthetic_label <- function(name, field_id, chart_type) {
        chart_config <- chart_aesthetics[[chart_type]]
        if (!is.null(chart_config)) {
          # Check if field is required for this chart type
          is_required <- field_id %in% chart_config$required
          if (is_required) {
            return(tags$span(
              tags$strong(name),
              tags$span("*", style = "color: #dc3545; margin-left: 2px;")
            ))
          }
        }
        name
      }

      # Need shinyjs for dynamic UI
      tagList(
        shinyjs::useShinyjs(),
        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Add custom CSS for chart type and theme selectors
          tags$style(HTML(
            "
            .chart-type-selector .btn-group-toggle {
              display: flex;
              flex-wrap: wrap;
              gap: 5px;
            }
            .chart-type-selector .btn {
              display: flex;
              flex-direction: column;
              align-items: center;
              padding: 8px 12px;
              min-width: 80px;
            }
            .chart-type-selector .btn i {
              font-size: 1.2em;
              margin-bottom: 4px;
            }
            .chart-type-selector .btn span {
              font-size: 0.85em;
            }
            .theme-selector .btn-group-toggle {
              display: flex;
              flex-wrap: wrap;
              gap: 5px;
            }
            .theme-selector .btn {
              display: flex;
              flex-direction: column;
              align-items: center;
              padding: 6px 10px;
              min-width: 65px;
            }
            .theme-selector .btn i {
              font-size: 1.1em;
              margin-bottom: 3px;
            }
            .theme-selector .btn span {
              font-size: 0.8em;
            }
          "
          )),

          # Set container query context
          block_container_script(),

          div(
            class = "block-form-grid",

            # Chart Type Selection Section (always visible)
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
                      tags$div(icon("chart-area"), tags$span("Violin")),
                      tags$div(icon("wave-square"), tags$span("Density")),
                      tags$div(icon("chart-area"), tags$span("Area")),
                      tags$div(icon("chart-bar"), tags$span("Histogram")),
                      tags$div(icon("chart-pie"), tags$span("Pie"))
                    ),
                    choiceValues = c(
                      "point",
                      "bar",
                      "line",
                      "boxplot",
                      "violin",
                      "density",
                      "area",
                      "histogram",
                      "pie"
                    ),
                    selected = type,
                    status = "primary",
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
                "Aesthetic Mapping",
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
                    choices = x,
                    selected = x,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "y"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "y"),
                    label = make_aesthetic_label("Y-axis", "y", type),
                    choices = c("(none)", y),
                    selected = if (length(y) == 0) "(none)" else y,
                    width = "100%"
                  )
                ),
                # Other aesthetic mappings
                div(
                  id = NS(id, "color"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "color"),
                    label = make_aesthetic_label("Color By", "color", type),
                    choices = c("(none)", color),
                    selected = if (length(color) == 0) "(none)" else color,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "fill"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "fill"),
                    label = make_aesthetic_label("Fill By", "fill", type),
                    choices = c("(none)", fill),
                    selected = if (length(fill) == 0) "(none)" else fill,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "size"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "size"),
                    label = make_aesthetic_label("Size By", "size", type),
                    choices = c("(none)", size),
                    selected = if (length(size) == 0) "(none)" else size,
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "shape"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "shape"),
                    label = make_aesthetic_label("Shape By", "shape", type),
                    choices = c("(none)", shape),
                    selected = if (length(shape) == 0) "(none)" else shape,
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
                    choices = c("(none)", linetype),
                    selected = if (length(linetype) == 0) {
                      "(none)"
                    } else {
                      linetype
                    },
                    width = "100%"
                  )
                ),
                div(
                  id = NS(id, "group"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "group"),
                    label = make_aesthetic_label("Group By", "group", type),
                    choices = c("(none)", group),
                    selected = if (length(group) == 0) "(none)" else group,
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
                ),
                # Theme selector - allow natural grid positioning
                div(
                  class = "block-input-wrapper theme-selector",
                  tags$label(
                    "Theme",
                    class = "control-label",
                    style = "margin-bottom: 8px;"
                  ),
                  shinyWidgets::radioGroupButtons(
                    inputId = NS(id, "theme"),
                    label = NULL,
                    choiceNames = list(
                      tags$div(icon("minus"), tags$span("Minimal")),
                      tags$div(icon("columns"), tags$span("Classic")),
                      tags$div(icon("palette"), tags$span("Gray"))
                    ),
                    choiceValues = c(
                      "minimal",
                      "classic",
                      "gray"
                    ),
                    selected = theme,
                    status = "primary",
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
    class = "chart_block",
    allow_empty_state = c(
      "y",
      "color",
      "fill",
      "size",
      "shape",
      "linetype",
      "group"
    ),
    ...
  )
}
