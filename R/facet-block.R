#' Create SVG preview of facet layout
#'
#' @param facet_type "wrap" or "grid"
#' @param n_levels Number of unique facet levels (for wrap)
#' @param ncol_val Number of columns ("" for auto)
#' @param nrow_val Number of rows ("" for auto)
#' @param n_rows Number of row facet levels (for grid)
#' @param n_cols Number of column facet levels (for grid)
#' @return List with svg, status text, and status type
#' @noRd
create_facet_preview_svg <- function(facet_type, n_levels = 1, ncol_val = "", nrow_val = "", n_rows = 1, n_cols = 1) {
  if (facet_type == "wrap") {
    # Use ggplot2::wrap_dims for wrap layout calculation
    result <- tryCatch(
      {
        if (ncol_val == "" && nrow_val == "") {
          # Full auto mode
          dims <- ggplot2::wrap_dims(n_levels, nrow = NULL, ncol = NULL)
        } else if (nrow_val != "" && ncol_val != "") {
          # Both specified
          dims <- ggplot2::wrap_dims(
            n_levels,
            nrow = as.numeric(nrow_val),
            ncol = as.numeric(ncol_val)
          )
        } else if (ncol_val != "") {
          # Only ncol specified
          dims <- ggplot2::wrap_dims(
            n_levels,
            nrow = NULL,
            ncol = as.numeric(ncol_val)
          )
        } else {
          # Only nrow specified
          dims <- ggplot2::wrap_dims(
            n_levels,
            nrow = as.numeric(nrow_val),
            ncol = NULL
          )
        }
        list(dims = dims, is_valid = TRUE, error_msg = NULL)
      },
      error = function(e) {
        if (nrow_val != "" && ncol_val != "") {
          dims <- c(as.numeric(nrow_val), as.numeric(ncol_val))
        } else {
          dims <- c(1, 1)
        }
        list(dims = dims, is_valid = FALSE, error_msg = conditionMessage(e))
      }
    )

    nrow_actual <- result$dims[1]
    ncol_actual <- result$dims[2]
    is_valid <- result$is_valid
    total_slots <- nrow_actual * ncol_actual

  } else {
    # facet_grid: simple rows x cols
    nrow_actual <- n_rows
    ncol_actual <- n_cols
    is_valid <- TRUE
    total_slots <- nrow_actual * ncol_actual
    n_levels <- total_slots
  }

  # SVG dimensions
  max_width <- 300
  gap <- 4
  cell_size <- max_width / ncol_actual
  cell_size <- max(50, min(cell_size, 100))
  preview_width <- cell_size * ncol_actual
  preview_height <- cell_size * nrow_actual

  # Status based on validation
  if (!is_valid) {
    fill_color <- "rgba(244, 67, 54, 0.3)"
    stroke_color <- "#f44336"
    status_icon <- "\u274c"
    status_text <- sprintf(
      "Need %d slots but only have %d (increase ncol and/or nrow)",
      n_levels, total_slots
    )
  } else if (total_slots == n_levels) {
    fill_color <- "rgba(76, 175, 80, 0.3)"
    stroke_color <- "#4CAF50"
    status_icon <- "\u2713"
    if (facet_type == "wrap") {
      status_text <- sprintf(
        "Perfect fit: %d facets in %dx%d grid",
        n_levels, nrow_actual, ncol_actual
      )
    } else {
      status_text <- sprintf(
        "%d × %d grid (%d total facets)",
        nrow_actual, ncol_actual, total_slots
      )
    }
  } else {
    fill_color <- "rgba(33, 150, 243, 0.3)"
    stroke_color <- "#2196F3"
    status_icon <- "\u2713"
    empty_slots <- total_slots - n_levels
    status_text <- sprintf(
      "%d facets in %dx%d grid (%d empty slot%s)",
      n_levels, nrow_actual, ncol_actual,
      empty_slots, if (empty_slots > 1) "s" else ""
    )
  }

  # Create cells
  cells <- list()
  facet_idx <- 1
  for (row in 0:(nrow_actual - 1)) {
    for (col in 0:(ncol_actual - 1)) {
      x <- col * cell_size + gap
      y <- row * cell_size + gap
      w <- cell_size - 2 * gap
      h <- cell_size - 2 * gap

      is_filled <- facet_idx <= n_levels

      cells[[length(cells) + 1]] <- tags$rect(
        x = x, y = y, width = w, height = h,
        fill = if (is_filled) fill_color else "#f5f5f5",
        stroke = if (is_filled) stroke_color else "#ddd",
        `stroke-width` = if (is_filled) "2" else "1",
        rx = "3"
      )

      if (is_filled) {
        cells[[length(cells) + 1]] <- tags$text(
          x = x + w / 2, y = y + h / 2,
          `text-anchor` = "middle",
          `dominant-baseline` = "middle",
          style = sprintf(
            "font-size: %dpx; fill: %s; font-weight: bold;",
            max(10, cell_size / 5), stroke_color
          ),
          as.character(facet_idx)
        )
      }

      facet_idx <- facet_idx + 1
    }
  }

  svg <- tags$svg(
    width = preview_width,
    height = preview_height,
    viewBox = sprintf("0 0 %d %d", preview_width, preview_height),
    style = paste(
      "border: 1px solid #ddd; background: white;",
      "border-radius: 4px;"
    ),
    do.call(tagList, cells)
  )

  list(
    svg = svg,
    status = status_text,
    status_icon = status_icon,
    is_valid = is_valid,
    stroke_color = stroke_color
  )
}

#' Facet Block
#'
#' Applies faceting to a ggplot object using facet_wrap() or facet_grid().
#' Accepts a single ggplot input and adds faceting based on data columns.
#'
#' @param facet_type Type of faceting: "wrap" or "grid" (default: "wrap")
#' @param facets Column(s) to facet by for facet_wrap (character vector)
#' @param rows Column(s) for row facets in facet_grid (character vector)
#' @param cols Column(s) for column facets in facet_grid (character vector)
#' @param ncol Number of columns for facet_wrap (default: NULL for auto)
#' @param nrow Number of rows for facet_wrap (default: NULL for auto)
#' @param scales Scale behavior: "fixed", "free", "free_x", "free_y" (default: "fixed")
#' @param labeller Labeller function: "label_value", "label_both", "label_parsed" (default: "label_value")
#' @param dir Direction for facet_wrap: "h" (horizontal) or "v" (vertical) (default: "h")
#' @param space Space behavior for facet_grid: "fixed", "free_x", "free_y" (default: "fixed")
#' @param ... Forwarded to [new_ggplot_transform_block()]
#' @export
new_facet_block <- function(
  facet_type = "wrap",
  facets = character(),
  rows = character(),
  cols = character(),
  ncol = character(),
  nrow = character(),
  scales = "fixed",
  labeller = "label_value",
  dir = "h",
  space = "fixed",
  ...
) {
  new_ggplot_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Get column names from the data
          cols_data <- reactive({
            if (inherits(data(), "ggplot")) {
              # Extract data from ggplot object
              plot_data <- data()$data
              if (is.data.frame(plot_data)) {
                return(colnames(plot_data))
              }
            }
            character()
          })

          # Count actual unique levels for facet variables
          count_unique_levels <- reactive({
            if (inherits(data(), "ggplot")) {
              plot_data <- data()$data
              if (is.data.frame(plot_data)) {
                # Count for rows
                n_rows <- if (length(r_rows()) > 0) {
                  # Check if columns exist in data
                  if (all(r_rows() %in% colnames(plot_data))) {
                    nrow(unique(plot_data[, r_rows(), drop = FALSE]))
                  } else {
                    2^length(r_rows())  # Fallback estimate
                  }
                } else {
                  1
                }

                # Count for cols
                n_cols <- if (length(r_cols()) > 0) {
                  # Check if columns exist in data
                  if (all(r_cols() %in% colnames(plot_data))) {
                    nrow(unique(plot_data[, r_cols(), drop = FALSE]))
                  } else {
                    2^length(r_cols())  # Fallback estimate
                  }
                } else {
                  1
                }

                # Count for wrap facets
                n_facets <- if (length(r_facets()) > 0) {
                  # Check if columns exist in data
                  if (all(r_facets() %in% colnames(plot_data))) {
                    nrow(unique(plot_data[, r_facets(), drop = FALSE]))
                  } else {
                    3^length(r_facets())  # Fallback estimate
                  }
                } else {
                  0
                }

                return(list(rows = n_rows, cols = n_cols, facets = n_facets))
              }
            }
            # Fallback estimates if no data available
            list(
              rows = if (length(r_rows()) > 0) 2^length(r_rows()) else 1,
              cols = if (length(r_cols()) > 0) 2^length(r_cols()) else 1,
              facets = if (length(r_facets()) > 0) 3^length(r_facets()) else 0
            )
          })

          # Reactive values
          r_facet_type <- reactiveVal(facet_type)
          r_facets <- reactiveVal(facets)
          r_rows <- reactiveVal(rows)
          r_cols <- reactiveVal(cols)
          r_ncol <- reactiveVal(ncol)
          r_nrow <- reactiveVal(nrow)
          r_scales <- reactiveVal(scales)
          r_labeller <- reactiveVal(labeller)
          r_dir <- reactiveVal(dir)
          r_space <- reactiveVal(space)

          # Update reactive values from inputs
          observeEvent(input$facet_type, {
            r_facet_type(input$facet_type)

            # Toggle visibility of variable selectors based on facet type
            if (input$facet_type == "wrap") {
              shinyjs::show("wrap_vars")
              shinyjs::hide("grid_vars_row")
              shinyjs::hide("grid_vars_col")
              shinyjs::show("layout_section")
              shinyjs::hide("space_option")
            } else {
              shinyjs::hide("wrap_vars")
              shinyjs::show("grid_vars_row")
              shinyjs::show("grid_vars_col")
              shinyjs::hide("layout_section")
              shinyjs::show("space_option")
            }
          })

          observeEvent(input$facets, r_facets(input$facets), ignoreNULL = FALSE)
          observeEvent(input$rows, r_rows(input$rows), ignoreNULL = FALSE)
          observeEvent(input$cols, r_cols(input$cols), ignoreNULL = FALSE)
          observeEvent(input$ncol, r_ncol(input$ncol))
          observeEvent(input$nrow, r_nrow(input$nrow))
          observeEvent(input$scales, r_scales(input$scales))
          observeEvent(input$labeller, r_labeller(input$labeller))
          observeEvent(input$dir, r_dir(input$dir))
          observeEvent(input$space, r_space(input$space))

          # Initialize visibility based on default facet_type
          observe({
            # This runs once on initialization
            if (facet_type == "wrap") {
              shinyjs::show("wrap_vars")
              shinyjs::hide("grid_vars_row")
              shinyjs::hide("grid_vars_col")
              shinyjs::show("layout_section")
              shinyjs::hide("space_option")
            } else {
              shinyjs::hide("wrap_vars")
              shinyjs::show("grid_vars_row")
              shinyjs::show("grid_vars_col")
              shinyjs::hide("layout_section")
              shinyjs::show("space_option")
            }
          })

          # Update column choices when data changes
          observeEvent(
            cols_data(),
            {
              available_cols <- cols_data()
              if (length(available_cols) > 0) {
                # Update facets selector
                shiny::updateSelectizeInput(
                  session,
                  inputId = "facets",
                  choices = available_cols,
                  selected = r_facets()
                )
                # Update rows selector
                shiny::updateSelectizeInput(
                  session,
                  inputId = "rows",
                  choices = available_cols,
                  selected = r_rows()
                )
                # Update cols selector
                shiny::updateSelectizeInput(
                  session,
                  inputId = "cols",
                  choices = available_cols,
                  selected = r_cols()
                )
              }
            }
          )

          # Layout preview output
          output$layout_preview <- renderUI({
            current_type <- r_facet_type()

            if (current_type == "wrap") {
              # Calculate number of unique levels for preview
              if (length(r_facets()) == 0) {
                return(tags$div(
                  style = paste(
                    "padding: 10px; background: #fff3cd;",
                    "border-radius: 4px; margin-bottom: 15px;"
                  ),
                  tags$strong("\u26a0\ufe0f Select facet variable(s)"),
                  tags$br(),
                  "Choose one or more columns to facet by"
                ))
              }

              # Use actual data to count unique levels
              level_counts <- count_unique_levels()
              actual_levels <- level_counts$facets

              preview <- create_facet_preview_svg(
                "wrap",
                n_levels = actual_levels,
                ncol_val = r_ncol(),
                nrow_val = r_nrow()
              )
            } else {
              # facet_grid
              if (length(r_rows()) == 0 && length(r_cols()) == 0) {
                return(tags$div(
                  style = paste(
                    "padding: 10px; background: #fff3cd;",
                    "border-radius: 4px; margin-bottom: 15px;"
                  ),
                  tags$strong("\u26a0\ufe0f Select row and/or column variables"),
                  tags$br(),
                  "Choose variables for rows and/or columns"
                ))
              }

              # Use actual data to count unique levels
              level_counts <- count_unique_levels()
              n_row_levels <- level_counts$rows
              n_col_levels <- level_counts$cols

              preview <- create_facet_preview_svg(
                "grid",
                n_rows = n_row_levels,
                n_cols = n_col_levels
              )
            }

            # Determine background color based on status
            bg_color <- if (!preview$is_valid) {
              "#f44336"
            } else if (preview$stroke_color == "#4CAF50") {
              "#4CAF50"
            } else {
              "#2196F3"
            }

            tags$div(
              style = "margin-bottom: 15px;",
              tags$div(
                style = "text-align: center; margin-bottom: 10px;",
                preview$svg
              ),
              tags$div(
                style = sprintf(
                  paste(
                    "padding: 8px; border-radius: 4px;",
                    "background-color: %s; color: white;"
                  ),
                  bg_color
                ),
                tags$strong(paste(preview$status_icon, preview$status))
              )
            )
          })

          list(
            expr = reactive({
              current_type <- r_facet_type()

              if (current_type == "wrap") {
                # Build facet_wrap call
                facet_vars <- r_facets()
                if (length(facet_vars) == 0) {
                  # No faceting - return data as-is (wrapped in parens to make it a call)
                  return(parse(text = "(data)")[[1]])
                }

                # Build facets formula
                if (length(facet_vars) == 1) {
                  facets_formula <- glue::glue("~{facet_vars[1]}")
                } else {
                  facets_formula <- glue::glue("~{paste(facet_vars, collapse = ' + ')}")
                }

                # Build function call arguments
                args <- list()
                if (r_ncol() != "") args$ncol <- as.numeric(r_ncol())
                if (r_nrow() != "") args$nrow <- as.numeric(r_nrow())
                args$scales <- r_scales()
                args$labeller <- r_labeller()
                if (r_dir() != "h") args$dir <- r_dir()

                # Build the call
                facet_call <- glue::glue(
                  "ggplot2::facet_wrap({facets_formula}",
                  if (length(args) > 0) {
                    paste0(", ", paste(
                      mapply(function(name, value) {
                        if (is.numeric(value)) {
                          glue::glue("{name} = {value}")
                        } else {
                          glue::glue("{name} = '{value}'")
                        }
                      }, names(args), args, SIMPLIFY = TRUE),
                      collapse = ", "
                    ))
                  } else "",
                  ")"
                )

                text <- glue::glue("data + {facet_call}")
                parse(text = text)[[1]]

              } else {
                # Build facet_grid call
                row_vars <- r_rows()
                col_vars <- r_cols()

                if (length(row_vars) == 0 && length(col_vars) == 0) {
                  # No faceting - return data as-is (wrapped in parens to make it a call)
                  return(parse(text = "(data)")[[1]])
                }

                # Build rows formula
                if (length(row_vars) == 0) {
                  rows_part <- "."
                } else if (length(row_vars) == 1) {
                  rows_part <- row_vars[1]
                } else {
                  rows_part <- paste(row_vars, collapse = " + ")
                }

                # Build cols formula
                if (length(col_vars) == 0) {
                  cols_part <- "."
                } else if (length(col_vars) == 1) {
                  cols_part <- col_vars[1]
                } else {
                  cols_part <- paste(col_vars, collapse = " + ")
                }

                grid_formula <- glue::glue("{rows_part} ~ {cols_part}")

                # Build function call arguments
                args <- list()
                args$scales <- r_scales()
                args$labeller <- r_labeller()
                if (r_space() != "fixed") args$space <- r_space()

                # Build the call
                facet_call <- glue::glue(
                  "ggplot2::facet_grid({grid_formula}",
                  if (length(args) > 0) {
                    paste0(", ", paste(
                      mapply(function(name, value) {
                        glue::glue("{name} = '{value}'")
                      }, names(args), args, SIMPLIFY = TRUE),
                      collapse = ", "
                    ))
                  } else "",
                  ")"
                )

                text <- glue::glue("data + {facet_call}")
                parse(text = text)[[1]]
              }
            }),
            state = list(
              facet_type = r_facet_type,
              facets = r_facets,
              rows = r_rows,
              cols = r_cols,
              ncol = r_ncol,
              nrow = r_nrow,
              scales = r_scales,
              labeller = r_labeller,
              dir = r_dir,
              space = r_space
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Add custom CSS for facet type selector
          tags$style(HTML(
            "
            .facet-type-selector .btn-group-toggle {
              display: flex;
              flex-wrap: wrap;
              gap: 5px;
            }
            .facet-type-selector .btn {
              display: flex;
              flex-direction: column;
              align-items: center;
              padding: 8px 12px;
              min-width: 80px;
            }
            .facet-type-selector .btn i {
              font-size: 1.2em;
              margin-bottom: 4px;
            }
            .facet-type-selector .btn span {
              font-size: 0.85em;
            }
          "
          )),

          # Set container query context
          block_container_script(),

          # Display visual facet layout preview at the top
          uiOutput(NS(id, "layout_preview")),

          div(
            class = "block-form-grid",

            # Facet Type Section
            div(
              class = "block-section",
              tags$h4("Facet Type"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper facet-type-selector",
                  style = "grid-column: 1 / -1;",
                  shinyWidgets::radioGroupButtons(
                    inputId = NS(id, "facet_type"),
                    label = NULL,
                    choiceNames = list(
                      tags$div(icon("border-all"), tags$span("Wrap")),
                      tags$div(icon("th"), tags$span("Grid"))
                    ),
                    choiceValues = c("wrap", "grid"),
                    selected = facet_type,
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
                )
              )
            ),

            # Variables Section - changes based on facet type
            div(
              class = "block-section",
              tags$h4("Variables"),
              div(
                class = "block-section-grid",
                # For facet_wrap
                shinyjs::hidden(
                  div(
                    id = NS(id, "wrap_vars"),
                    class = "block-input-wrapper",
                    style = "grid-column: 1 / -1;",
                    selectizeInput(
                      NS(id, "facets"),
                      "Facet By",
                      choices = facets,
                      selected = facets,
                      multiple = TRUE,
                      width = "100%",
                      options = list(
                        placeholder = "Select column(s) to facet by",
                        plugins = list("remove_button")
                      )
                    )
                  )
                ),
                # For facet_grid
                shinyjs::hidden(
                  div(
                    id = NS(id, "grid_vars_row"),
                    class = "block-input-wrapper",
                    selectizeInput(
                      NS(id, "rows"),
                      "Row Facets",
                      choices = rows,
                      selected = rows,
                      multiple = TRUE,
                      width = "100%",
                      options = list(
                        placeholder = "Select column(s) for rows",
                        plugins = list("remove_button")
                      )
                    )
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = NS(id, "grid_vars_col"),
                    class = "block-input-wrapper",
                    selectizeInput(
                      NS(id, "cols"),
                      "Column Facets",
                      choices = cols,
                      selected = cols,
                      multiple = TRUE,
                      width = "100%",
                      options = list(
                        placeholder = "Select column(s) for columns",
                        plugins = list("remove_button")
                      )
                    )
                  )
                )
              )
            ),

            # Layout Section (for facet_wrap only)
            shinyjs::hidden(
              div(
                id = NS(id, "layout_section"),
                class = "block-section",
                tags$h4("Layout"),
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "ncol"),
                      "Columns",
                      choices = c(
                        "Auto" = "",
                        "1" = "1",
                        "2" = "2",
                        "3" = "3",
                        "4" = "4",
                        "5" = "5"
                      ),
                      selected = ncol,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "nrow"),
                      "Rows",
                      choices = c(
                        "Auto" = "",
                        "1" = "1",
                        "2" = "2",
                        "3" = "3",
                        "4" = "4",
                        "5" = "5"
                      ),
                      selected = nrow,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "dir"),
                      "Direction",
                      choices = c(
                        "Horizontal" = "h",
                        "Vertical" = "v"
                      ),
                      selected = dir,
                      width = "100%"
                    )
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
                    NS(id, "scales"),
                    "Scales",
                    choices = c(
                      "Fixed" = "fixed",
                      "Free" = "free",
                      "Free X" = "free_x",
                      "Free Y" = "free_y"
                    ),
                    selected = scales,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "labeller"),
                    "Labels",
                    choices = c(
                      "Value only" = "label_value",
                      "Variable and value" = "label_both",
                      "Parsed expressions" = "label_parsed"
                    ),
                    selected = labeller,
                    width = "100%"
                  )
                ),
                # Space option for facet_grid only
                shinyjs::hidden(
                  div(
                    id = NS(id, "space_option"),
                    class = "block-input-wrapper",
                    selectInput(
                      NS(id, "space"),
                      "Space",
                      choices = c(
                        "Fixed" = "fixed",
                        "Free X" = "free_x",
                        "Free Y" = "free_y"
                      ),
                      selected = space,
                      width = "100%"
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
      stopifnot(inherits(data, "ggplot"))
    },
    allow_empty_state = c("facets", "rows", "cols", "ncol", "nrow"),
    class = "facet_block",
    ...
  )
}