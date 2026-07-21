#' Create SVG preview of facet layout
#'
#' @param facet_type "wrap" or "grid"
#' @param n_levels Number of unique facet levels (for wrap)
#' @param ncol_val Number of columns ("" for auto)
#' @param nrow_val Number of rows ("" for auto)
#' @param n_rows Number of row facet levels (for grid)
#' @param n_cols Number of column facet levels (for grid)
#' @param dir Direction for facet_wrap: "h" (horizontal) or "v" (vertical)
#' @return List with svg, status text, and status type
#' @noRd
create_facet_preview_svg <- function(
  facet_type,
  n_levels = 1,
  ncol_val = "",
  nrow_val = "",
  n_rows = 1,
  n_cols = 1,
  dir = "h"
) {
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
  max_width <- 200
  gap <- 4
  cell_size <- max_width / ncol_actual
  cell_size <- max(40, min(cell_size, 80))
  preview_width <- cell_size * ncol_actual
  preview_height <- cell_size * nrow_actual

  # Status based on validation
  if (!is_valid) {
    fill_color <- "rgba(244, 67, 54, 0.3)"
    stroke_color <- "#f44336"
    status_icon <- "\u274c"
    status_text <- sprintf(
      "Need %d slots but only have %d (increase ncol and/or nrow)",
      n_levels,
      total_slots
    )
  } else if (total_slots == n_levels) {
    fill_color <- "rgba(76, 175, 80, 0.3)"
    stroke_color <- "#4CAF50"
    status_icon <- "\u2713"
    if (facet_type == "wrap") {
      status_text <- sprintf(
        "Perfect fit: %d facets in %dx%d grid",
        n_levels,
        nrow_actual,
        ncol_actual
      )
    } else {
      status_text <- sprintf(
        "%d \u00d7 %d grid (%d total facets)",
        nrow_actual,
        ncol_actual,
        total_slots
      )
    }
  } else {
    fill_color <- "rgba(33, 150, 243, 0.3)"
    stroke_color <- "#2196F3"
    status_icon <- "\u2713"
    empty_slots <- total_slots - n_levels
    status_text <- sprintf(
      "%d facets in %dx%d grid (%d empty slot%s)",
      n_levels,
      nrow_actual,
      ncol_actual,
      empty_slots,
      if (empty_slots > 1) "s" else ""
    )
  }

  # Create cells
  cells <- list()
  facet_idx <- 1

  # Fill order depends on direction
  if (dir == "h") {
    # Horizontal: fill columns first (left to right, then top to bottom)
    for (row in 0:(nrow_actual - 1)) {
      for (col in 0:(ncol_actual - 1)) {
        x <- col * cell_size + gap
        y <- row * cell_size + gap
        w <- cell_size - 2 * gap
        h <- cell_size - 2 * gap

        is_filled <- facet_idx <= n_levels

        cells[[length(cells) + 1]] <- tags$rect(
          x = x,
          y = y,
          width = w,
          height = h,
          fill = if (is_filled) fill_color else "#f5f5f5",
          stroke = if (is_filled) stroke_color else "#ddd",
          `stroke-width` = if (is_filled) "2" else "1",
          rx = "3"
        )

        if (is_filled) {
          cells[[length(cells) + 1]] <- tags$text(
            x = x + w / 2,
            y = y + h / 2,
            `text-anchor` = "middle",
            `dominant-baseline` = "middle",
            style = sprintf(
              "font-size: %dpx; fill: %s; font-weight: bold;",
              as.integer(max(10, cell_size / 5)),
              stroke_color
            ),
            as.character(facet_idx)
          )
        }

        facet_idx <- facet_idx + 1
      }
    }
  } else {
    # Vertical: fill rows first (top to bottom, then left to right)
    for (col in 0:(ncol_actual - 1)) {
      for (row in 0:(nrow_actual - 1)) {
        x <- col * cell_size + gap
        y <- row * cell_size + gap
        w <- cell_size - 2 * gap
        h <- cell_size - 2 * gap

        is_filled <- facet_idx <= n_levels

        cells[[length(cells) + 1]] <- tags$rect(
          x = x,
          y = y,
          width = w,
          height = h,
          fill = if (is_filled) fill_color else "#f5f5f5",
          stroke = if (is_filled) stroke_color else "#ddd",
          `stroke-width` = if (is_filled) "2" else "1",
          rx = "3"
        )

        if (is_filled) {
          cells[[length(cells) + 1]] <- tags$text(
            x = x + w / 2,
            y = y + h / 2,
            `text-anchor` = "middle",
            `dominant-baseline` = "middle",
            style = sprintf(
              "font-size: %dpx; fill: %s; font-weight: bold;",
              as.integer(max(10, cell_size / 5)),
              stroke_color
            ),
            as.character(facet_idx)
          )
        }

        facet_idx <- facet_idx + 1
      }
    }
  }

  svg <- tags$svg(
    width = preview_width,
    height = preview_height,
    viewBox = sprintf(
      "0 0 %d %d",
      as.integer(preview_width),
      as.integer(preview_height)
    ),
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
#' @param scales Scale behavior: "fixed", "free", "free_x", "free_y"
#'   (default: "fixed")
#' @param labeller Labeller function: "label_value", "label_both",
#'   "label_parsed" (default: "label_value")
#' @param dir Direction for facet_wrap: "h" (horizontal) or "v" (vertical)
#'   (default: "h")
#' @param space Space behavior for facet_grid: "fixed", "free_x", "free_y"
#'   (default: "fixed")
#' @param ... Forwarded to [new_ggplot_transform_block()]
#'
#' @return A ggplot transform block object of class `facet_block`.
#'
#' @examples
#' # Create a facet wrap block
#' new_facet_block(facet_type = "wrap", facets = "cyl")
#'
#' # Create a facet grid block
#' new_facet_block(facet_type = "grid", rows = "cyl", cols = "gear")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   # Facet block requires a ggplot input
#'   serve(new_facet_block())
#' }
#'
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
                    2^length(r_rows()) # Fallback estimate
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
                    2^length(r_cols()) # Fallback estimate
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
                    3^length(r_facets()) # Fallback estimate
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

          # Reactive values. ncol/nrow normalize the character() constructor
          # default to "" (the "Auto" select value) so the expr reactive's
          # `!= ""` checks are length-safe before the first config echo.
          r_facet_type <- reactiveVal(facet_type)
          r_facets <- reactiveVal(facets)
          r_rows <- reactiveVal(rows)
          r_cols <- reactiveVal(cols)
          r_ncol <- reactiveVal(if (length(ncol)) ncol else "")
          r_nrow <- reactiveVal(if (length(nrow)) nrow else "")
          r_scales <- reactiveVal(scales)
          r_labeller <- reactiveVal(labeller)
          r_dir <- reactiveVal(dir)
          r_space <- reactiveVal(space)

          # Column metadata for the JS settings band, extracted from the
          # upstream ggplot's data (same shape as the ggplot block).
          r_col_meta <- reactive({
            d <- if (inherits(data(), "ggplot")) data()$data else NULL
            if (!is.data.frame(d)) {
              return(list())
            }
            lapply(names(d), function(col) {
              vals <- d[[col]]
              lbl <- attr(vals, "label")
              res <- list(
                name = col,
                type = if (is.numeric(vals)) "numeric" else "categorical",
                n_unique = length(unique(vals))
              )
              if (!is.null(lbl) && nzchar(lbl)) res$label <- lbl
              if (is.factor(vals)) res$levels <- as.list(levels(vals))
              res
            })
          })

          # Length-1-or-empty transport helper (ncol/nrow may be "" or "3").
          s1 <- function(v) if (length(v) == 1) v else ""

          # Push columns + config to JS (single observe; see ggplot-block.R).
          # Multi-column values go through as.list() so a length-1 selection
          # still serializes as a JSON array.
          observe({
            session$sendCustomMessage("gg-block-data", list(
              id = session$ns("gg_block"),
              block = "facet",
              columns = r_col_meta(),
              config = list(
                facet_type = r_facet_type(),
                facets = as.list(r_facets()),
                rows = as.list(r_rows()),
                cols = as.list(r_cols()),
                ncol = s1(r_ncol()),
                nrow = s1(r_nrow()),
                scales = r_scales(),
                labeller = r_labeller(),
                dir = r_dir(),
                space = r_space()
              )
            ))
          })

          # JS -> R: full-config echo through one action input, with the
          # identical() guard against R->JS->R loops (see ggplot-block.R).
          # Multi-column values arrive as lists -> character vectors.
          upd <- function(rv, v) {
            if (!identical(isolate(rv()), v)) rv(v)
          }
          chr_vec <- function(v) as.character(unlist(v))

          observeEvent(input$gg_block_action, {
            msg <- input$gg_block_action
            if (!identical(msg$action, "config")) {
              return()
            }
            if (!is.null(msg$facet_type)) upd(r_facet_type, msg$facet_type)
            if (!is.null(msg$facets)) upd(r_facets, chr_vec(msg$facets))
            if (!is.null(msg$rows)) upd(r_rows, chr_vec(msg$rows))
            if (!is.null(msg$cols)) upd(r_cols, chr_vec(msg$cols))
            if (!is.null(msg$ncol)) upd(r_ncol, msg$ncol)
            if (!is.null(msg$nrow)) upd(r_nrow, msg$nrow)
            if (!is.null(msg$scales)) upd(r_scales, msg$scales)
            if (!is.null(msg$labeller)) upd(r_labeller, msg$labeller)
            if (!is.null(msg$dir)) upd(r_dir, msg$dir)
            if (!is.null(msg$space)) upd(r_space, msg$space)
          })


          # Layout preview output
          output$layout_preview <- renderUI({
            current_type <- r_facet_type()

            if (current_type == "wrap") {
              # Nothing to preview yet, and nothing to say: the "Facet by"
              # field carries the amber required-empty cue (engine
              # requiredMap). No banner, no duplicated instruction down here.
              if (length(r_facets()) == 0) {
                return(NULL)
              }

              # Use actual data to count unique levels
              level_counts <- count_unique_levels()
              actual_levels <- level_counts$facets

              preview <- create_facet_preview_svg(
                "wrap",
                n_levels = actual_levels,
                ncol_val = r_ncol(),
                nrow_val = r_nrow(),
                dir = r_dir()
              )
            } else {
              # facet_grid: rows and cols are either-or, so neither field is
              # marked required; their help lines carry the instruction.
              if (length(r_rows()) == 0 && length(r_cols()) == 0) {
                return(NULL)
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

            # Determine status class based on validity
            status_class <- if (!preview$is_valid) {
              "error"
            } else if (preview$stroke_color == "#4CAF50") {
              "valid"
            } else {
              "warning"
            }

            tags$div(
              tags$div(
                class = "preview-svg-container",
                preview$svg
              ),
              tags$div(
                class = paste("preview-status", status_class),
                preview$status
              )
            )
          })

          list(
            expr = reactive({
              current_type <- r_facet_type()

              # One side of a facet formula as a language object. as.name()
              # reproduces non-syntactic column names without backticking;
              # `.` (the base R placeholder) stands in for an empty side.
              formula_side <- function(vars) {
                if (length(vars) == 0) {
                  quote(.)
                } else if (length(vars) == 1) {
                  as.name(vars[1])
                } else {
                  Reduce(function(a, b) call("+", a, b), lapply(vars, as.name))
                }
              }

              if (current_type == "wrap") {
                # Build facet_wrap call
                facet_vars <- r_facets()
                if (length(facet_vars) == 0) {
                  # No faceting - pass data through (`.(data)` marker)
                  return(quote(.(data)))
                }

                # One-sided formula: ~a or ~a + b
                facets_formula <- call("~", formula_side(facet_vars))

                # Named arguments, in the order a user would write them
                named <- list()
                if (r_ncol() != "") {
                  named$ncol <- as.numeric(r_ncol())
                }
                if (r_nrow() != "") {
                  named$nrow <- as.numeric(r_nrow())
                }
                named$scales <- r_scales()
                named$labeller <- r_labeller()
                if (r_dir() != "h") {
                  named$dir <- r_dir()
                }

                facet_call <- as.call(c(
                  list(quote(ggplot2::facet_wrap), facets_formula),
                  named
                ))
              } else {
                # Build facet_grid call
                row_vars <- r_rows()
                col_vars <- r_cols()

                if (length(row_vars) == 0 && length(col_vars) == 0) {
                  # No faceting - pass data through (`.(data)` marker)
                  return(quote(.(data)))
                }

                # Two-sided formula: rows ~ cols (either side may be `.`)
                grid_formula <- call(
                  "~",
                  formula_side(row_vars),
                  formula_side(col_vars)
                )

                named <- list()
                named$scales <- r_scales()
                named$labeller <- r_labeller()
                if (r_space() != "fixed") {
                  named$space <- r_space()
                }

                facet_call <- as.call(c(
                  list(quote(ggplot2::facet_grid), grid_formula),
                  named
                ))
              }

              # `.(data)` (not a bare `data`) so blockr.core's bquoted export
              # names the upstream block in place of the marker.
              gg_add(list(call(".", quote(data)), facet_call))
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
      # JS-first UI (settings-band pattern, see ggplot-block.R): the html
      # dependencies plus a container; inst/js/gg-blocks.js builds the gear
      # header and settings band (spec "facet") ABOVE the existing children,
      # so the SVG layout preview below survives band re-renders. The
      # preview shows only while the band is open (gg-blocks.css).
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "facet",
          div(
            class = "gg-preview",
            uiOutput(NS(id, "layout_preview"))
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(inherits(data, "ggplot"))
    },
    allow_empty_state = c("facets", "rows", "cols", "ncol", "nrow"),
    class = "facet_block",
    # `.(data)` markers above (rather than a bare `data`) let blockr.core
    # substitute the upstream block's name in place on export, giving
    # `plot + ggplot2::facet_wrap(~g)` instead of the `with(list(data =
    # plot), ...)` wrapper the default "quoted" type produces.
    expr_type = "bquoted",
    external_ctrl = TRUE,
    ...
  )
}
