#' Layout of the facet panels, for the preview in the gear tray
#'
#' @param facet_type "wrap" or "grid"
#' @param n_levels Number of panels (for wrap)
#' @param ncol_val,nrow_val Columns and rows as chosen ("" for auto, wrap)
#' @param n_rows,n_cols Row and column levels (for grid)
#' @param dir Fill order for wrap: "h" (across) or "v" (down)
#' @return See layout_preview().
#' @noRd
facet_layout_preview <- function(
  facet_type,
  n_levels = 1,
  ncol_val = "",
  nrow_val = "",
  n_rows = 1,
  n_cols = 1,
  dir = "h"
) {
  if (facet_type == "wrap") {
    return(layout_preview(n_levels, ncol_val, nrow_val, "panels", dir))
  }
  n <- n_rows * n_cols
  layout_cells(
    n, n_rows, n_cols, "h", "fit",
    sprintf("%d panels in a %d \u00d7 %d grid", n, n_rows, n_cols)
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

          # Column metadata for the JS controls, extracted from the
          # upstream ggplot's data (same shape as the ggplot block).
          r_col_meta <- reactive({
            d <- if (inherits(data(), "ggplot")) data()$data else NULL
            if (!is.data.frame(d)) {
              return(list())
            }
            lapply(names(d), function(col) {
              vals <- d[[col]]
              lbl <- attr(vals, "label", exact = TRUE)
              res <- list(
                name = col,
                type = if (is.numeric(vals)) "numeric" else "categorical",
                n_unique = length(unique(vals))
              )
              if (is.character(lbl) && length(lbl) == 1L && nzchar(lbl)) {
                res$label <- lbl
              }
              if (is.factor(vals)) res$levels <- as.list(levels(vals))
              res
            })
          })

          # Length-1-or-empty transport helper (ncol/nrow may be "" or "3").
          s1 <- function(v) if (length(v) == 1) v else ""

          # Push columns + config to JS (single observe; see ggplot-block.R).
          # Multi-column values go through as.list() so a length-1 selection
          # still serializes as a JSON array.

          # The client announces itself when it binds with nothing
          # buffered for it. Shiny DROPS a custom message that has no
          # registered handler, and a dock panel on a view nobody has
          # opened yet has no element to receive one -- so this push
          # can be lost outright, and the controls then render empty, with
          # no columns and no config. Keep the
          # last payload and re-send it when the client says it is here.
          # Layout preview for the gear tray, drawn by gg-blocks.js. None
          # until something is faceted.
          r_preview <- reactive({
            counts <- count_unique_levels()
            if (r_facet_type() == "wrap") {
              if (length(r_facets()) == 0) {
                return(NULL)
              }
              facet_layout_preview(
                "wrap",
                n_levels = counts$facets,
                ncol_val = r_ncol(),
                nrow_val = r_nrow(),
                dir = r_dir()
              )
            } else {
              if (length(r_rows()) == 0 && length(r_cols()) == 0) {
                return(NULL)
              }
              facet_layout_preview(
                "grid",
                n_rows = counts$rows,
                n_cols = counts$cols
              )
            }
          })

          last_push <- new.env(parent = emptyenv())
          last_push$msg <- NULL

          observe({
            last_push$msg <- list(
              id = session$ns("gg_block"),
              block = "facet",
              columns = r_col_meta(),
              preview = r_preview(),
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
            )
            session$sendCustomMessage("gg-block-data", last_push$msg)
          })

          observeEvent(input$gg_block_ready, {
            if (!is.null(last_push$msg)) {
              session$sendCustomMessage("gg-block-data", last_push$msg)
            }
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
      # JS-first UI: the html dependencies and a container; gg-blocks.js
      # builds the face (layout, facet columns) and the gear tray (spec
      # "facet").
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "facet"
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
