#' Create SVG preview of plot grid layout
#'
#' @param n_plots Number of plots to arrange
#' @param ncol_val Number of columns ("" for auto)
#' @param nrow_val Number of rows ("" for auto)
#' @return List with svg, status text, and status type
#' @noRd
create_grid_preview_svg <- function(n_plots, ncol_val, nrow_val) {
  # Calculate actual grid dimensions using exact patchwork algorithm
  # (via ggplot2::wrap_dims which patchwork uses internally)
  # wrap_dims throws an error if nrow * ncol < n_plots, so catch that
  result <- tryCatch(
    {
      if (ncol_val == "" && nrow_val == "") {
        # Full auto mode - use wrap_dims with both NULL
        dims <- ggplot2::wrap_dims(n_plots, nrow = NULL, ncol = NULL)
      } else if (nrow_val != "" && ncol_val != "") {
        # Both specified - validate with wrap_dims
        dims <- ggplot2::wrap_dims(
          n_plots,
          nrow = as.numeric(nrow_val),
          ncol = as.numeric(ncol_val)
        )
      } else if (ncol_val != "") {
        # Only ncol specified - let wrap_dims calculate nrow
        dims <- ggplot2::wrap_dims(
          n_plots,
          nrow = NULL,
          ncol = as.numeric(ncol_val)
        )
      } else {
        # Only nrow specified - let wrap_dims calculate ncol
        dims <- ggplot2::wrap_dims(
          n_plots,
          nrow = as.numeric(nrow_val),
          ncol = NULL
        )
      }
      list(dims = dims, is_valid = TRUE, error_msg = NULL)
    },
    error = function(e) {
      # If wrap_dims fails, calculate what it would be without validation
      if (nrow_val != "" && ncol_val != "") {
        dims <- c(as.numeric(nrow_val), as.numeric(ncol_val))
      } else {
        # Fallback dimensions for error display
        dims <- c(1, 1)
      }
      list(dims = dims, is_valid = FALSE, error_msg = conditionMessage(e))
    }
  )

  nrow_actual <- result$dims[1]
  ncol_actual <- result$dims[2]
  is_valid <- result$is_valid

  # Check if configuration is valid
  total_slots <- nrow_actual * ncol_actual

  # SVG dimensions - scale to fit the actual grid
  max_width <- 300
  gap <- 4
  # Calculate cell size based on actual columns (not a minimum)
  cell_size <- max_width / ncol_actual
  # But ensure cells aren't too small or too large
  cell_size <- max(50, min(cell_size, 100))
  preview_width <- cell_size * ncol_actual
  preview_height <- cell_size * nrow_actual

  # Status based on validation
  if (!is_valid) {
    fill_color <- "rgba(244, 67, 54, 0.3)"
    stroke_color <- "#f44336"
    status_icon <- "\u274c" # Red X
    status_text <- sprintf(
      "Need %d slots but only have %d (increase ncol and/or nrow)",
      n_plots,
      total_slots
    )
  } else if (total_slots == n_plots) {
    fill_color <- "rgba(76, 175, 80, 0.3)"
    stroke_color <- "#4CAF50"
    status_icon <- "\u2713" # Check mark
    status_text <- sprintf(
      "Perfect fit: %d plots in %dx%d grid",
      n_plots,
      nrow_actual,
      ncol_actual
    )
  } else {
    fill_color <- "rgba(33, 150, 243, 0.3)"
    stroke_color <- "#2196F3"
    status_icon <- "\u2713" # Check mark
    empty_slots <- total_slots - n_plots
    status_text <- sprintf(
      "%d plots in %dx%d grid (%d empty slot%s)",
      n_plots,
      nrow_actual,
      ncol_actual,
      empty_slots,
      if (empty_slots > 1) "s" else ""
    )
  }

  # Create cells
  cells <- list()
  plot_idx <- 1
  for (row in 0:(nrow_actual - 1)) {
    for (col in 0:(ncol_actual - 1)) {
      x <- col * cell_size + gap
      y <- row * cell_size + gap
      w <- cell_size - 2 * gap
      h <- cell_size - 2 * gap

      is_filled <- plot_idx <= n_plots

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
            max(10, cell_size / 5),
            stroke_color
          ),
          as.character(plot_idx)
        )
      }

      plot_idx <- plot_idx + 1
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

# Variadic `...args` helpers under blockr.core's name-or-position convention
# (core #251): unnamed slots are referenced as .arg1, .arg2, ... in the eval
# environment, named slots by their link name, and slot values are accessed
# through `.()` calls. Copied from blockr.core (not exported):
# dot_sym/arg_refs/dot_arg_refs from R/utils-misc.R, as_dot_call from
# R/utils-expr.R — keep in sync. `names(...args)` dispatches on core's
# exported names.reactives method, so these work on the `reactives` object
# a variadic block server receives.
dot_sym <- function(i) {
  paste0(".arg", i)
}

arg_refs <- function(nms) {
  unnamed <- !nzchar(nms)
  replace(nms, unnamed, dot_sym(seq_len(sum(unnamed))))
}

dot_arg_refs <- function(x) {
  nms <- names(x)

  if (is.null(nms)) {
    nms <- character(length(x))
  }

  set_names(arg_refs(nms), nms)
}

as_dot_call <- function(x) {
  call(".", as.name(x))
}

#' Grid Block
#'
#' Combines multiple ggplot objects using patchwork::wrap_plots().
#' Variadic block that accepts 1 or more ggplot inputs with automatic
#' alignment. Supports layout control (ncol, nrow) and annotations
#' (title, subtitle, auto-tags).
#'
#' @param ncol Number of columns in grid layout (default: NULL for auto)
#' @param nrow Number of rows in grid layout (default: NULL for auto)
#' @param title Overall plot title (default: "")
#' @param subtitle Overall plot subtitle (default: "")
#' @param caption Overall plot caption (default: "")
#' @param tag_levels Auto-tagging style: 'A', 'a', '1', 'I', 'i', or NULL
#'   (default: NULL)
#' @param guides Legend handling: 'auto', 'collect', or 'keep'
#'   (default: 'auto')
#' @param ... Forwarded to [new_ggplot_transform_block()]
#'
#' @return A ggplot transform block object of class `grid_block`.
#'
#' @examples
#' # Create a grid block with 2 columns
#' new_grid_block(ncol = "2")
#'
#' # Create a grid block with title
#' new_grid_block(title = "My Combined Plots", ncol = "2")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   # Grid block requires multiple ggplot inputs
#'   serve(new_grid_block())
#' }
#'
#' @export
new_grid_block <- function(
  ncol = character(),
  nrow = character(),
  title = character(),
  subtitle = character(),
  caption = character(),
  tag_levels = character(),
  guides = "auto",
  ...
) {
  new_ggplot_transform_block(
    function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          # Eval-env references for the connected inputs (named slots by
          # link name, unnamed as .argN); reactive on the link set.
          arg_names <- reactive(
            dot_arg_refs(...args)
          )

          # Reactive values for the settings band. Character() constructor
          # defaults normalize to "" so the expr reactive's `!= ""` checks
          # are length-safe before the first config echo.
          chr1 <- function(v) if (length(v)) v else ""
          r_ncol <- reactiveVal(chr1(ncol))
          r_nrow <- reactiveVal(chr1(nrow))
          r_title <- reactiveVal(chr1(title))
          r_subtitle <- reactiveVal(chr1(subtitle))
          r_caption <- reactiveVal(chr1(caption))
          r_tag_levels <- reactiveVal(chr1(tag_levels))
          r_guides <- reactiveVal(guides)

          # Push config to JS (single observe; see ggplot-block.R). The grid
          # block combines upstream plots, so no column metadata is sent.
          observe({
            session$sendCustomMessage("gg-block-data", list(
              id = session$ns("gg_block"),
              block = "grid",
              columns = list(),
              config = list(
                ncol = r_ncol(),
                nrow = r_nrow(),
                guides = r_guides(),
                title = r_title(),
                subtitle = r_subtitle(),
                caption = r_caption(),
                tag_levels = r_tag_levels()
              )
            ))
          })

          # JS -> R: full-config echo through one action input, with the
          # identical() guard against R->JS->R loops (see ggplot-block.R).
          upd <- function(rv, v) {
            if (!identical(isolate(rv()), v)) rv(v)
          }

          observeEvent(input$gg_block_action, {
            msg <- input$gg_block_action
            if (!identical(msg$action, "config")) {
              return()
            }
            if (!is.null(msg$ncol)) upd(r_ncol, msg$ncol)
            if (!is.null(msg$nrow)) upd(r_nrow, msg$nrow)
            if (!is.null(msg$guides)) upd(r_guides, msg$guides)
            if (!is.null(msg$title)) upd(r_title, msg$title)
            if (!is.null(msg$subtitle)) upd(r_subtitle, msg$subtitle)
            if (!is.null(msg$caption)) upd(r_caption, msg$caption)
            if (!is.null(msg$tag_levels)) upd(r_tag_levels, msg$tag_levels)
          })


          # Layout preview output
          output$layout_preview <- renderUI({
            n_plots <- length(arg_names())
            ncol_val <- r_ncol()
            nrow_val <- r_nrow()

            # Handle case where no plots are connected yet
            if (n_plots == 0) {
              return(tags$div(
                style = paste(
                  "padding: 10px; background: #fff3cd;",
                  "border-radius: 4px; margin-bottom: 15px;"
                ),
                tags$strong("\u26a0\ufe0f Waiting for input plots"),
                tags$br(),
                "Connect 1 or more ggplot blocks to create a grid"
              ))
            }

            # Generate preview
            preview <- create_grid_preview_svg(n_plots, ncol_val, nrow_val)

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
              # Base wrap_plots expression over all connected inputs,
              # referenced via `.()` calls (see blockr.core's rbind_block).
              # Readiness gating happens upstream (inputs_ready), so no
              # NULL-filtering is needed anymore.
              base_expr <- bquote(
                patchwork::wrap_plots(..(dat)),
                list(dat = lapply(arg_names(), as_dot_call)),
                splice = TRUE
              )

              # Build plot_layout() arguments
              layout_args <- list()
              if (r_ncol() != "" && !is.na(as.numeric(r_ncol()))) {
                layout_args$ncol <- as.numeric(r_ncol())
              }
              if (r_nrow() != "" && !is.na(as.numeric(r_nrow()))) {
                layout_args$nrow <- as.numeric(r_nrow())
              }
              if (r_guides() != "auto") {
                layout_args$guides <- r_guides()
              }

              # Build plot_annotation() arguments
              annot_args <- list()
              if (r_title() != "") {
                annot_args$title <- r_title()
              }
              if (r_subtitle() != "") {
                annot_args$subtitle <- r_subtitle()
              }
              if (r_caption() != "") {
                annot_args$caption <- r_caption()
              }
              if (r_tag_levels() != "") {
                annot_args$tag_levels <- r_tag_levels()
              }

              # Add plot_layout() if needed
              if (length(layout_args) > 0) {
                base_expr <- call(
                  "+",
                  base_expr,
                  as.call(c(quote(patchwork::plot_layout), layout_args))
                )
              }

              # Add plot_annotation() if needed
              if (length(annot_args) > 0) {
                base_expr <- call(
                  "+",
                  base_expr,
                  as.call(c(quote(patchwork::plot_annotation), annot_args))
                )
              }

              base_expr
            }),
            state = list(
              ncol = r_ncol,
              nrow = r_nrow,
              title = r_title,
              subtitle = r_subtitle,
              caption = r_caption,
              tag_levels = r_tag_levels,
              guides = r_guides
            )
          )
        }
      )
    },
    ui = function(id) {
      # JS-first UI (settings-band pattern, see ggplot-block.R): the html
      # dependencies plus a container; inst/js/gg-blocks.js builds the gear
      # header and settings band (spec "grid") ABOVE the existing children,
      # so the SVG layout preview below survives band re-renders. The
      # preview shows only while the band is open (gg-blocks.css).
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "grid",
          div(
            class = "gg-preview",
            uiOutput(NS(id, "layout_preview"))
          )
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("grid_block", "rbind_block"),
    external_ctrl = TRUE,
    # The expr references variadic inputs via `.()` calls (see
    # blockr.core's rbind_block); "bquoted" makes core resolve them
    # against the eval environment.
    expr_type = "bquoted",
    ...
  )
}
