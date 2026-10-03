#' Layout of a set of plots or panels, for the preview in the gear tray
#'
#' Uses ggplot2::wrap_dims(), which patchwork and facet_wrap() call, so the
#' preview shows the layout the plot will get.
#'
#' @param n Number of plots (or facet panels).
#' @param ncol_val,nrow_val Columns and rows as chosen ("" for auto).
#' @param what What the cells are, for the status line ("plots", "panels").
#' @param dir Fill order: "h" fills across, "v" fills down.
#' @return A list: `rows`, `cols`, `cells` (one label per slot, row by row,
#'   "" for an empty slot; empty when there are too many slots to draw),
#'   `state` ("fit", "gaps" or "invalid") and the `status` line.
#' @noRd
layout_preview <- function(n, ncol_val = "", nrow_val = "", what = "plots",
                           dir = "h") {
  num <- function(v) {
    if (length(v) == 1L && nzchar(v)) as.numeric(v) else NULL
  }
  nc <- num(ncol_val)
  nr <- num(nrow_val)

  # wrap_dims() throws when rows * columns cannot hold n.
  dims <- tryCatch(
    ggplot2::wrap_dims(n, nrow = nr, ncol = nc),
    error = function(e) NULL
  )

  if (is.null(dims)) {
    rows <- if (is.null(nr)) 1 else nr
    cols <- if (is.null(nc)) 1 else nc
    state <- "invalid"
    status <- sprintf(
      "%d %s need %d slots, this layout has %d. Add columns or rows.",
      n, what, n, rows * cols
    )
  } else {
    rows <- dims[1]
    cols <- dims[2]
    empty <- rows * cols - n
    state <- if (empty == 0) "fit" else "gaps"
    status <- sprintf("%d %s in a %d \u00d7 %d grid", n, what, rows, cols)
    if (empty > 0) {
      status <- paste0(status, sprintf(", %d empty", empty))
    }
  }

  layout_cells(n, rows, cols, dir, state, status)
}

#' @noRd
layout_cells <- function(n, rows, cols, dir, state, status) {
  slots <- rows * cols
  cells <- character()
  # Past 60 slots the cells say nothing the status line does not.
  if (slots <= 60) {
    k <- seq_len(slots)
    r <- (k - 1) %/% cols
    c <- (k - 1) %% cols
    idx <- if (identical(dir, "v")) c * rows + r + 1 else k
    cells <- ifelse(idx <= n, as.character(idx), "")
  }
  list(
    rows = rows,
    cols = cols,
    cells = as.list(cells),
    state = state,
    status = status
  )
}

# Variadic `...args` helpers under blockr.core's name-or-position convention
# (core #251): unnamed slots are referenced as .arg1, .arg2, ... in the eval
# environment, named slots by their link name, and slot values are accessed
# through `.()` calls. Copied from blockr.core (not exported):
# dot_sym/arg_refs/dot_arg_refs from R/utils-misc.R, as_dot_call from
# R/utils-expr.R — keep in sync. These work on the `reactive_exprs`
# collection from the reactives package that a variadic block server receives.
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

          # Reactive values for the JS controls. Character() constructor
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

          # The client announces itself when it binds with nothing
          # buffered for it. Shiny DROPS a custom message that has no
          # registered handler, and a dock panel on a view nobody has
          # opened yet has no element to receive one -- so this push
          # can be lost outright, and the controls then render empty, with
          # no columns and no config. Keep the
          # last payload and re-send it when the client says it is here.
          last_push <- new.env(parent = emptyenv())
          last_push$msg <- NULL

          # Layout preview for the gear tray, drawn by gg-blocks.js.
          r_preview <- reactive({
            n_plots <- length(arg_names())
            if (n_plots == 0) {
              return(NULL)
            }
            layout_preview(n_plots, r_ncol(), r_nrow(), what = "plots")
          })

          observe({
            last_push$msg <- list(
              id = session$ns("gg_block"),
              block = "grid",
              columns = list(),
              preview = r_preview(),
              config = list(
                ncol = r_ncol(),
                nrow = r_nrow(),
                guides = r_guides(),
                title = r_title(),
                subtitle = r_subtitle(),
                caption = r_caption(),
                tag_levels = r_tag_levels()
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
      # JS-first UI: the html dependencies and a container; gg-blocks.js
      # builds the gear and its tray (spec "grid"). The face is the plot.
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "grid"
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
