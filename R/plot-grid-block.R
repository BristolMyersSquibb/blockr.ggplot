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

# Helper function to extract argument names for variadic blocks
# Copied from blockr.core:::dot_args_names (not exported)
dot_args_names <- function(x) {
  res <- names(x)
  unnamed <- grepl("^[1-9][0-9]*$", res)

  if (all(unnamed)) {
    return(NULL)
  }

  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }

  res
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
          arg_names <- reactive(
            set_names(names(...args), dot_args_names(...args))
          )

          # Reactive values for UI inputs
          r_ncol <- reactiveVal(ncol)
          r_nrow <- reactiveVal(nrow)
          r_title <- reactiveVal(title)
          r_subtitle <- reactiveVal(subtitle)
          r_caption <- reactiveVal(caption)
          r_tag_levels <- reactiveVal(tag_levels)
          r_guides <- reactiveVal(guides)

          # Update reactive values from inputs
          observeEvent(input$ncol, r_ncol(input$ncol))
          observeEvent(input$nrow, r_nrow(input$nrow))
          observeEvent(input$title, r_title(input$title))
          observeEvent(input$subtitle, r_subtitle(input$subtitle))
          observeEvent(input$caption, r_caption(input$caption))
          observeEvent(input$tag_levels, r_tag_levels(input$tag_levels))
          observeEvent(input$guides, r_guides(input$guides))

          # Layout preview output
          output$layout_preview <- renderUI({
            args_list <- shiny::reactiveValuesToList(...args)
            n_plots <- sum(!sapply(args_list, is.null))
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
              # Base wrap_plots expression
              base_expr <- bquote(
                patchwork::wrap_plots(..(dat)),
                list(dat = lapply(arg_names(), as.name)),
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
      tagList(
        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Two-column responsive CSS
          tags$style(HTML("
            /* Responsive layout for inputs and preview */
            @media (min-width: 700px) {
              .grid-layout-wrapper {
                display: grid;
                grid-template-columns: 2fr 1fr;
                gap: 20px;
                align-items: start;
              }
              .grid-preview-sidebar {
                position: sticky;
                top: 20px;
              }
            }

            @media (max-width: 699px) {
              .grid-layout-wrapper {
                display: block;
              }
              .grid-preview-sidebar {
                margin-top: 20px;
              }
            }

            /* Subtle preview status */
            .preview-svg-container {
              text-align: center;
              margin-bottom: 8px;
            }

            .preview-status {
              font-size: 0.875rem;
              color: #6c757d;
              text-align: center;
              padding: 6px 8px;
              border-radius: 4px;
              background-color: #f8f9fa;
            }

            .preview-status.valid {
              color: #28a745;
            }

            .preview-status.warning {
              color: #ffc107;
            }

            .preview-status.error {
              color: #dc3545;
            }
          ")),

          # Set container query context
          block_container_script(),

          # Two-column wrapper
          div(
            class = "grid-layout-wrapper",

            # Left: Inputs (2/3 width)
            div(
              class = "grid-inputs",
              div(
                class = "block-form-grid",

            # Layout Section
            div(
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
                    NS(id, "guides"),
                    "Legends",
                    choices = c(
                      "Auto" = "auto",
                      "Collect" = "collect",
                      "Keep separate" = "keep"
                    ),
                    selected = guides,
                    width = "100%"
                  )
                )
              )
            ),

            # Annotation Section
            div(
              class = "block-section",
              tags$h4("Annotation"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "title"),
                    "Title",
                    value = title,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "subtitle"),
                    "Subtitle",
                    value = subtitle,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  textInput(
                    NS(id, "caption"),
                    "Caption",
                    value = caption,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "tag_levels"),
                    "Auto-tag plots",
                    choices = c(
                      "None" = "",
                      "A, B, C..." = "A",
                      "a, b, c..." = "a",
                      "1, 2, 3..." = "1",
                      "I, II, III..." = "I",
                      "i, ii, iii..." = "i"
                    ),
                    selected = tag_levels,
                    width = "100%"
                  )
                )
              )
            )
          )
          ), # Close grid-inputs div (and block-form-grid div)

          # Right: Preview sidebar (1/3 width)
          div(
            class = "grid-preview-sidebar",
            uiOutput(NS(id, "layout_preview"))
          )
        ) # Close grid-layout-wrapper div
      ) # Close block-container div
    ) # Close tagList
  },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("grid_block", "rbind_block"),
    ...
  )
}
