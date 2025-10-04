#' Plot Grid Block
#'
#' Combines multiple ggplot objects using patchwork::wrap_plots().
#' Variadic block that accepts 2 or more ggplot inputs with automatic alignment.
#' Supports layout control (ncol, nrow) and annotations (title, subtitle, auto-tags).
#'
#' @param ncol Number of columns in grid layout (default: NULL for auto)
#' @param nrow Number of rows in grid layout (default: NULL for auto)
#' @param title Overall plot title (default: "")
#' @param subtitle Overall plot subtitle (default: "")
#' @param caption Overall plot caption (default: "")
#' @param tag_levels Auto-tagging style: 'A', 'a', '1', 'I', 'i', or NULL (default: NULL)
#' @param guides Legend handling: 'auto', 'collect', or 'keep' (default: 'auto')
#' @param ... Forwarded to [new_ggplot_transform_block()]
#' @export
new_plot_grid_block <- function(
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
            set_names(names(...args), blockr.core:::dot_args_names(...args))
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
              if (r_title() != "") annot_args$title <- r_title()
              if (r_subtitle() != "") annot_args$subtitle <- r_subtitle()
              if (r_caption() != "") annot_args$caption <- r_caption()
              if (r_tag_levels() != "") annot_args$tag_levels <- r_tag_levels()

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

          # Set container query context
          block_container_script(),

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
                    choices = c("Auto" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"),
                    selected = ncol,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "nrow"),
                    "Rows",
                    choices = c("Auto" = "", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"),
                    selected = nrow,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "guides"),
                    "Legends",
                    choices = c("Auto" = "auto", "Collect" = "collect", "Keep separate" = "keep"),
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
                  textInput(NS(id, "title"), "Title", value = title, width = "100%")
                ),
                div(
                  class = "block-input-wrapper",
                  textInput(NS(id, "subtitle"), "Subtitle", value = subtitle, width = "100%")
                ),
                div(
                  class = "block-input-wrapper",
                  textInput(NS(id, "caption"), "Caption", value = caption, width = "100%")
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
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 2L)
    },
    allow_empty_state = TRUE,
    class = "plot_grid_block",
    ...
  )
}
