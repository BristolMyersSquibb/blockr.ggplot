# Color palette functions for theme block
background_light_colors <- function() {
  c(
    "#FFFFFF", # White
    "#FFF8DC", # Cornsilk (yellowish)
    "#FFFACD", # Lemon Chiffon
    "#F0F0F0", # Light gray
    "#E8F4F8", # Light blue
    "#F0FFF0", # Honeydew
    "#FFF0F5", # Lavender Blush
    "#FFFAF0" # Floral White
  )
}

plot_bg_colors <- function() {
  c(
    "#FFFFFF", # White
    "#FFF6E8", # Light yellow
    "#FFF8DC", # Cornsilk (yellowish)
    "#FFFACD", # Lemon Chiffon
    "#FFFAF0", # Floral White
    "#F8F8F8", # Very light gray
    "#F0F0F0", # Light gray
    "#FAFAFA" # Almost white
  )
}

grid_colors <- function() {
  c(
    "#E5E5E5", # Default light gray
    "#CCCCCC", # Medium gray
    "#999999", # Dark gray
    "#FFFFFF", # White
    "#000000", # Black
    "#D3D3D3" # Light gray 2
  )
}

#' Theme customization block for ggplot2 plots
#'
#' A block that applies advanced theme customizations to ggplot2 objects.
#' Allows fine-grained control over backgrounds, fonts, grid lines, and more.
#' Empty/NULL values will use the base theme's defaults.
#'
#' @param panel_bg Panel background color (default "" uses base theme default)
#' @param plot_bg Plot background color (default "" uses base theme default)
#' @param base_size Base font size in points
#'   (default NA uses base theme default)
#' @param base_family Font family: "sans", "serif", or "mono"
#'   (default "" uses base theme default)
#' @param show_major_grid Show major grid lines: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param show_minor_grid Show minor grid lines: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param grid_color Grid line color
#'   (default "" uses base theme default)
#' @param show_panel_border Show panel border: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param legend_position Legend position: "", "right", "left", "top",
#'   "bottom", "none" (default "" uses base theme default)
#' @param base_theme Base ggplot2 theme: "minimal", "classic", "gray", "bw"
#'   (default "minimal")
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @importFrom colourpicker colourInput
#' @export
new_theme_block <- function(
  panel_bg = "",
  plot_bg = "",
  base_size = NA_real_,
  base_family = "",
  show_major_grid = "auto",
  show_minor_grid = "auto",
  grid_color = "",
  show_panel_border = "auto",
  legend_position = "",
  base_theme = "minimal",
  ...
) {
  new_ggplot_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_panel_bg <- reactiveVal(panel_bg)
          r_plot_bg <- reactiveVal(plot_bg)
          r_base_size <- reactiveVal(base_size)
          r_base_family <- reactiveVal(base_family)
          r_show_major_grid <- reactiveVal(show_major_grid)
          r_show_minor_grid <- reactiveVal(show_minor_grid)
          r_grid_color <- reactiveVal(grid_color)
          r_show_panel_border <- reactiveVal(show_panel_border)
          r_legend_position <- reactiveVal(legend_position)
          r_base_theme <- reactiveVal(base_theme)

          # Observe input changes
          # (convert "transparent" or "#00000000" to empty string for colors)
          observeEvent(
            input$panel_bg,
            {
              val <- input$panel_bg
              # Treat transparent, #00000000 (transparent black),
              # or empty as "auto"
              r_panel_bg(
                if (val %in% c("transparent", "#00000000", "")) "" else val
              )
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$plot_bg,
            {
              val <- input$plot_bg
              # Treat transparent, #00000000 (transparent black),
              # or empty as "auto"
              r_plot_bg(
                if (val %in% c("transparent", "#00000000", "")) "" else val
              )
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$base_size,
            {
              r_base_size(
                if (input$base_size == "auto") {
                  NA_real_
                } else {
                  as.numeric(input$base_size)
                }
              )
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$base_family,
            {
              r_base_family(input$base_family)
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$show_major_grid,
            {
              r_show_major_grid(input$show_major_grid)
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$show_minor_grid,
            {
              r_show_minor_grid(input$show_minor_grid)
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$grid_color,
            {
              val <- input$grid_color
              # Treat transparent, #00000000 (transparent black),
              # or empty as "auto"
              r_grid_color(
                if (val %in% c("transparent", "#00000000", "")) "" else val
              )
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$show_panel_border,
            {
              r_show_panel_border(input$show_panel_border)
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$legend_position,
            {
              r_legend_position(input$legend_position)
            },
            ignoreNULL = FALSE
          )
          observeEvent(
            input$base_theme,
            {
              r_base_theme(input$base_theme)
            },
            ignoreNULL = FALSE
          )

          list(
            expr = reactive({
              # Build theme customization code
              theme_parts <- c()

              # Panel background (only if explicitly set by user)
              if (r_panel_bg() != "") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue(
                    "panel.background = ",
                    'ggplot2::element_rect(fill = "{r_panel_bg()}")'
                  )
                )
              }

              # Plot background
              if (r_plot_bg() != "") {
                # User explicitly set a color
                theme_parts <- c(
                  theme_parts,
                  glue::glue(
                    "plot.background = ",
                    'ggplot2::element_rect(fill = "{r_plot_bg()}")'
                  )
                )
              } else if (r_base_theme() == "gray") {
                # Gray theme: set plot background to match panel
                # (gray theme doesn't set this itself)
                theme_parts <- c(
                  theme_parts,
                  'plot.background = ggplot2::element_rect(fill = "#EBEBEB")'
                )
              }

              # Build text element modifications
              text_parts <- c()

              # Base text size (only if explicitly set)
              if (!is.na(r_base_size())) {
                text_parts <- c(
                  text_parts,
                  glue::glue("size = {r_base_size()}")
                )
              }

              # Base font family (only if explicitly set)
              if (r_base_family() != "") {
                text_parts <- c(
                  text_parts,
                  glue::glue('family = "{r_base_family()}"')
                )
              }

              # Add combined text element if any parts were specified
              if (length(text_parts) > 0) {
                theme_parts <- c(
                  theme_parts,
                  glue::glue(
                    "text = ggplot2::element_text(",
                    "{paste(text_parts, collapse = \", \")})"
                  )
                )
              }

              # Grid lines
              if (r_show_major_grid() == "hide") {
                # Force hide major grid
                theme_parts <- c(
                  theme_parts,
                  "panel.grid.major = ggplot2::element_blank()"
                )
              } else if (r_show_major_grid() == "show") {
                # Force show major grid
                if (r_grid_color() != "") {
                  # With custom color
                  theme_parts <- c(
                    theme_parts,
                    glue::glue(
                      "panel.grid.major = ggplot2::element_line(",
                      'colour = "{r_grid_color()}")'
                    )
                  )
                } else {
                  # With theme default color
                  theme_parts <- c(
                    theme_parts,
                    "panel.grid.major = ggplot2::element_line()"
                  )
                }
              } else if (
                r_show_major_grid() == "auto" && r_grid_color() != ""
              ) {
                # Auto mode but custom color specified - apply the color
                theme_parts <- c(
                  theme_parts,
                  glue::glue(
                    "panel.grid.major = ggplot2::element_line(",
                    'colour = "{r_grid_color()}")'
                  )
                )
              }
              # auto with no color: don't add anything, use base theme default

              if (r_show_minor_grid() == "hide") {
                # Force hide minor grid
                theme_parts <- c(
                  theme_parts,
                  "panel.grid.minor = ggplot2::element_blank()"
                )
              } else if (r_show_minor_grid() == "show") {
                # Force show minor grid
                if (r_grid_color() != "") {
                  # With custom color
                  theme_parts <- c(
                    theme_parts,
                    glue::glue(
                      "panel.grid.minor = ggplot2::element_line(",
                      'colour = "{r_grid_color()}")'
                    )
                  )
                } else {
                  # With theme default color
                  theme_parts <- c(
                    theme_parts,
                    "panel.grid.minor = ggplot2::element_line()"
                  )
                }
              } else if (
                r_show_minor_grid() == "auto" && r_grid_color() != ""
              ) {
                # Auto mode but custom color specified - apply the color
                theme_parts <- c(
                  theme_parts,
                  glue::glue(
                    "panel.grid.minor = ggplot2::element_line(",
                    'colour = "{r_grid_color()}")'
                  )
                )
              }
              # auto with no color: don't add anything, use base theme default

              # Panel border
              if (r_show_panel_border() == "show") {
                # Force show panel border
                theme_parts <- c(
                  theme_parts,
                  paste0(
                    "panel.border = ggplot2::element_rect(",
                    "colour = \"grey50\", fill = NA)"
                  )
                )
              } else if (r_show_panel_border() == "hide") {
                # Force hide panel border
                theme_parts <- c(
                  theme_parts,
                  "panel.border = ggplot2::element_blank()"
                )
              }
              # auto: don't add anything, use base theme default

              # Legend position (only if explicitly set)
              if (r_legend_position() != "") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('legend.position = "{r_legend_position()}"')
                )
              }

              # Build the complete expression
              # Start with base theme
              # (overrides ggplot block's theme_minimal if different)
              base_theme_func <- switch(
                r_base_theme(),
                # Built-in ggplot2 themes
                minimal = "ggplot2::theme_minimal()",
                classic = "ggplot2::theme_classic()",
                gray = "ggplot2::theme_gray()",
                bw = "ggplot2::theme_bw()",
                light = "ggplot2::theme_light()",
                dark = "ggplot2::theme_dark()",
                void = "ggplot2::theme_void()",
                # cowplot themes
                cowplot = "cowplot::theme_cowplot()",
                minimal_grid = "cowplot::theme_minimal_grid()",
                minimal_hgrid = "cowplot::theme_minimal_hgrid()",
                minimal_vgrid = "cowplot::theme_minimal_vgrid()",
                # ggthemes
                economist = "ggthemes::theme_economist()",
                fivethirtyeight = "ggthemes::theme_fivethirtyeight()",
                tufte = "ggthemes::theme_tufte()",
                wsj = "ggthemes::theme_wsj()",
                # hrbrthemes
                ipsum = "hrbrthemes::theme_ipsum()",
                ipsum_rc = "hrbrthemes::theme_ipsum_rc()",
                ipsum_ps = "hrbrthemes::theme_ipsum_ps()",
                ft_rc = "hrbrthemes::theme_ft_rc()",
                modern_rc = "hrbrthemes::theme_modern_rc()",
                # ggpubr
                pubr = "ggpubr::theme_pubr()",
                pubclean = "ggpubr::theme_pubclean()",
                "ggplot2::theme_minimal()" # fallback
              )

              text <- glue::glue("data + {base_theme_func}")

              # Add custom theme tweaks if any
              if (length(theme_parts) > 0) {
                theme_call <- paste(theme_parts, collapse = ", ")
                text <- glue::glue("({text}) + ggplot2::theme({theme_call})")
              }

              parse(text = text)[[1]]
            }),
            state = list(
              panel_bg = r_panel_bg,
              plot_bg = r_plot_bg,
              base_size = r_base_size,
              base_family = r_base_family,
              show_major_grid = r_show_major_grid,
              show_minor_grid = r_show_minor_grid,
              grid_color = r_grid_color,
              show_panel_border = r_show_panel_border,
              legend_position = r_legend_position,
              base_theme = r_base_theme
            )
          )
        }
      )
    },
    function(id) {
      # Helper function for theme color inputs with sub-label
      make_theme_color_input <- function(
        id_suffix,
        label_text,
        init_value,
        palette,
        sub_label = "Transparent: theme default"
      ) {
        div(
          colourpicker::colourInput(
            inputId = NS(id, id_suffix),
            label = label_text,
            value = if (init_value == "") "transparent" else init_value,
            showColour = "both",
            palette = "square",
            allowedCols = c("transparent", palette),
            allowTransparent = TRUE
          ),
          tags$small(
            class = "text-muted",
            style = "margin-top: -8px; margin-bottom: 8px; display: block;",
            sub_label
          )
        )
      }

      tagList(
        shinyjs::useShinyjs(),

        # CSS for collapsible section
        tags$style(HTML(sprintf(
          "
          #%s-advanced-options {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
            grid-column: 1 / -1;
            display: grid;
            grid-template-columns: subgrid;
            gap: 15px;
          }
          #%s-advanced-options.expanded {
            max-height: 2000px;
            transition: max-height 0.5s ease-in;
          }
          .advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            display: flex;
            align-items: center;
            gap: 10px;
            grid-column: 1 / -1;
            color: #6c757d;
          }
          .advanced-toggle .chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .advanced-toggle .chevron.rotated {
            transform: rotate(90deg);
          }
        ",
          id,
          id
        ))),

        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Set container query context
          block_container_script(),

          div(
            class = "block-form-grid",

            # Main Section: Always Visible
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "base_theme"),
                    label = "Base Theme",
                    choices = list(
                      "ggplot2 (Built-in)" = list(
                        "Minimal" = "minimal",
                        "Classic" = "classic",
                        "Gray" = "gray",
                        "Black & White" = "bw",
                        "Light" = "light",
                        "Dark" = "dark",
                        "Void" = "void"
                      ),
                      "cowplot (Publication)" = list(
                        "Cowplot" = "cowplot",
                        "Minimal Grid" = "minimal_grid",
                        "Minimal H-Grid" = "minimal_hgrid",
                        "Minimal V-Grid" = "minimal_vgrid"
                      ),
                      "ggthemes (Publications)" = list(
                        "The Economist" = "economist",
                        "FiveThirtyEight" = "fivethirtyeight",
                        "Tufte" = "tufte",
                        "Wall Street Journal" = "wsj"
                      ),
                      "hrbrthemes (Typography)" = list(
                        "Ipsum" = "ipsum",
                        "Ipsum RC" = "ipsum_rc",
                        "Ipsum PS" = "ipsum_ps",
                        "Financial Times" = "ft_rc",
                        "Modern RC" = "modern_rc"
                      ),
                      "ggpubr (Scientific)" = list(
                        "Publication Ready" = "pubr",
                        "Publication Clean" = "pubclean"
                      )
                    ),
                    selected = base_theme,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "legend_position"),
                    label = "Legend Position",
                    choices = c(
                      "Auto (theme default)" = "",
                      "Right" = "right",
                      "Left" = "left",
                      "Top" = "top",
                      "Bottom" = "bottom",
                      "None" = "none"
                    ),
                    selected = legend_position,
                    width = "100%"
                  )
                )
              )
            ),

            # Advanced Options Toggle - wrapped in block-section for full width
            div(
              class = "block-section",
              div(
                class = "advanced-toggle text-muted",
                id = NS(id, "advanced-toggle"),
                onclick = sprintf(
                  "
                  const section = document.getElementById('%s');
                  const chevron = document.querySelector('#%s .chevron');
                  section.classList.toggle('expanded');
                  chevron.classList.toggle('rotated');
                ",
                  NS(id, "advanced-options"),
                  NS(id, "advanced-toggle")
                ),
                # Single right-pointing angle quotation mark (prettier chevron)
                tags$span(class = "chevron", "\u203A"),
                "Show advanced options"
              )
            ),

            # Advanced Options Section (Collapsible)
            div(
              id = NS(id, "advanced-options"),

              # Section 1: Colors & Backgrounds
              div(
                class = "block-section",
                tags$h4("Colors & Backgrounds"),
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    make_theme_color_input(
                      "panel_bg",
                      "Panel Background",
                      panel_bg,
                      background_light_colors()
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    make_theme_color_input(
                      "plot_bg",
                      "Plot Background",
                      plot_bg,
                      plot_bg_colors()
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    make_theme_color_input(
                      "grid_color",
                      "Grid Color",
                      grid_color,
                      grid_colors()
                    )
                  )
                )
              ),

              # Section 2: Typography
              div(
                class = "block-section",
                tags$h4("Typography"),
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = NS(id, "base_size"),
                      label = "Base Font Size",
                      choices = c(
                        "Auto (theme default)" = "auto",
                        "8" = "8",
                        "9" = "9",
                        "10" = "10",
                        "11" = "11",
                        "12" = "12",
                        "13" = "13",
                        "14" = "14",
                        "16" = "16",
                        "18" = "18",
                        "20" = "20"
                      ),
                      selected = if (is.na(base_size)) {
                        "auto"
                      } else {
                        as.character(base_size)
                      },
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = NS(id, "base_family"),
                      label = "Font Family",
                      choices = c(
                        "Auto (theme default)" = "",
                        "Sans Serif" = "sans",
                        "Serif" = "serif",
                        "Monospace" = "mono"
                      ),
                      selected = base_family,
                      width = "100%"
                    )
                  )
                )
              ),

              # Section 3: Grid & Borders
              div(
                class = "block-section",
                tags$h4("Grid & Borders"),
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = NS(id, "show_major_grid"),
                      label = "Major Grid Lines",
                      choices = c(
                        "Auto (theme default)" = "auto",
                        "Show" = "show",
                        "Hide" = "hide"
                      ),
                      selected = show_major_grid,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = NS(id, "show_minor_grid"),
                      label = "Minor Grid Lines",
                      choices = c(
                        "Auto (theme default)" = "auto",
                        "Show" = "show",
                        "Hide" = "hide"
                      ),
                      selected = show_minor_grid,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = NS(id, "show_panel_border"),
                      label = "Panel Border",
                      choices = c(
                        "Auto (theme default)" = "auto",
                        "Show" = "show",
                        "Hide" = "hide"
                      ),
                      selected = show_panel_border,
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
    class = "theme_block",
    allow_empty_state = c(
      "panel_bg",
      "plot_bg",
      "base_size",
      "base_family",
      "grid_color",
      "legend_position"
    ),
    ...
  )
}
