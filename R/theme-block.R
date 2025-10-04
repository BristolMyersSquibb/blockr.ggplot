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
    "#FFFAF0"  # Floral White
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
    "#FAFAFA"  # Almost white
  )
}

grid_colors <- function() {
  c(
    "#E5E5E5", # Default light gray
    "#CCCCCC", # Medium gray
    "#999999", # Dark gray
    "#FFFFFF", # White
    "#000000", # Black
    "#D3D3D3"  # Light gray 2
  )
}

#' Theme customization block for ggplot2 plots
#'
#' A block that applies advanced theme customizations to ggplot2 objects.
#' Allows fine-grained control over backgrounds, fonts, grid lines, and more.
#'
#' @param panel_bg Panel background color (default "#FFFFFF" - white)
#' @param plot_bg Plot background color (default "#FFFFFF" - white)
#' @param base_size Base font size in points (default 11)
#' @param base_family Font family: "sans", "serif", or "mono" (default "sans")
#' @param show_major_grid Show major grid lines (default TRUE)
#' @param show_minor_grid Show minor grid lines (default TRUE)
#' @param grid_color Grid line color (default "#E5E5E5")
#' @param show_panel_border Show panel border (default FALSE)
#' @param legend_position Legend position: "right", "left", "top", "bottom", "none" (default "right")
#' @param base_theme Base ggplot2 theme: "minimal", "classic", "gray", "bw" (default "minimal")
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @importFrom colourpicker colourInput
#' @export
new_theme_block <- function(
  panel_bg = "#FFFFFF",
  plot_bg = "#FFFFFF",
  base_size = 11,
  base_family = "sans",
  show_major_grid = TRUE,
  show_minor_grid = TRUE,
  grid_color = "#E5E5E5",
  show_panel_border = FALSE,
  legend_position = "right",
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
          observeEvent(input$panel_bg, r_panel_bg(input$panel_bg))
          observeEvent(input$plot_bg, r_plot_bg(input$plot_bg))
          observeEvent(input$base_size, r_base_size(input$base_size))
          observeEvent(input$base_family, r_base_family(input$base_family))
          observeEvent(input$show_major_grid, r_show_major_grid(input$show_major_grid))
          observeEvent(input$show_minor_grid, r_show_minor_grid(input$show_minor_grid))
          observeEvent(input$grid_color, r_grid_color(input$grid_color))
          observeEvent(input$show_panel_border, r_show_panel_border(input$show_panel_border))
          observeEvent(input$legend_position, r_legend_position(input$legend_position))
          observeEvent(input$base_theme, r_base_theme(input$base_theme))

          list(
            expr = reactive({
              # Build theme customization code
              theme_parts <- c()

              # Panel background
              if (r_panel_bg() != "#FFFFFF") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('panel.background = ggplot2::element_rect(fill = "{r_panel_bg()}")')
                )
              }

              # Plot background
              if (r_plot_bg() != "#FFFFFF") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('plot.background = ggplot2::element_rect(fill = "{r_plot_bg()}")')
                )
              }

              # Base text size (applies to all text elements)
              if (r_base_size() != 11) {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('text = ggplot2::element_text(size = {r_base_size()})')
                )
              }

              # Base font family
              if (r_base_family() != "sans") {
                if (length(theme_parts) > 0 && grepl("^text = ", theme_parts[length(theme_parts)])) {
                  # Update the existing text element
                  theme_parts[length(theme_parts)] <- glue::glue(
                    'text = ggplot2::element_text(size = {r_base_size()}, family = "{r_base_family()}")'
                  )
                } else {
                  theme_parts <- c(
                    theme_parts,
                    glue::glue('text = ggplot2::element_text(family = "{r_base_family()}")')
                  )
                }
              }

              # Grid lines
              if (!r_show_major_grid()) {
                theme_parts <- c(
                  theme_parts,
                  "panel.grid.major = ggplot2::element_blank()"
                )
              } else if (r_grid_color() != "#E5E5E5") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('panel.grid.major = ggplot2::element_line(colour = "{r_grid_color()}")')
                )
              }

              if (!r_show_minor_grid()) {
                theme_parts <- c(
                  theme_parts,
                  "panel.grid.minor = ggplot2::element_blank()"
                )
              } else if (r_grid_color() != "#E5E5E5") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('panel.grid.minor = ggplot2::element_line(colour = "{r_grid_color()}")')
                )
              }

              # Panel border
              if (r_show_panel_border()) {
                theme_parts <- c(
                  theme_parts,
                  'panel.border = ggplot2::element_rect(colour = "grey50", fill = NA)'
                )
              }

              # Legend position
              if (r_legend_position() != "right") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('legend.position = "{r_legend_position()}"')
                )
              }

              # Build the complete expression
              # Start with base theme (overrides ggplot block's theme_minimal if different)
              base_theme_func <- switch(
                r_base_theme(),
                minimal = "ggplot2::theme_minimal()",
                classic = "ggplot2::theme_classic()",
                gray = "ggplot2::theme_gray()",
                bw = "ggplot2::theme_bw()",
                "ggplot2::theme_minimal()"  # fallback
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
      # Helper function for theme color inputs
      make_theme_color_input <- function(id_suffix, label_text, init_value, palette) {
        colourpicker::colourInput(
          inputId = NS(id, id_suffix),
          label = label_text,
          value = init_value,
          showColour = "both",
          palette = "square",
          allowedCols = palette
        )
      }

      tagList(
        shinyjs::useShinyjs(),
        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Set container query context
          block_container_script(),

          div(
            class = "block-form-grid",

            # Section 1: Colors & Backgrounds
            div(
              class = "block-section",
              tags$h4("Colors & Backgrounds"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  make_theme_color_input("panel_bg", "Panel Background", panel_bg, background_light_colors())
                ),
                div(
                  class = "block-input-wrapper",
                  make_theme_color_input("plot_bg", "Plot Background", plot_bg, plot_bg_colors())
                ),
                div(
                  class = "block-input-wrapper",
                  make_theme_color_input("grid_color", "Grid Color", grid_color, grid_colors())
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
                  sliderInput(
                    inputId = NS(id, "base_size"),
                    label = "Base Font Size",
                    min = 8,
                    max = 20,
                    value = base_size,
                    step = 1,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "base_family"),
                    label = "Font Family",
                    choices = c(
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

            # Section 3: Elements
            div(
              class = "block-section",
              tags$h4("Elements"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    inputId = NS(id, "show_major_grid"),
                    label = "Show Major Grid",
                    value = show_major_grid
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    inputId = NS(id, "show_minor_grid"),
                    label = "Show Minor Grid",
                    value = show_minor_grid
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    inputId = NS(id, "show_panel_border"),
                    label = "Show Panel Border",
                    value = show_panel_border
                  )
                )
              )
            ),

            # Section 4: Appearance
            div(
              class = "block-section",
              tags$h4("Appearance"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "legend_position"),
                    label = "Legend Position",
                    choices = c(
                      "Right" = "right",
                      "Left" = "left",
                      "Top" = "top",
                      "Bottom" = "bottom",
                      "None" = "none"
                    ),
                    selected = legend_position,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "base_theme"),
                    label = "Base Theme",
                    choices = c(
                      "Minimal" = "minimal",
                      "Classic" = "classic",
                      "Gray" = "gray",
                      "Black & White" = "bw"
                    ),
                    selected = base_theme,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "theme_block",
    ...
  )
}
