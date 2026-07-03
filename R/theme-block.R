#' Build theme choices list based on available packages
#'
#' @return A named list of theme choices for selectInput
#' @keywords internal
build_theme_choices <- function() {
  # Start with base choices (always available)
  choices <- list(
    "Auto (keep upstream)" = "auto",
    "ggplot2 (Built-in)" = list(
      "Minimal" = "minimal",
      "Classic" = "classic",
      "Gray" = "gray",
      "Black & White" = "bw",
      "Light" = "light",
      "Dark" = "dark",
      "Void" = "void"
    )
  )

  # Add cowplot themes if available
  if (requireNamespace("cowplot", quietly = TRUE)) {
    choices[["cowplot (Publication)"]] <- list(
      "Cowplot" = "cowplot",
      "Minimal Grid" = "minimal_grid",
      "Minimal H-Grid" = "minimal_hgrid",
      "Minimal V-Grid" = "minimal_vgrid"
    )
  }

  # Add ggthemes if available
  if (requireNamespace("ggthemes", quietly = TRUE)) {
    choices[["ggthemes (Publications)"]] <- list(
      "The Economist" = "economist",
      "FiveThirtyEight" = "fivethirtyeight",
      "Tufte" = "tufte",
      "Wall Street Journal" = "wsj"
    )
  }

  # Add ggpubr themes if available
  if (requireNamespace("ggpubr", quietly = TRUE)) {
    choices[["ggpubr (Scientific)"]] <- list(
      "Publication Ready" = "pubr",
      "Publication Clean" = "pubclean"
    )
  }

  choices
}

#' Get theme function call for a given theme name
#'
#' Maps theme names to their corresponding ggplot2 function calls.
#' Returns a fallback theme if the requested theme's package is not available.
#'
#' @param theme_name Character string naming the theme
#' @return Character string with the theme function call
#' @keywords internal
get_theme_function <- function(theme_name) {
  # Handle auto mode
  if (theme_name == "auto") {
    return("")
  }

  # Built-in ggplot2 themes (always available)
  builtin_themes <- c(
    minimal = "ggplot2::theme_minimal()",
    classic = "ggplot2::theme_classic()",
    gray = "ggplot2::theme_gray()",
    bw = "ggplot2::theme_bw()",
    light = "ggplot2::theme_light()",
    dark = "ggplot2::theme_dark()",
    void = "ggplot2::theme_void()"
  )

  if (theme_name %in% names(builtin_themes)) {
    return(builtin_themes[[theme_name]])
  }

  # cowplot themes (check availability)
  cowplot_themes <- c(
    cowplot = "cowplot::theme_cowplot()",
    minimal_grid = "cowplot::theme_minimal_grid()",
    minimal_hgrid = "cowplot::theme_minimal_hgrid()",
    minimal_vgrid = "cowplot::theme_minimal_vgrid()"
  )

  if (theme_name %in% names(cowplot_themes)) {
    if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot_themes[[theme_name]])
    } else {
      # Fallback to minimal if cowplot not available
      return("ggplot2::theme_minimal()")
    }
  }

  # ggthemes (check availability)
  ggthemes_themes <- c(
    economist = "ggthemes::theme_economist()",
    fivethirtyeight = "ggthemes::theme_fivethirtyeight()",
    tufte = "ggthemes::theme_tufte()",
    wsj = "ggthemes::theme_wsj()"
  )

  if (theme_name %in% names(ggthemes_themes)) {
    if (requireNamespace("ggthemes", quietly = TRUE)) {
      return(ggthemes_themes[[theme_name]])
    } else {
      # Fallback to minimal if ggthemes not available
      return("ggplot2::theme_minimal()")
    }
  }

  # ggpubr themes (check availability)
  ggpubr_themes <- c(
    pubr = "ggpubr::theme_pubr()",
    pubclean = "ggpubr::theme_pubclean()"
  )

  if (theme_name %in% names(ggpubr_themes)) {
    if (requireNamespace("ggpubr", quietly = TRUE)) {
      return(ggpubr_themes[[theme_name]])
    } else {
      # Fallback to minimal if ggpubr not available
      return("ggplot2::theme_minimal()")
    }
  }

  # Default fallback for unknown themes
  "ggplot2::theme_minimal()"
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
#'   (default "auto" uses base theme default)
#' @param base_family Font family: "auto", "sans", "serif", or "mono"
#'   (default "auto" preserves upstream font)
#' @param show_major_grid Show major grid lines: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param show_minor_grid Show minor grid lines: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param grid_color Grid line color
#'   (default "" uses base theme default)
#' @param show_panel_border Show panel border: "auto", "show", "hide"
#'   (default "auto" uses base theme default)
#' @param legend_position Legend position: "auto", "right", "left", "top",
#'   "bottom", "none" (default "auto" preserves upstream position)
#' @param base_theme Base ggplot2 theme: "auto", "minimal", "classic",
#'   "gray", "bw", etc. (default "auto" preserves upstream theme)
#' @param palette_fill Color palette for fill aesthetic: "auto" (keep upstream),
#'   "viridis", "magma", "plasma", "inferno", "cividis", or "ggplot2"
#'   (default "auto" preserves upstream palette)
#' @param palette_colour Color palette for colour aesthetic: "auto" (keep
#'   upstream), "viridis", "magma", "plasma", "inferno", "cividis", or "ggplot2"
#'   (default "auto" preserves upstream palette)
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A ggplot transform block object of class `theme_block`.
#'
#' @examples
#' # Create a theme block with classic theme
#' new_theme_block(base_theme = "classic")
#'
#' # Create a theme block with custom settings
#' new_theme_block(
#'   base_theme = "minimal",
#'   legend_position = "bottom",
#'   base_size = 14
#' )
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   # Theme block requires a ggplot input
#'   serve(new_theme_block())
#' }
#'
#' @export
new_theme_block <- function(
  panel_bg = "",
  plot_bg = "",
  base_size = "auto",
  base_family = "auto",
  show_major_grid = "auto",
  show_minor_grid = "auto",
  grid_color = "",
  show_panel_border = "auto",
  legend_position = "auto",
  base_theme = "auto",
  palette_fill = "auto",
  palette_colour = "auto",
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
          r_palette_fill <- reactiveVal(palette_fill)
          r_palette_colour <- reactiveVal(palette_colour)

          # Flattened base_theme options for the JS settings band: the
          # grouped build_theme_choices() list is runtime-dependent
          # (installed theme packages), so it travels with the push
          # message as {value, label} pairs.
          theme_choice_options <- function() {
            choices <- build_theme_choices()
            out <- list()
            for (i in seq_along(choices)) {
              nm <- names(choices)[i]
              entry <- choices[[i]]
              if (is.list(entry)) {
                grp <- sub(" \\(.*$", "", nm)
                for (j in seq_along(entry)) {
                  out[[length(out) + 1]] <- list(
                    value = entry[[j]],
                    label = paste0(names(entry)[j], " (", grp, ")")
                  )
                }
              } else {
                out[[length(out) + 1]] <- list(value = entry, label = nm)
              }
            }
            out
          }

          # Push config to JS (single observe; see ggplot-block.R). The
          # theme block has a ggplot input, so no column metadata is sent.
          # base_size travels as character ("auto" or the number).
          observe({
            session$sendCustomMessage("gg-block-data", list(
              id = session$ns("gg_block"),
              block = "theme",
              columns = list(),
              choices = list(base_theme = theme_choice_options()),
              config = list(
                base_theme = r_base_theme(),
                legend_position = r_legend_position(),
                palette_fill = r_palette_fill(),
                palette_colour = r_palette_colour(),
                panel_bg = r_panel_bg(),
                plot_bg = r_plot_bg(),
                base_size = as.character(r_base_size()),
                base_family = r_base_family(),
                show_major_grid = r_show_major_grid(),
                show_minor_grid = r_show_minor_grid(),
                grid_color = r_grid_color(),
                show_panel_border = r_show_panel_border()
              )
            ))
          })

          # JS -> R: full-config echo through one action input, with the
          # identical() guard against R->JS->R loops (see ggplot-block.R).
          # Color values arrive as "" (theme default) or a hex string — no
          # "transparent" normalization needed anymore.
          upd <- function(rv, v) {
            if (!identical(isolate(rv()), v)) rv(v)
          }

          observeEvent(input$gg_block_action, {
            msg <- input$gg_block_action
            if (!identical(msg$action, "config")) {
              return()
            }
            if (!is.null(msg$base_theme)) upd(r_base_theme, msg$base_theme)
            if (!is.null(msg$legend_position)) {
              upd(r_legend_position, msg$legend_position)
            }
            if (!is.null(msg$palette_fill)) {
              upd(r_palette_fill, msg$palette_fill)
            }
            if (!is.null(msg$palette_colour)) {
              upd(r_palette_colour, msg$palette_colour)
            }
            if (!is.null(msg$panel_bg)) upd(r_panel_bg, msg$panel_bg)
            if (!is.null(msg$plot_bg)) upd(r_plot_bg, msg$plot_bg)
            if (!is.null(msg$base_size)) {
              upd(r_base_size, if (identical(msg$base_size, "auto")) {
                "auto"
              } else {
                as.numeric(msg$base_size)
              })
            }
            if (!is.null(msg$base_family)) {
              upd(r_base_family, msg$base_family)
            }
            if (!is.null(msg$show_major_grid)) {
              upd(r_show_major_grid, msg$show_major_grid)
            }
            if (!is.null(msg$show_minor_grid)) {
              upd(r_show_minor_grid, msg$show_minor_grid)
            }
            if (!is.null(msg$grid_color)) upd(r_grid_color, msg$grid_color)
            if (!is.null(msg$show_panel_border)) {
              upd(r_show_panel_border, msg$show_panel_border)
            }
          })


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
              if (r_base_size() != "auto") {
                text_parts <- c(
                  text_parts,
                  glue::glue("size = {r_base_size()}")
                )
              }

              # Base font family (only if explicitly set)
              if (r_base_family() != "" && r_base_family() != "auto") {
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
              if (r_legend_position() != "" && r_legend_position() != "auto") {
                theme_parts <- c(
                  theme_parts,
                  glue::glue('legend.position = "{r_legend_position()}"')
                )
              }

              # Build the complete expression
              # Start with base theme (unless "auto" - keep upstream)
              base_theme_func <- get_theme_function(r_base_theme())

              # Start with data, optionally add base theme
              if (base_theme_func == "") {
                # Auto mode: don't apply base theme, just pass data through
                text <- "(data)"
              } else {
                # Apply selected base theme
                text <- glue::glue("data + {base_theme_func}")
              }

              # Add custom theme tweaks if any
              if (length(theme_parts) > 0) {
                theme_call <- paste(theme_parts, collapse = ", ")
                text <- glue::glue("({text}) + ggplot2::theme({theme_call})")
              }

              # Add color palette overrides
              # Values are like "viridis_d", "viridis_c", "magma_d", etc.
              if (r_palette_fill() != "auto") {
                if (r_palette_fill() == "ggplot2") {
                  # Reset to ggplot2 default
                  text <- glue::glue(
                    "({text}) + ggplot2::scale_fill_discrete()"
                  )
                } else {
                  # Parse option name and type (viridis_d -> viridis, d)
                  parts <- strsplit(r_palette_fill(), "_")[[1]]
                  option_name <- parts[1]
                  scale_type <- parts[2]
                  if (scale_type == "c") {
                    text <- glue::glue(
                      "({text}) + ggplot2::scale_fill_viridis_c(",
                      "option = \"{option_name}\")"
                    )
                  } else {
                    text <- glue::glue(
                      "({text}) + ggplot2::scale_fill_viridis_d(",
                      "option = \"{option_name}\")"
                    )
                  }
                }
              }

              if (r_palette_colour() != "auto") {
                if (r_palette_colour() == "ggplot2") {
                  # Reset to ggplot2 default
                  text <- glue::glue(
                    "({text}) + ggplot2::scale_colour_discrete()"
                  )
                } else {
                  # Parse option name and type (viridis_d -> viridis, d)
                  parts <- strsplit(r_palette_colour(), "_")[[1]]
                  option_name <- parts[1]
                  scale_type <- parts[2]
                  if (scale_type == "c") {
                    text <- glue::glue(
                      "({text}) + ggplot2::scale_colour_viridis_c(",
                      "option = \"{option_name}\")"
                    )
                  } else {
                    text <- glue::glue(
                      "({text}) + ggplot2::scale_colour_viridis_d(",
                      "option = \"{option_name}\")"
                    )
                  }
                }
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
              base_theme = r_base_theme,
              palette_fill = r_palette_fill,
              palette_colour = r_palette_colour
            )
          )
        }
      )
    },
    function(id) {
      # JS-first UI (settings-band pattern, see ggplot-block.R): only the
      # html dependencies plus an empty container; inst/js/gg-blocks.js
      # builds the gear header and settings band (spec "theme").
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "theme"
        )
      )
    },
    class = "theme_block",
    external_ctrl = TRUE,
    allow_empty_state = c(
      "panel_bg",
      "plot_bg",
      "base_size",
      "base_family",
      "grid_color",
      "legend_position",
      "palette_fill",
      "palette_colour"
    ),
    ...
  )
}
