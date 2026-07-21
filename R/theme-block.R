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

#' Get the base theme call for a given theme name
#'
#' Maps theme names to their corresponding ggplot2 (or add-on package)
#' function calls, returned as language objects. Falls back to
#' `ggplot2::theme_minimal()` when the requested theme's package is not
#' available.
#'
#' @param theme_name Character string naming the theme
#' @return A language object (the theme function call), or NULL in "auto"
#'   mode (keep the upstream theme).
#' @keywords internal
get_theme_call <- function(theme_name) {
  # Handle auto mode (keep upstream theme)
  if (theme_name == "auto") {
    return(NULL)
  }

  minimal <- quote(ggplot2::theme_minimal())

  # Built-in ggplot2 themes (always available)
  builtin_themes <- list(
    minimal = minimal,
    classic = quote(ggplot2::theme_classic()),
    gray = quote(ggplot2::theme_gray()),
    bw = quote(ggplot2::theme_bw()),
    light = quote(ggplot2::theme_light()),
    dark = quote(ggplot2::theme_dark()),
    void = quote(ggplot2::theme_void())
  )

  if (theme_name %in% names(builtin_themes)) {
    return(builtin_themes[[theme_name]])
  }

  # cowplot themes (check availability)
  cowplot_themes <- list(
    cowplot = quote(cowplot::theme_cowplot()),
    minimal_grid = quote(cowplot::theme_minimal_grid()),
    minimal_hgrid = quote(cowplot::theme_minimal_hgrid()),
    minimal_vgrid = quote(cowplot::theme_minimal_vgrid())
  )

  if (theme_name %in% names(cowplot_themes)) {
    if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot_themes[[theme_name]])
    } else {
      # Fallback to minimal if cowplot not available
      return(minimal)
    }
  }

  # ggthemes (check availability)
  ggthemes_themes <- list(
    economist = quote(ggthemes::theme_economist()),
    fivethirtyeight = quote(ggthemes::theme_fivethirtyeight()),
    tufte = quote(ggthemes::theme_tufte()),
    wsj = quote(ggthemes::theme_wsj())
  )

  if (theme_name %in% names(ggthemes_themes)) {
    if (requireNamespace("ggthemes", quietly = TRUE)) {
      return(ggthemes_themes[[theme_name]])
    } else {
      # Fallback to minimal if ggthemes not available
      return(minimal)
    }
  }

  # ggpubr themes (check availability)
  ggpubr_themes <- list(
    pubr = quote(ggpubr::theme_pubr()),
    pubclean = quote(ggpubr::theme_pubclean())
  )

  if (theme_name %in% names(ggpubr_themes)) {
    if (requireNamespace("ggpubr", quietly = TRUE)) {
      return(ggpubr_themes[[theme_name]])
    } else {
      # Fallback to minimal if ggpubr not available
      return(minimal)
    }
  }

  # Default fallback for unknown themes
  minimal
}

#' Build a color-palette scale call for the theme block
#'
#' @param value Palette selector: "auto" (no scale, returns NULL), "ggplot2"
#'   (reset to the default discrete scale), or a viridis option with a type
#'   suffix such as "viridis_d" / "magma_c".
#' @param aesthetic Either "fill" or "colour".
#' @return A language object (the scale call), or NULL when `value` is "auto".
#' @keywords internal
palette_scale_call <- function(value, aesthetic) {
  if (value == "auto") {
    return(NULL)
  }

  gg <- function(fun) {
    as.call(list(call("::", quote(ggplot2), as.name(fun))))
  }

  if (value == "ggplot2") {
    # Reset to ggplot2 default
    fun <- if (aesthetic == "fill") {
      "scale_fill_discrete"
    } else {
      "scale_colour_discrete"
    }
    return(gg(fun))
  }

  # Parse option name and type (viridis_d -> viridis, d)
  parts <- strsplit(value, "_")[[1]]
  option_name <- parts[1]
  scale_type <- parts[2]

  base <- if (aesthetic == "fill") {
    "scale_fill_viridis"
  } else {
    "scale_colour_viridis"
  }
  fun <- paste0(base, if (scale_type == "c") "_c" else "_d")

  as.call(list(
    call("::", quote(ggplot2), as.name(fun)),
    option = option_name
  ))
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
              # Build theme customization as a named list of language objects.
              # Names carry the ggplot2 theme element (e.g. panel.background);
              # each value is a call like ggplot2::element_rect(fill = "...").
              theme_args <- list()

              # Small builders for the element_*() values.
              element_rect_fill <- function(color) {
                bquote(ggplot2::element_rect(fill = .(color)))
              }
              element_line_colour <- function(color) {
                bquote(ggplot2::element_line(colour = .(color)))
              }

              # Panel background (only if explicitly set by user)
              if (r_panel_bg() != "") {
                theme_args[["panel.background"]] <- element_rect_fill(
                  r_panel_bg()
                )
              }

              # Plot background
              if (r_plot_bg() != "") {
                # User explicitly set a color
                theme_args[["plot.background"]] <- element_rect_fill(
                  r_plot_bg()
                )
              } else if (r_base_theme() == "gray") {
                # Gray theme: set plot background to match panel
                # (gray theme doesn't set this itself)
                theme_args[["plot.background"]] <- element_rect_fill("#EBEBEB")
              }

              # Build combined text element (base size and/or family)
              text_args <- list()
              if (r_base_size() != "auto") {
                text_args$size <- as.numeric(r_base_size())
              }
              if (r_base_family() != "" && r_base_family() != "auto") {
                text_args$family <- r_base_family()
              }
              if (length(text_args) > 0) {
                theme_args$text <- as.call(c(
                  list(quote(ggplot2::element_text)),
                  text_args
                ))
              }

              # Grid lines
              if (r_show_major_grid() == "hide") {
                # Force hide major grid
                theme_args[["panel.grid.major"]] <-
                  quote(ggplot2::element_blank())
              } else if (r_show_major_grid() == "show") {
                # Force show major grid
                theme_args[["panel.grid.major"]] <- if (r_grid_color() != "") {
                  element_line_colour(r_grid_color())
                } else {
                  quote(ggplot2::element_line())
                }
              } else if (
                r_show_major_grid() == "auto" && r_grid_color() != ""
              ) {
                # Auto mode but custom color specified - apply the color
                theme_args[["panel.grid.major"]] <- element_line_colour(
                  r_grid_color()
                )
              }
              # auto with no color: don't add anything, use base theme default

              if (r_show_minor_grid() == "hide") {
                # Force hide minor grid
                theme_args[["panel.grid.minor"]] <-
                  quote(ggplot2::element_blank())
              } else if (r_show_minor_grid() == "show") {
                # Force show minor grid
                theme_args[["panel.grid.minor"]] <- if (r_grid_color() != "") {
                  element_line_colour(r_grid_color())
                } else {
                  quote(ggplot2::element_line())
                }
              } else if (
                r_show_minor_grid() == "auto" && r_grid_color() != ""
              ) {
                # Auto mode but custom color specified - apply the color
                theme_args[["panel.grid.minor"]] <- element_line_colour(
                  r_grid_color()
                )
              }
              # auto with no color: don't add anything, use base theme default

              # Panel border
              if (r_show_panel_border() == "show") {
                # Force show panel border
                theme_args[["panel.border"]] <- quote(
                  ggplot2::element_rect(colour = "grey50", fill = NA)
                )
              } else if (r_show_panel_border() == "hide") {
                # Force hide panel border
                theme_args[["panel.border"]] <-
                  quote(ggplot2::element_blank())
              }
              # auto: don't add anything, use base theme default

              # Legend position (only if explicitly set)
              if (r_legend_position() != "" && r_legend_position() != "auto") {
                theme_args[["legend.position"]] <- r_legend_position()
              }

              # Assemble `.(data) + <base theme> + theme(...) + <palettes>` as a
              # left-associative `+` chain. `.(data)` (not a bare `data`) so
              # blockr.core's bquoted export names the upstream block in place.
              terms <- list(call(".", quote(data)))

              # Base theme (NULL in auto mode - keep upstream)
              base_theme_call <- get_theme_call(r_base_theme())
              if (!is.null(base_theme_call)) {
                terms <- c(terms, list(base_theme_call))
              }

              # Custom theme tweaks, if any
              if (length(theme_args) > 0) {
                terms <- c(
                  terms,
                  list(as.call(c(list(quote(ggplot2::theme)), theme_args)))
                )
              }

              # Color palette overrides (values like "viridis_d", "magma_c",
              # or "ggplot2" to reset to the default scale)
              terms <- c(
                terms,
                palette_scale_call(r_palette_fill(), "fill"),
                palette_scale_call(r_palette_colour(), "colour")
              )

              gg_add(terms)
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
    # See facet-block.R: `.(data)` + "bquoted" keeps the exported code as a
    # plain `plot + ggplot2::theme_minimal()` chain.
    expr_type = "bquoted",
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
