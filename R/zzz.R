register_ggplot_blocks <- function() {
  # nocov start
  register_blocks(
    "new_ggplot_block",
    name = "ggplot",
    description = paste0(
      "Create visualizations including scatter plots, bar charts, ",
      "line graphs, histograms, and more"
    ),
    category = "plot",
    icon = "bar-chart-line",
    guidance = paste(
      "CRITICAL -- valid type values: point, bar, col, line, histogram,",
      "density, boxplot, violin, pie.",
      '"scatter" is NOT valid -- always use "point" for scatter plots.',
      '\n\nOnly literal column names or "(none)" are accepted for',
      "x, y, color, fill, size, shape, linetype, group, alpha.",
      "No expressions, no functions (e.g. mean(x) is invalid),",
      'no computed stats (e.g. "..count..", "stat(count)" are invalid).',
      '\n\n"(none)" is the sentinel value for omitting an aesthetic.',
      "Do not use empty string, null, or NA.",
      '\n\nFor bar charts counting rows: set y = "(none)".',
      '\n\nFor histogram: set y = "(none)" -- counts are automatic.',
      "\n\nIf the user wants computed statistics (mean, median, sum, etc.),",
      "suggest adding an upstream Summarize/Aggregate block first,",
      "then plot the pre-computed column."
    ),
    arguments = list(
      new_block_args(
        type = new_block_arg(
          paste0(
            'Plot type. Valid values: "point", "bar", "col", "line", ',
            '"histogram", "density", "boxplot", "violin", "pie". ',
            'NOTE: "scatter" is NOT valid -- use "point" for scatter plots.'
          ),
          example = "point",
          type = arg_enum(
            c("point", "bar", "col", "line", "histogram",
              "density", "boxplot", "violin", "pie")
          )
        ),
        x = new_block_arg(
          "Column name from the data (required for all plot types)",
          example = "Sepal.Length",
          type = arg_string()
        ),
        y = new_block_arg(
          paste0(
            'Column name from the data, or "(none)". ',
            'For bar and histogram, set y = "(none)" to auto-count. ',
            'For pie, y = "(none)" means equal slices. ',
            "All other types require a real column name for y."
          ),
          example = "Sepal.Width",
          type = arg_string()
        ),
        color = new_block_arg(
          'Column name for color aesthetic, or "(none)" to omit',
          example = "Species",
          type = arg_string()
        ),
        fill = new_block_arg(
          'Column name for fill aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        size = new_block_arg(
          'Column name for size aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        shape = new_block_arg(
          'Column name for shape aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        linetype = new_block_arg(
          'Column name for linetype aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        group = new_block_arg(
          'Column name for group aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        alpha = new_block_arg(
          'Column name for alpha aesthetic, or "(none)" to omit',
          example = "(none)",
          type = arg_string()
        ),
        density_alpha = new_block_arg(
          "Number between 0 and 1, opacity for density plots (default 0.8)",
          example = 0.8,
          type = arg_number()
        ),
        position = new_block_arg(
          '"stack", "dodge", or "fill" -- position adjustment for bar/col/histogram',
          example = "stack",
          type = arg_enum(c("stack", "dodge", "fill"))
        ),
        bins = new_block_arg(
          "Integer, number of bins for histogram (default 30)",
          example = 30L,
          type = arg_integer()
        ),
        donut = new_block_arg(
          "Boolean, if true render pie chart as donut (only applies to pie type)",
          example = FALSE,
          type = arg_boolean()
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_grid_block",
    name = "Grid",
    description = paste0(
      "Combine multiple ggplot objects into a grid layout ",
      "with annotations"
    ),
    category = "plot",
    icon = "grid-3x3",
    guidance = paste(
      "This block arranges existing plot outputs into a grid layout.",
      "It does NOT create plots -- plots must come from upstream ggplot blocks.",
      'Use guides = "collect" to de-duplicate shared legends across panels.'
    ),
    arguments = list(
      new_block_args(
        ncol = new_block_arg(
          'Number of columns: "1", "2", "3", "4", "5", or "" for auto',
          example = "2",
          type = arg_enum(c("1", "2", "3", "4", "5", ""))
        ),
        nrow = new_block_arg(
          'Number of rows: "1", "2", "3", "4", "5", or "" for auto',
          example = "",
          type = arg_enum(c("1", "2", "3", "4", "5", ""))
        ),
        title = new_block_arg(
          'Overall grid title string, or "" for none',
          example = "My Plots",
          type = arg_string()
        ),
        subtitle = new_block_arg(
          'Grid subtitle string, or "" for none',
          example = "",
          type = arg_string()
        ),
        caption = new_block_arg(
          'Grid caption string, or "" for none',
          example = "",
          type = arg_string()
        ),
        tag_levels = new_block_arg(
          paste0(
            'Panel tag labels: "A" (uppercase), "a" (lowercase), ',
            '"1" (numeric), "I" (roman), "i" (roman lower), or "" for none'
          ),
          example = "A",
          type = arg_enum(c("A", "a", "1", "I", "i", ""))
        ),
        guides = new_block_arg(
          '"auto", "collect", or "keep". Use "collect" to share legends across plots',
          example = "collect",
          type = arg_enum(c("auto", "collect", "keep"))
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_theme_block",
    name = "Theme",
    description = paste0(
      "Customize plot appearance including colors, fonts, backgrounds, ",
      "and grid lines"
    ),
    category = "plot",
    icon = "palette2",
    guidance = paste(
      'Use "auto" to keep the upstream default for any setting.',
      'Use "" (empty string) for color fields to keep theme defaults.',
      "\n\nPalette suffix rule: use _d (discrete) for categorical data",
      "and _c (continuous) for numeric scales.",
      'Use "ggplot2" for the default ggplot2 color scheme.'
    ),
    arguments = list(
      new_block_args(
        base_theme = new_block_arg(
          paste0(
            'Base ggplot2 theme: "auto", "minimal", "classic", "gray", "bw", ',
            '"light", "dark", "void". Additional themes available if packages ',
            'installed: "economist", "fivethirtyeight", "tufte", "wsj", ',
            '"gdocs", "calc", "solarized", "pander", "clean"'
          ),
          example = "minimal",
          type = arg_enum(
            c("auto", "minimal", "classic", "gray", "bw", "light", "dark",
              "void", "economist", "fivethirtyeight", "tufte", "wsj",
              "gdocs", "calc", "solarized", "pander", "clean")
          )
        ),
        legend_position = new_block_arg(
          '"auto", "right", "left", "top", "bottom", "none"',
          example = "bottom",
          type = arg_enum(
            c("auto", "right", "left", "top", "bottom", "none")
          )
        ),
        palette_fill = new_block_arg(
          paste0(
            'Fill color palette: "auto", "viridis_d", "viridis_c", "magma_d", ',
            '"magma_c", "plasma_d", "plasma_c", "ggplot2"'
          ),
          example = "viridis_d",
          type = arg_enum(
            c("auto", "viridis_d", "viridis_c", "magma_d", "magma_c",
              "plasma_d", "plasma_c", "ggplot2")
          )
        ),
        palette_colour = new_block_arg(
          paste0(
            'Colour palette: "auto", "viridis_d", "viridis_c", "magma_d", ',
            '"magma_c", "plasma_d", "plasma_c", "ggplot2"'
          ),
          example = "auto",
          type = arg_enum(
            c("auto", "viridis_d", "viridis_c", "magma_d", "magma_c",
              "plasma_d", "plasma_c", "ggplot2")
          )
        ),
        base_size = new_block_arg(
          '"auto" or a numeric string (e.g. "14") for base font size',
          example = "auto",
          type = arg_string()
        ),
        base_family = new_block_arg(
          '"auto", "sans", "serif", or "mono"',
          example = "auto",
          type = arg_enum(c("auto", "sans", "serif", "mono"))
        ),
        panel_bg = new_block_arg(
          'Hex color string for panel background (e.g. "#FFFFFF"), or "" for theme default',
          example = "",
          type = arg_string()
        ),
        plot_bg = new_block_arg(
          'Hex color string for plot background, or "" for theme default',
          example = "",
          type = arg_string()
        ),
        grid_color = new_block_arg(
          'Hex color string for grid lines, or "" for theme default',
          example = "",
          type = arg_string()
        ),
        show_major_grid = new_block_arg(
          '"auto", "show", or "hide"',
          example = "auto",
          type = arg_enum(c("auto", "show", "hide"))
        ),
        show_minor_grid = new_block_arg(
          '"auto", "show", or "hide"',
          example = "auto",
          type = arg_enum(c("auto", "show", "hide"))
        ),
        show_panel_border = new_block_arg(
          '"auto", "show", or "hide"',
          example = "auto",
          type = arg_enum(c("auto", "show", "hide"))
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_facet_block",
    name = "Facet",
    description = paste0(
      "Split plots into multiple panels based on ",
      "categorical variables"
    ),
    category = "plot",
    icon = "grid-3x2",
    guidance = paste(
      "For wrap: set facets and leave rows/cols as empty arrays.",
      "For grid: set rows and/or cols and leave facets as empty array.",
      "ncol and nrow only apply to wrap layout.",
      "space only applies to grid layout."
    ),
    arguments = list(
      new_block_args(
        facet_type = new_block_arg(
          '"wrap" (one variable, flexible layout) or "grid" (row/column matrix)',
          example = "wrap",
          type = arg_enum(c("wrap", "grid"))
        ),
        facets = new_block_arg(
          "Array of column names for facet_wrap (used when facet_type is wrap)",
          example = list("Species"),
          type = arg_array(arg_string())
        ),
        rows = new_block_arg(
          "Array of column names for grid rows (used when facet_type is grid)",
          example = list(),
          type = arg_array(arg_string())
        ),
        cols = new_block_arg(
          "Array of column names for grid columns (used when facet_type is grid)",
          example = list(),
          type = arg_array(arg_string())
        ),
        ncol = new_block_arg(
          'Number of columns for wrap: "1"-"5", or "" for auto',
          example = "2",
          type = arg_enum(c("1", "2", "3", "4", "5", ""))
        ),
        nrow = new_block_arg(
          'Number of rows for wrap: "1"-"5", or "" for auto',
          example = "",
          type = arg_enum(c("1", "2", "3", "4", "5", ""))
        ),
        scales = new_block_arg(
          '"fixed", "free", "free_x", or "free_y"',
          example = "fixed",
          type = arg_enum(c("fixed", "free", "free_x", "free_y"))
        ),
        labeller = new_block_arg(
          '"label_value", "label_both", or "label_parsed"',
          example = "label_value",
          type = arg_enum(c("label_value", "label_both", "label_parsed"))
        ),
        dir = new_block_arg(
          '"h" (horizontal) or "v" (vertical) -- fill direction for wrap',
          example = "h",
          type = arg_enum(c("h", "v"))
        ),
        space = new_block_arg(
          '"fixed", "free_x", or "free_y" -- panel sizing for grid',
          example = "fixed",
          type = arg_enum(c("fixed", "free_x", "free_y"))
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
