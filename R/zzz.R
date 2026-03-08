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
    arguments = list(
      structure(
        c(
          type = paste0(
            'Plot type. Valid values: "point", "bar", "col", "line", ',
            '"histogram", "density", "boxplot", "violin", "pie". ',
            'NOTE: "scatter" is NOT valid — use "point" for scatter plots.'
          ),
          x = "Column name from the data (required for all plot types)",
          y = paste0(
            'Column name from the data, or "(none)". ',
            'For bar and histogram, set y = "(none)" to auto-count. ',
            'For pie, y = "(none)" means equal slices. ',
            "All other types require a real column name for y."
          ),
          color = 'Column name for color aesthetic, or "(none)" to omit',
          fill = 'Column name for fill aesthetic, or "(none)" to omit',
          size = 'Column name for size aesthetic, or "(none)" to omit',
          shape = 'Column name for shape aesthetic, or "(none)" to omit',
          linetype = 'Column name for linetype aesthetic, or "(none)" to omit',
          group = 'Column name for group aesthetic, or "(none)" to omit',
          alpha = 'Column name for alpha aesthetic, or "(none)" to omit',
          density_alpha = "Number between 0 and 1, opacity for density plots (default 0.8)",
          position = '"stack", "dodge", or "fill" — position adjustment for bar/col/histogram',
          bins = "Integer, number of bins for histogram (default 30)",
          donut = "Boolean, if true render pie chart as donut (only applies to pie type)"
        ),
        examples = list(
          type = "point",
          x = "Sepal.Length",
          y = "Sepal.Width",
          color = "Species",
          fill = "(none)",
          size = "(none)",
          shape = "(none)",
          linetype = "(none)",
          group = "(none)",
          alpha = "(none)",
          density_alpha = 0.8,
          position = "stack",
          bins = 30L,
          donut = FALSE
        ),
        prompt = paste(
          "CRITICAL — valid type values: point, bar, col, line, histogram,",
          "density, boxplot, violin, pie.",
          '"scatter" is NOT valid — always use "point" for scatter plots.',
          '\n\nOnly literal column names or "(none)" are accepted for',
          "x, y, color, fill, size, shape, linetype, group, alpha.",
          "No expressions, no functions (e.g. mean(x) is invalid),",
          'no computed stats (e.g. "..count..", "stat(count)" are invalid).',
          '\n\n"(none)" is the sentinel value for omitting an aesthetic.',
          "Do not use empty string, null, or NA.",
          '\n\nFor bar charts counting rows: set y = "(none)".',
          '\n\nFor histogram: set y = "(none)" — counts are automatic.',
          "\n\nIf the user wants computed statistics (mean, median, sum, etc.),",
          "suggest adding an upstream Summarize/Aggregate block first,",
          "then plot the pre-computed column."
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
    arguments = list(
      structure(
        c(
          ncol = 'Number of columns: "1", "2", "3", "4", "5", or "" for auto',
          nrow = 'Number of rows: "1", "2", "3", "4", "5", or "" for auto',
          title = 'Overall grid title string, or "" for none',
          subtitle = 'Grid subtitle string, or "" for none',
          caption = 'Grid caption string, or "" for none',
          tag_levels = 'Panel tag labels: "A" (uppercase), "a" (lowercase), "1" (numeric), "I" (roman), "i" (roman lower), or "" for none',
          guides = '"auto", "collect", or "keep". Use "collect" to share legends across plots'
        ),
        examples = list(
          ncol = "2",
          nrow = "",
          title = "My Plots",
          subtitle = "",
          caption = "",
          tag_levels = "A",
          guides = "collect"
        ),
        prompt = paste(
          "This block arranges existing plot outputs into a grid layout.",
          "It does NOT create plots — plots must come from upstream ggplot blocks.",
          'Use guides = "collect" to de-duplicate shared legends across panels.'
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
    arguments = list(
      structure(
        c(
          base_theme = paste0(
            'Base ggplot2 theme: "auto", "minimal", "classic", "gray", "bw", ',
            '"light", "dark", "void". Additional themes available if packages ',
            'installed: "economist", "fivethirtyeight", "tufte", "wsj", ',
            '"gdocs", "calc", "solarized", "pander", "clean"'
          ),
          legend_position = '"auto", "right", "left", "top", "bottom", "none"',
          palette_fill = paste0(
            'Fill color palette: "auto", "viridis_d", "viridis_c", "magma_d", ',
            '"magma_c", "plasma_d", "plasma_c", "ggplot2"'
          ),
          palette_colour = paste0(
            'Colour palette: "auto", "viridis_d", "viridis_c", "magma_d", ',
            '"magma_c", "plasma_d", "plasma_c", "ggplot2"'
          ),
          base_size = '"auto" or a numeric string (e.g. "14") for base font size',
          base_family = '"auto", "sans", "serif", or "mono"',
          panel_bg = 'Hex color string for panel background (e.g. "#FFFFFF"), or "" for theme default',
          plot_bg = 'Hex color string for plot background, or "" for theme default',
          grid_color = 'Hex color string for grid lines, or "" for theme default',
          show_major_grid = '"auto", "show", or "hide"',
          show_minor_grid = '"auto", "show", or "hide"',
          show_panel_border = '"auto", "show", or "hide"'
        ),
        examples = list(
          base_theme = "minimal",
          legend_position = "bottom",
          palette_fill = "viridis_d",
          palette_colour = "auto",
          base_size = "auto",
          base_family = "auto",
          panel_bg = "",
          plot_bg = "",
          grid_color = "",
          show_major_grid = "auto",
          show_minor_grid = "auto",
          show_panel_border = "auto"
        ),
        prompt = paste(
          'Use "auto" to keep the upstream default for any setting.',
          'Use "" (empty string) for color fields to keep theme defaults.',
          "\n\nPalette suffix rule: use _d (discrete) for categorical data",
          "and _c (continuous) for numeric scales.",
          'Use "ggplot2" for the default ggplot2 color scheme.'
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
    arguments = list(
      structure(
        c(
          facet_type = '"wrap" (one variable, flexible layout) or "grid" (row/column matrix)',
          facets = "Array of column names for facet_wrap (used when facet_type is wrap)",
          rows = "Array of column names for grid rows (used when facet_type is grid)",
          cols = "Array of column names for grid columns (used when facet_type is grid)",
          ncol = 'Number of columns for wrap: "1"-"5", or "" for auto',
          nrow = 'Number of rows for wrap: "1"-"5", or "" for auto',
          scales = '"fixed", "free", "free_x", or "free_y"',
          labeller = '"label_value", "label_both", or "label_parsed"',
          dir = '"h" (horizontal) or "v" (vertical) — fill direction for wrap',
          space = '"fixed", "free_x", or "free_y" — panel sizing for grid'
        ),
        examples = list(
          facet_type = "wrap",
          facets = list("Species"),
          rows = list(),
          cols = list(),
          ncol = "2",
          nrow = "",
          scales = "fixed",
          labeller = "label_value",
          dir = "h",
          space = "fixed"
        ),
        prompt = paste(
          "For wrap: set facets and leave rows/cols as empty arrays.",
          "For grid: set rows and/or cols and leave facets as empty array.",
          "ncol and nrow only apply to wrap layout.",
          "space only applies to grid layout."
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
