register_ggplot_blocks <- function() {
  # nocov start
  register_blocks(
    "new_ggplot_block",
    name = "ggplot",
    description = paste0(
      "Universal ggplot2 visualization block. ",
      "Create scatter, bar, line, box, violin, density, area, ",
      "histogram, and pie charts with dynamic controls. ",
      "Simply select your chart type and the interface adapts ",
      "to show relevant aesthetics and options."
    ),
    category = "plot",
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_plot_grid_block",
    name = "Plot Grid",
    description = paste0(
      "Combine multiple ggplot objects into a responsive grid layout. ",
      "Supports automatic or manual layout control (rows/columns), ",
      "legend management, and annotations (title, subtitle, tags). ",
      "Uses patchwork for professional plot composition with ",
      "visual layout preview."
    ),
    category = "plot",
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_theme_block",
    name = "Theme",
    description = paste0(
      "Advanced theme customization for ggplot2 plots. ",
      "Fine-tune backgrounds, fonts, grid lines, panel borders, ",
      "and legend positioning. Use color pickers for backgrounds, ",
      "sliders for font sizes, and checkboxes for grid visibility. ",
      "Perfect for creating custom styled visualizations."
    ),
    category = "plot",
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
