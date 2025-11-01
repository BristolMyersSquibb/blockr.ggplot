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
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
