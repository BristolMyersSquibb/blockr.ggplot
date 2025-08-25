register_ggplot_blocks <- function() {
  # nocov start
  register_blocks(
    c(
      "new_area_chart_block",
      "new_bar_chart_block",
      "new_boxplot_block",
      "new_density_plot_block",
      "new_heatmap_block",
      "new_line_chart_block",
      "new_pie_chart_block",
      "new_scatter_plot_block",
      "new_violin_plot_block"
    ),
    name = c(
      "Area chart block",
      "Bar chart block",
      "Boxplot block",
      "Density plot block",
      "Heatmap block",
      "Line chart block",
      "Pie chart block",
      "Scatter plot block",
      "Violin plot block"
    ),
    description = c(
      "Create an area chart using ggplot2",
      "Create a bar chart using ggplot2",
      "Create a boxplot using ggplot2",
      "Create a density plot using ggplot2",
      "Create a heatmap using ggplot2",
      "Create a line chart using ggplot2",
      "Create a pie chart using ggplot2",
      "Create a scatter plot using ggplot2",
      "Create a violin plot using ggplot2"
    ),
    category = c(
      "plot",
      "plot",
      "plot",
      "plot",
      "plot",
      "plot",
      "plot",
      "plot",
      "plot"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
