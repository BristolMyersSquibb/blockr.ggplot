#!/usr/bin/env Rscript

#' Generate Single Screenshot for blockr.ggplot
#'
#' This script generates one screenshot at a time using the new validate_block_screenshot function.
#' Run this script multiple times, changing the block_type parameter.
#'
#' Usage:
#' R -e "block_type <- 'scatter'; source('inst/scripts/generate_single_screenshot.R')"

# Load the development version of blockr.ggplot from current directory
# This ensures we test the current code without needing to install the package
devtools::load_all()

# Default to scatter if not specified
if (!exists("block_type")) {
  block_type <- "scatter"
}

cat(sprintf("Generating screenshot for: %s\n", block_type))

# Generate screenshot based on block_type using the new validate_block_screenshot function
result <- switch(
  block_type,
  "scatter" = validate_block_screenshot(
    new_chart_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl",
      size = "hp"
    ),
    filename = "scatter-plot.png"
  ),

  "bar" = validate_block_screenshot(
    new_chart_block(
      type = "bar",
      x = "cyl",
      fill = "gear",
      position = "dodge"
    ),
    filename = "bar-chart.png"
  ),

  "line" = validate_block_screenshot(
    new_chart_block(
      type = "line",
      x = "Time",
      y = "demand"
    ),
    data = BOD,
    filename = "line-chart.png"
  ),

  "pie" = validate_block_screenshot(
    new_chart_block(
      type = "pie",
      x = "Species"
    ),
    data = iris,
    filename = "pie-chart.png"
  ),

  "boxplot" = validate_block_screenshot(
    new_chart_block(
      type = "boxplot",
      x = "cyl",
      y = "mpg",
      fill = "gear"
    ),
    filename = "boxplot.png"
  ),

  "histogram" = validate_block_screenshot(
    new_chart_block(
      type = "histogram",
      x = "mpg",
      bins = 15,
      fill = "cyl",
      position = "identity",
      alpha = 0.5
    ),
    filename = "histogram.png"
  ),

  "area" = validate_block_screenshot(
    new_chart_block(
      type = "area",
      x = "Time",
      y = "demand"
    ),
    data = BOD,
    filename = "area-chart.png"
  ),

  "density" = validate_block_screenshot(
    new_chart_block(
      type = "density",
      x = "mpg",
      fill = "cyl"
    ),
    filename = "density-plot.png"
  ),

  "violin" = validate_block_screenshot(
    new_chart_block(
      type = "violin",
      x = "cyl",
      y = "mpg",
      fill = "cyl"
    ),
    filename = "violin-plot.png"
  ),

  # Individual blocks for reference (prefixed with individual-)
  "individual-scatter" = validate_block_screenshot(
    new_scatter_plot_block(
      x = "wt",
      y = "mpg",
      color = "cyl",
      size = "hp",
      add_smooth = TRUE
    ),
    filename = "individual-scatter-plot.png"
  ),

  "individual-bar" = validate_block_screenshot(
    new_bar_chart_block(
      x = "cyl",
      fill = "gear",
      position = "dodge"
    ),
    filename = "individual-bar-chart.png"
  ),

  "individual-line" = validate_block_screenshot(
    new_line_chart_block(
      x = "Time",
      y = "demand"
    ),
    data = BOD,
    filename = "individual-line-chart.png"
  ),

  "individual-pie" = validate_block_screenshot(
    new_pie_chart_block(
      x = "Species",
      show_labels = TRUE
    ),
    data = iris,
    filename = "individual-pie-chart.png"
  ),

  "individual-boxplot" = validate_block_screenshot(
    new_boxplot_block(
      x = "cyl",
      y = "mpg",
      fill = "gear"
    ),
    filename = "individual-boxplot.png"
  ),

  "individual-histogram" = validate_block_screenshot(
    new_histogram_block(
      x = "mpg",
      bins = 15,
      fill = "cyl"
    ),
    filename = "individual-histogram.png"
  ),

  "individual-area" = validate_block_screenshot(
    new_area_chart_block(
      x = "Time",
      y = "demand"
    ),
    data = BOD,
    filename = "individual-area-chart.png"
  ),

  "individual-density" = validate_block_screenshot(
    new_density_plot_block(
      x = "mpg",
      fill = "cyl"
    ),
    filename = "individual-density-plot.png"
  ),

  "individual-violin" = validate_block_screenshot(
    new_violin_plot_block(
      x = "cyl",
      y = "mpg",
      fill = "cyl"
    ),
    filename = "individual-violin-plot.png"
  ),

  "heatmap" = {
    ucb_data <- as.data.frame(UCBAdmissions)
    validate_block_screenshot(
      new_heatmap_block(
        x = "Dept",
        y = "Gender",
        fill = "Freq",
        color_palette = "viridis"
      ),
      data = ucb_data,
      filename = "heatmap.png"
    )
  },

  "chart" = validate_block_screenshot(
    new_chart_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl",
      size = "hp"
    ),
    filename = "chart-block.png"
  ),

  "chart-pie" = validate_block_screenshot(
    new_chart_block(
      type = "pie",
      x = "Species",
      alpha = 1.0
    ),
    data = iris,
    filename = "chart-pie.png"
  ),

  "chart-donut" = validate_block_screenshot(
    new_chart_block(
      type = "pie",
      x = "Species",
      alpha = 1.0,
      donut = TRUE
    ),
    data = iris,
    filename = "chart-donut.png"
  ),

  "chart-bar" = validate_block_screenshot(
    new_chart_block(
      type = "bar",
      x = "cyl",
      fill = "gear",
      position = "dodge"
    ),
    filename = "chart-bar.png"
  ),

  "chart-line" = validate_block_screenshot(
    new_chart_block(
      type = "line",
      x = "Time",
      y = "demand"
    ),
    data = BOD,
    filename = "chart-line.png"
  ),

  "chart-histogram" = validate_block_screenshot(
    new_chart_block(
      type = "histogram",
      x = "mpg",
      fill = "cyl",
      bins = 15
    ),
    filename = "chart-histogram.png"
  ),

  stop(sprintf(
    "Unknown block_type: %s. Valid options: scatter, bar, line, pie, boxplot, histogram, area, density, violin, heatmap, chart, chart-pie, chart-donut, chart-bar, chart-line, chart-histogram, individual-scatter, individual-bar, individual-line, individual-pie, individual-boxplot, individual-histogram, individual-area, individual-density, individual-violin",
    block_type
  ))
)

# Report the result
if (!is.null(result)) {
  if (result$success) {
    cat(sprintf("✓ Screenshot generation complete for %s!\n", block_type))
    cat(sprintf("Screenshot saved to: %s\n", result$path))
  } else {
    cat(sprintf("✗ Screenshot generation failed for %s\n", block_type))
    cat(sprintf("Error: %s\n", result$error))
  }
} else {
  cat("No result returned - check block_type\n")
}
