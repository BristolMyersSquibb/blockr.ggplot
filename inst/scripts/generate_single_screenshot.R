#!/usr/bin/env Rscript

#' Generate Single Screenshot for blockr.ggplot
#'
#' This script generates one screenshot at a time to avoid timeouts.
#' Run this script multiple times, changing the block_type parameter.
#'
#' Usage:
#' R -e "block_type <- 'scatter'; source('inst/scripts/generate_single_screenshot.R')"

# Check dependencies
if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("Please install webshot2: install.packages('webshot2')")
}

if (!requireNamespace("blockr.core", quietly = TRUE)) {
  stop("blockr.core package must be installed")
}

library(webshot2)
library(blockr.core)

# Load the development version of blockr.ggplot from current directory
# This ensures we test the current code without needing to install the package
devtools::load_all()

# Increase timeout for Shiny app launching
options(webshot.app.timeout = 120)

# Configuration
SCREENSHOT_WIDTH <- 800
SCREENSHOT_HEIGHT <- 600
OUTPUT_DIR <- "man/figures"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Default to scatter if not specified
if (!exists("block_type")) {
  block_type <- "scatter"
}

cat(sprintf("Generating screenshot for: %s\n", block_type))

# Helper function to create temporary app and take screenshot
create_screenshot <- function(block, filename, data = list(data = mtcars)) {
  cat(sprintf("Generating %s...\n", filename))

  tryCatch(
    {
      # Create temporary directory for the app
      temp_dir <- tempfile("blockr_app")
      dir.create(temp_dir)

      # Save data to RDS file
      saveRDS(data, file.path(temp_dir, "data.rds"))

      # Create minimal app.R file
      app_content <- sprintf(
        '
library(blockr.core)

# Load the development version of blockr.ggplot
devtools::load_all("%s")

# Load data
data <- readRDS("data.rds")

# Run the app
blockr.core::serve(
  %s,
  data = data
)
    ',
        normalizePath("."),
        deparse(substitute(block), width.cutoff = 500)
      )

      writeLines(app_content, file.path(temp_dir, "app.R"))

      # Take screenshot
      webshot2::appshot(
        app = temp_dir,
        file = file.path(OUTPUT_DIR, filename),
        vwidth = SCREENSHOT_WIDTH,
        vheight = SCREENSHOT_HEIGHT,
        delay = 5 # Wait for app to load
      )

      # Cleanup
      unlink(temp_dir, recursive = TRUE)

      cat(sprintf("✓ %s created\n", filename))
    },
    error = function(e) {
      cat(sprintf("✗ Failed to create %s: %s\n", filename, e$message))
    }
  )
}

# Generate screenshot based on block_type
switch(
  block_type,
  "scatter" = create_screenshot(
    new_scatter_plot_block(
      x = "wt",
      y = "mpg",
      color = "cyl",
      size = "hp",
      add_smooth = TRUE
    ),
    "scatter-plot.png"
  ),

  "bar" = create_screenshot(
    new_bar_chart_block(
      x = "cyl",
      fill = "gear",
      position = "dodge"
    ),
    "bar-chart.png"
  ),

  "line" = create_screenshot(
    new_line_chart_block(
      x = "Time",
      y = "demand"
    ),
    "line-chart.png",
    data = list(data = BOD)
  ),

  "pie" = create_screenshot(
    new_pie_chart_block(
      x = "Species",
      show_labels = TRUE
    ),
    "pie-chart.png",
    data = list(data = iris)
  ),

  "boxplot" = create_screenshot(
    new_boxplot_block(
      x = "cyl",
      y = "mpg",
      fill = "gear"
    ),
    "boxplot.png"
  ),

  "histogram" = create_screenshot(
    new_histogram_block(
      x = "mpg",
      bins = 15,
      fill = "cyl"
    ),
    "histogram.png"
  ),

  "area" = create_screenshot(
    new_area_chart_block(
      x = "Time",
      y = "demand"
    ),
    "area-chart.png",
    data = list(data = BOD)
  ),

  "density" = create_screenshot(
    new_density_plot_block(
      x = "mpg",
      fill = "cyl"
    ),
    "density-plot.png"
  ),

  "violin" = create_screenshot(
    new_violin_plot_block(
      x = "cyl",
      y = "mpg",
      fill = "cyl"
    ),
    "violin-plot.png"
  ),

  "heatmap" = {
    ucb_data <- as.data.frame(UCBAdmissions)
    create_screenshot(
      new_heatmap_block(
        x = "Dept",
        y = "Gender",
        fill = "Freq",
        color_palette = "viridis"
      ),
      "heatmap.png",
      data = list(data = ucb_data)
    )
  },

  "chart" = create_screenshot(
    new_chart_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl",
      size = "hp"
    ),
    "chart-block.png"
  ),

  "chart-pie" = create_screenshot(
    new_chart_block(
      type = "pie",
      x = "Species",
      alpha = 1.0
    ),
    "chart-pie.png",
    data = list(data = iris)
  ),

  "chart-donut" = create_screenshot(
    new_chart_block(
      type = "pie",
      x = "Species",
      alpha = 1.0,
      donut = TRUE
    ),
    "chart-donut.png",
    data = list(data = iris)
  ),

  "chart-bar" = create_screenshot(
    new_chart_block(
      type = "bar",
      x = "cyl",
      fill = "gear",
      position = "dodge"
    ),
    "chart-bar.png"
  ),

  "chart-line" = create_screenshot(
    new_chart_block(
      type = "line",
      x = "Time",
      y = "demand"
    ),
    "chart-line.png",
    data = list(data = BOD)
  ),

  "chart-histogram" = create_screenshot(
    new_chart_block(
      type = "histogram",
      x = "mpg",
      fill = "cyl",
      bins = 15
    ),
    "chart-histogram.png"
  ),

  stop(sprintf(
    "Unknown block_type: %s. Valid options: scatter, bar, line, pie, boxplot, histogram, area, density, violin, heatmap, chart, chart-pie, chart-donut, chart-bar, chart-line, chart-histogram",
    block_type
  ))
)

cat(sprintf("Screenshot generation complete for %s!\n", block_type))
cat(sprintf("Screenshot saved to: %s/\n", OUTPUT_DIR))
