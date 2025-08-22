#!/usr/bin/env Rscript

#' Generate Screenshots for blockr.ggplot README Documentation
#'
#' This script generates screenshots of all blockr.ggplot blocks for inclusion
#' in the README. Screenshots are saved to man/figures/ directory.
#'
#' Requirements:
#' - webshot2 package: install.packages("webshot2")
#' - Chrome/Chromium browser installed on system
#'
#' Usage:
#' source("inst/scripts/generate_screenshots.R")
#' # or from installed package:
#' # source(system.file("scripts/generate_screenshots.R", package = "blockr.ggplot"))

# Check dependencies
if (!requireNamespace("webshot2", quietly = TRUE)) {
  stop("Please install webshot2: install.packages('webshot2')")
}

if (!requireNamespace("blockr.ggplot", quietly = TRUE)) {
  stop("blockr.ggplot package must be installed and loaded")
}

if (!requireNamespace("blockr.core", quietly = TRUE)) {
  stop("blockr.core package must be installed")
}

library(webshot2)
library(blockr.ggplot)
library(blockr.core)

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

cat("Generating screenshots for blockr.ggplot blocks...\n")

# Helper function to create temporary app and take screenshot
create_screenshot <- function(block, filename, data = list(data = mtcars)) {
  cat(sprintf("Generating %s...\n", filename))

  tryCatch({
    # Create temporary directory for the app
    temp_dir <- tempfile("blockr_app")
    dir.create(temp_dir)

    # Save data to RDS file
    saveRDS(data, file.path(temp_dir, "data.rds"))

    # Create minimal app.R file
    app_content <- sprintf('
library(blockr.ggplot)
library(blockr.core)

# Load data
data <- readRDS("data.rds")

# Run the app
blockr.core::serve(
  %s,
  data = data
)
    ', deparse(substitute(block), width.cutoff = 500))

    writeLines(app_content, file.path(temp_dir, "app.R"))

    # Take screenshot
    webshot2::appshot(
      app = temp_dir,
      file = file.path(OUTPUT_DIR, filename),
      vwidth = SCREENSHOT_WIDTH,
      vheight = SCREENSHOT_HEIGHT,
      delay = 5  # Wait for app to load
    )

    # Cleanup
    unlink(temp_dir, recursive = TRUE)

    cat(sprintf("✓ %s created\n", filename))

  }, error = function(e) {
    cat(sprintf("✗ Failed to create %s: %s\n", filename, e$message))
  })
}

# Generate screenshots for each block type

# 1. Scatter Plot Block
create_screenshot(
  new_scatter_plot_block(
    x = "wt",
    y = "mpg",
    color = "cyl",
    size = "hp",
    add_smooth = TRUE
  ),
  "scatter-plot.png"
)

# 2. Bar Chart Block
create_screenshot(
  new_bar_chart_block(
    x = "cyl",
    fill = "gear",
    position = "dodge"
  ),
  "bar-chart.png"
)

# 3. Line Chart Block
create_screenshot(
  new_line_chart_block(
    x = "Time",
    y = "demand"
  ),
  "line-chart.png",
  data = list(data = BOD)
)

# 4. Pie Chart Block
create_screenshot(
  new_pie_chart_block(
    x = "Species",
    show_labels = TRUE
  ),
  "pie-chart.png",
  data = list(data = iris)
)

# 5. Boxplot Block
create_screenshot(
  new_boxplot_block(
    x = "cyl",
    y = "mpg",
    fill = "gear"
  ),
  "boxplot.png"
)

# 6. Histogram Block
create_screenshot(
  new_histogram_block(
    x = "mpg",
    bins = 15,
    fill = "cyl"
  ),
  "histogram.png"
)

# 7. Area Chart Block
create_screenshot(
  new_area_chart_block(
    x = "Time",
    y = "demand"
  ),
  "area-chart.png",
  data = list(data = BOD)
)

# 8. Density Plot Block
create_screenshot(
  new_density_plot_block(
    x = "mpg",
    fill = "cyl"
  ),
  "density-plot.png"
)

# 9. Violin Plot Block
create_screenshot(
  new_violin_plot_block(
    x = "cyl",
    y = "mpg",
    fill = "cyl"
  ),
  "violin-plot.png"
)

# 10. Heatmap Block (using UCB Admissions data)
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

cat("\nScreenshot generation complete!\n")
cat(sprintf("Screenshots saved to: %s/\n", OUTPUT_DIR))
cat("\nTo use in README, reference as: ![Description](man/figures/screenshot-name.png)\n")