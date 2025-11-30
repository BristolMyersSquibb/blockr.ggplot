#!/usr/bin/env Rscript

# Generate screenshots for all blockr.ggplot blocks
#
# This script creates screenshots of all blocks for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("dev/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load package with devtools::load_all() to ensure latest changes are picked up
devtools::load_all(".")

# Source the validation function
source("dev/screenshots/validate-screenshot.R")

cat("Generating screenshots for all blockr.ggplot blocks...\n")
cat("Output directory: man/figures/\n\n")

# Common screenshot settings
# Note: ggplot blocks need more height to show both controls AND the plot
SCREENSHOT_WIDTH <- 1400
SCREENSHOT_HEIGHT <- 1200
SCREENSHOT_DELAY <- 4

# =============================================================================
# GGPLOT BLOCK - SCATTER (point)
# =============================================================================
cat("1/14 - ggplot block: scatter\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "point",
    x = "mpg",
    y = "hp",
    color = "cyl",
    size = "wt"
  ),
  data = mtcars,
  filename = "block-ggplot-scatter.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - BAR
# =============================================================================
cat("2/14 - ggplot block: bar\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear"
  ),
  data = mtcars,
  filename = "block-ggplot-bar.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - LINE
# =============================================================================
cat("3/14 - ggplot block: line\n")
# Use Orange dataset for time series visualization
validate_block_screenshot(
  block = new_ggplot_block(
    type = "line",
    x = "age",
    y = "circumference",
    color = "Tree"
  ),
  data = datasets::Orange,
  filename = "block-ggplot-line.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "Orange",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - BOXPLOT
# =============================================================================
cat("4/14 - ggplot block: boxplot\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "boxplot",
    x = "cyl",
    y = "mpg",
    fill = "cyl"
  ),
  data = mtcars,
  filename = "block-ggplot-boxplot.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - VIOLIN
# =============================================================================
cat("5/14 - ggplot block: violin\n")
# Use iris for better distribution visualization
validate_block_screenshot(
  block = new_ggplot_block(
    type = "violin",
    x = "Species",
    y = "Sepal.Length",
    fill = "Species"
  ),
  data = datasets::iris,
  filename = "block-ggplot-violin.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "iris",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - DENSITY
# =============================================================================
cat("6/14 - ggplot block: density\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "density",
    x = "mpg",
    fill = "cyl"
  ),
  data = mtcars,
  filename = "block-ggplot-density.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - AREA
# =============================================================================
cat("7/14 - ggplot block: area\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "area",
    x = "age",
    y = "circumference",
    fill = "Tree"
  ),
  data = datasets::Orange,
  filename = "block-ggplot-area.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "Orange",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - HISTOGRAM
# =============================================================================
cat("8/14 - ggplot block: histogram\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl"
  ),
  data = mtcars,
  filename = "block-ggplot-histogram.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - PIE
# =============================================================================
cat("9/14 - ggplot block: pie\n")
# Use iris dataset - Species as category, Sepal.Length as values
validate_block_screenshot(
  block = new_ggplot_block(
    type = "pie",
    x = "Species",
    y = "Sepal.Length",
    fill = "Species"
  ),
  data = datasets::iris,
  filename = "block-ggplot-pie.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "iris",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - DONUT
# =============================================================================
cat("10/14 - ggplot block: donut\n")
# Same as pie but with donut = TRUE for hole in center
validate_block_screenshot(
  block = new_ggplot_block(
    type = "pie",
    x = "Species",
    y = "Sepal.Length",
    fill = "Species",
    donut = TRUE
  ),
  data = datasets::iris,
  filename = "block-ggplot-donut.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  dataset = "iris",
  dataset_package = "datasets",
  verbose = FALSE
)

# =============================================================================
# FACET BLOCK - WRAP
# Uses dock with upstream ggplot: dataset → ggplot → facet
# =============================================================================
cat("11/14 - facet block: wrap\n")
validate_block_screenshot(
  block = new_facet_block(
    facet_type = "wrap",
    facets = "cyl",
    ncol = 3
  ),
  data = mtcars,
  filename = "block-facet.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  upstream_ggplot = list(type = "point", x = "wt", y = "mpg", color = "cyl"),
  verbose = FALSE
)

# =============================================================================
# FACET BLOCK - GRID
# Uses dock with upstream ggplot: dataset → ggplot → facet
# =============================================================================
cat("12/14 - facet block: grid\n")
validate_block_screenshot(
  block = new_facet_block(
    facet_type = "grid",
    rows = "cyl",
    cols = "gear"
  ),
  data = mtcars,
  filename = "block-facet-grid.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  upstream_ggplot = list(type = "point", x = "wt", y = "mpg"),
  verbose = FALSE
)

# =============================================================================
# THEME BLOCK
# Uses dock with upstream ggplot: dataset → ggplot → theme
# =============================================================================
cat("13/14 - theme block\n")
validate_block_screenshot(
  block = new_theme_block(
    base_theme = "economist",
    panel_bg = "#f0f8ff",      # Light blue panel background
    plot_bg = "#ffffff",       # White plot background
    legend_position = "bottom"
  ),
  data = mtcars,
  filename = "block-theme.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  expand_advanced = TRUE,  # Show advanced options
  upstream_ggplot = list(type = "point", x = "wt", y = "mpg", color = "cyl"),
  verbose = FALSE
)

# =============================================================================
# GRID BLOCK - Multi-plot composition
# Uses dock with multiple upstream ggplots: dataset → ggplot1, ggplot2 → grid
# =============================================================================
cat("14/14 - grid block\n")
validate_block_screenshot(
  block = new_grid_block(
    layout = "horizontal",
    n_plots = 2
  ),
  data = mtcars,
  filename = "block-grid.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  upstream_ggplots = list(
    list(type = "point", x = "wt", y = "mpg", color = "cyl"),
    list(type = "boxplot", x = "cyl", y = "mpg", fill = "cyl")
  ),
  verbose = FALSE
)

cat("\n✓ All screenshots generated successfully!\n")
cat("Screenshots saved to: man/figures/\n\n")
