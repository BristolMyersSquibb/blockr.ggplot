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
  width = 800,
  height = 600,
  delay = 2,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - BAR
# =============================================================================
cat("2/14 - ggplot block: bar\n")
# Convert cyl and gear to factors for proper bar chart display
mtcars_for_bar <- mtcars
mtcars_for_bar$cyl <- factor(mtcars_for_bar$cyl)
mtcars_for_bar$gear <- factor(mtcars_for_bar$gear)

validate_block_screenshot(
  block = new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear"
  ),
  data = mtcars_for_bar,
  filename = "block-ggplot-bar.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
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
  width = 800,
  height = 600,
  delay = 2,
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
  width = 800,
  height = 600,
  delay = 2,
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
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - DENSITY
# =============================================================================
cat("6/14 - ggplot block: density\n")
# Convert cyl to factor for proper grouping
mtcars_density <- mtcars
mtcars_density$cyl <- factor(mtcars_density$cyl)

validate_block_screenshot(
  block = new_ggplot_block(
    type = "density",
    x = "mpg",
    fill = "cyl"
  ),
  data = mtcars_density,
  filename = "block-ggplot-density.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
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
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - HISTOGRAM
# =============================================================================
cat("8/14 - ggplot block: histogram\n")
# Convert cyl to factor for fill aesthetic
mtcars_hist <- mtcars
mtcars_hist$cyl <- factor(mtcars_hist$cyl)

validate_block_screenshot(
  block = new_ggplot_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl"
  ),
  data = mtcars_hist,
  filename = "block-ggplot-histogram.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - PIE
# =============================================================================
cat("9/14 - ggplot block: pie\n")
# Count cylinders for pie chart - use aggregated data
cyl_counts <- aggregate(mpg ~ cyl, data = mtcars, FUN = length)
names(cyl_counts) <- c("Cylinders", "Count")
cyl_counts$Cylinders <- factor(cyl_counts$Cylinders, labels = c("4 cyl", "6 cyl", "8 cyl"))

validate_block_screenshot(
  block = new_ggplot_block(
    type = "pie",
    x = "Cylinders",
    y = "Count",
    fill = "Cylinders"
  ),
  data = cyl_counts,
  filename = "block-ggplot-pie.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# GGPLOT BLOCK - DONUT
# =============================================================================
cat("10/14 - ggplot block: donut\n")
validate_block_screenshot(
  block = new_ggplot_block(
    type = "pie",
    x = "Cylinders",
    y = "Count",
    fill = "Cylinders",
    donut_hole = 0.5
  ),
  data = cyl_counts,
  filename = "block-ggplot-donut.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# FACET BLOCK - WRAP
# =============================================================================
cat("11/14 - facet block: wrap\n")
# Create a ggplot object as input for facet block
plot_for_facet <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(cyl))) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_minimal()

validate_block_screenshot(
  block = new_facet_block(
    facet_type = "wrap",
    facets = "cyl",
    ncol = 3
  ),
  data = list(data = plot_for_facet),
  filename = "block-facet.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# FACET BLOCK - GRID
# =============================================================================
cat("12/14 - facet block: grid\n")
# Create a ggplot object as input for facet grid
plot_for_grid <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::theme_minimal()

validate_block_screenshot(
  block = new_facet_block(
    facet_type = "grid",
    rows = "cyl",
    cols = "gear"
  ),
  data = list(data = plot_for_grid),
  filename = "block-facet-grid.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

# =============================================================================
# THEME BLOCK
# =============================================================================
cat("13/14 - theme block\n")
# Create a ggplot object as input for theme block
plot_for_theme <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(cyl))) +
  ggplot2::geom_point(size = 3) +
  ggplot2::labs(title = "MPG vs Weight", color = "Cylinders")

validate_block_screenshot(
  block = new_theme_block(
    base_theme = "economist"
  ),
  data = list(data = plot_for_theme),
  filename = "block-theme.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  expand_advanced = TRUE,  # Show advanced options
  verbose = FALSE
)

# =============================================================================
# GRID BLOCK - Multi-plot composition
# =============================================================================
cat("14/14 - grid block\n")
# Create multiple ggplot objects as input for grid block
plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
  ggplot2::geom_point(color = "steelblue", size = 2) +
  ggplot2::theme_minimal()

plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
  ggplot2::geom_boxplot(fill = "coral") +
  ggplot2::theme_minimal()

validate_block_screenshot(
  block = new_grid_block(
    layout = "horizontal",
    n_plots = 2
  ),
  data = list(`1` = plot1, `2` = plot2),
  filename = "block-grid.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 3,
  verbose = FALSE
)

cat("\n✓ All screenshots generated successfully!\n")
cat("Screenshots saved to: man/figures/\n\n")
