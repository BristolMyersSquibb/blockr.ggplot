#!/usr/bin/env Rscript

# Test script for facet block screenshot
Sys.setenv(NOT_CRAN = "true")

devtools::load_all(".")
source("dev/screenshots/validate-screenshot.R")

cat("Testing facet block with ggplot as input...\n")

# Create a ggplot object first
library(ggplot2)
plot_obj <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  theme_minimal()

result <- validate_block_screenshot(
  block = new_facet_block(
    facet_type = "wrap",
    facets = "cyl",
    ncol = 3
  ),
  data = list(data = plot_obj),  # Pass the ggplot as data
  filename = "test-facet.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 5,
  verbose = TRUE
)

if (result$success) {
  cat("\n✓ Screenshot created successfully!\n")
  cat("Path:", result$path, "\n")
  cat("\nPlease check if it shows a faceted plot.\n")
} else {
  cat("\n✗ Failed:", result$error, "\n")
}
