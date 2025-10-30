#!/usr/bin/env Rscript

# Test script for bar chart screenshot
Sys.setenv(NOT_CRAN = "true")

devtools::load_all(".")
source("dev/screenshots/validate-screenshot.R")

cat("Testing bar chart...\n")

# Convert cyl and gear to factors
mtcars_bar <- mtcars
mtcars_bar$cyl <- factor(mtcars_bar$cyl)
mtcars_bar$gear <- factor(mtcars_bar$gear)

result <- validate_block_screenshot(
  block = new_ggplot_block(
    type = "bar",
    x = "cyl",
    fill = "gear"
  ),
  data = mtcars_bar,
  filename = "test-bar.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 5,
  verbose = TRUE
)

if (result$success) {
  cat("\n✓ Screenshot created successfully!\n")
  cat("Path:", result$path, "\n")
  cat("\nPlease check the screenshot to verify it shows a BAR chart (not scatter).\n")
} else {
  cat("\n✗ Failed:", result$error, "\n")
}
