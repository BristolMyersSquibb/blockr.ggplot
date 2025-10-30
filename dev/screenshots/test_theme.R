#!/usr/bin/env Rscript

# Test script for theme block with Economist theme
Sys.setenv(NOT_CRAN = "true")

devtools::load_all(".")
source("dev/screenshots/validate-screenshot.R")

cat("Testing theme block with Economist theme and advanced options expanded...\n")

# Create a ggplot object as input for theme block
plot_for_theme <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(cyl))) +
  ggplot2::geom_point(size = 3) +
  ggplot2::labs(title = "MPG vs Weight", color = "Cylinders")

result <- validate_block_screenshot(
  block = new_theme_block(
    theme_name = "theme_economist"
  ),
  data = list(data = plot_for_theme),
  filename = "test-theme.png",
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 5,
  expand_advanced = TRUE,  # Expand advanced options
  verbose = TRUE
)

if (result$success) {
  cat("\n✓ Screenshot created successfully!\n")
  cat("Path:", result$path, "\n")
  cat("\nPlease check if it shows Economist theme with advanced options expanded.\n")
} else {
  cat("\n✗ Failed:", result$error, "\n")
}
