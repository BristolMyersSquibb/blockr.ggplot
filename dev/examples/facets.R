# blockr.ggplot Faceted Plot Example
#
# Data -> Plot -> Facet pipeline
# Run with: source("dev/examples/facets.R")

library(blockr.core)
library(blockr.dock)

devtools::load_all(".")

# Serve boxplot with facet_wrap
serve(
  new_dock_board(
    blocks = c(
      # Load data
      data = new_dataset_block("iris", package = "datasets"),
      # Create boxplot
      plot = new_ggplot_block(
        type = "boxplot",
        x = "Species",
        y = "Sepal.Length",
        fill = "Species"
      ),
      # Split by Sepal width categories
      faceted = new_facet_block(
        facet_type = "wrap",
        facets = "Species",
        ncol = "3",
        scales = "free"
      ),
      # Apply minimal theme
      styled = new_theme_block(
        base_theme = "minimal",
        palette_fill = "viridis"
      )
    ),
    links = list(
      from = c("data", "plot", "faceted"),
      to = c("plot", "faceted", "styled"),
      input = c("data", "data", "data")
    )
  )
)
