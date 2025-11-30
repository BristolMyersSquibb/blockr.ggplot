# blockr.ggplot Example Workflow
#
# Data -> Plot -> Theme pipeline
# Run with: source("dev/examples/workflow.R")

library(blockr.core)
library(blockr.dock)
library(blockr.dplyr)

devtools::load_all(".")

# Serve scatter plot with theme customization
serve(
  new_dock_board(
    blocks = c(
      # Load data
      data = new_dataset_block("mtcars", package = "datasets"),
      # Create scatter plot
      plot = new_ggplot_block(
        type = "point",
        x = "wt",
        y = "mpg",
        color = "cyl"
      ),
      # Apply theme
      styled = new_theme_block(
        base_theme = "minimal",
        legend_position = "bottom"
      )
    ),
    links = list(
      from = c("data", "plot"),
      to = c("plot", "styled"),
      input = c("data", "data")
    )
  )
)
