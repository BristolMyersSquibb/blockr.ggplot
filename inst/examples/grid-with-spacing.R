library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Create a dashboard demonstrating spacing controls
# - panel_spacing in facet block controls gap between facet panels
# - spacing in grid block controls gap between plots

board <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),

    # First plot: faceted scatter plot with increased panel spacing
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "factor(cyl)"
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      ncol = "3",
      panel_spacing = "2"  # Increase gap between facet panels
    ),

    # Second plot: histogram
    histogram = new_ggplot_block(
      type = "histogram",
      x = "mpg",
      bins = 15
    ),

    # Combine in grid with spacing between plots
    grid = new_grid_block(
      ncol = "2",
      title = "mtcars Analysis with Custom Spacing",
      spacing = "20"  # 20pt gap between plots (each plot gets 10pt margin)
    )
  ),
  links = c(
    # Data flows to both plots
    new_link("data", "scatter", "data"),
    new_link("data", "histogram", "data"),

    # Apply faceting to scatter plot
    new_link("scatter", "facet", "data"),

    # Add both plots to grid
    new_link("facet", "grid", "1"),      # Faceted scatter
    new_link("histogram", "grid", "2")   # Histogram
  )
)

blockr.core::serve(board)
