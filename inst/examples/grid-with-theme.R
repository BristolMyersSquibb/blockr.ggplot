library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Create a dashboard with plots that have custom spacing via theme block
# Demonstrates:
# - panel_spacing for faceted plots
# - plot_margin for spacing between plots in the grid

board <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),

    # First plot: faceted scatter plot
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "factor(cyl)"
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      ncol = "3"
    ),

    # Second plot: histogram
    histogram = new_ggplot_block(
      type = "histogram",
      x = "mpg",
      bins = 15
    ),

    # Theme block to control spacing
    # - panel_spacing controls gap between facet panels (in the scatter plot)
    # - plot_margin controls outer margins (affects spacing in the grid)
    theme = new_theme_block(
      base_theme = "minimal",
      panel_spacing = "2",           # Increase gap between facet panels
      plot_margin_top = "15",         # Add top margin
      plot_margin_right = "15",       # Add right margin
      plot_margin_bottom = "15",      # Add bottom margin
      plot_margin_left = "15"         # Add left margin
    ),

    # Combine in grid
    grid = new_grid_block(
      ncol = "2",
      title = "mtcars Analysis with Custom Spacing"
    )
  ),
  links = c(
    # Data flows to both plots
    new_link("data", "scatter", "data"),
    new_link("data", "histogram", "data"),

    # Apply faceting to scatter plot
    new_link("scatter", "facet", "data"),

    # Apply theme to faceted plot
    new_link("facet", "theme", "data"),

    # Add both plots to grid
    new_link("theme", "grid", "1"),      # Faceted scatter (with spacing)
    new_link("histogram", "grid", "2")   # Histogram
  )
)

blockr.core::serve(board)
