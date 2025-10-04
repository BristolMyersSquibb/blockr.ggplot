library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Comprehensive example: Combining facet and theme blocks
# Shows how to create publication-ready faceted visualizations

# Example 1: Scatter plot with facets and custom theme
board1 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "hp",
      size = "hp"
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      ncol = "3",
      scales = "free",
      labeller = "label_both"
    ),
    theme = new_theme_block(
      base_theme = "minimal",
      legend_position = "bottom"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "facet", "data"),
    new_link("facet", "theme", "data")
  )
)

# Example 2: Grid faceting with publication theme
# Ideal for scientific papers
if (requireNamespace("ggpubr", quietly = TRUE)) {
  board2 <- new_board(
    blocks = c(
      data = new_dataset_block("mtcars", package = "datasets"),
      boxplot = new_ggplot_block(
        type = "boxplot",
        x = "gear",
        y = "mpg",
        fill = "gear"
      ),
      facet = new_facet_block(
        facet_type = "grid",
        rows = "cyl",
        cols = "am",
        scales = "free_y",
        labeller = "label_both"
      ),
      theme = new_theme_block(
        base_theme = "pubr", # Publication-ready theme
        legend_position = "right"
      )
    ),
    links = c(
      new_link("data", "boxplot", "data"),
      new_link("boxplot", "facet", "data"),
      new_link("facet", "theme", "data")
    )
  )
}

# Example 3: Histogram with vertical facets and classic theme
board3 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    histogram = new_ggplot_block(
      type = "histogram",
      x = "mpg",
      fill = "cyl",
      bins = 15
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      nrow = "3",
      dir = "v",
      scales = "free"
    ),
    theme = new_theme_block(
      base_theme = "classic",
      legend_position = "none" # Remove legend (facets show the groups)
    )
  ),
  links = c(
    new_link("data", "histogram", "data"),
    new_link("histogram", "facet", "data"),
    new_link("facet", "theme", "data")
  )
)

# Launch board1
blockr.core::serve(board1)

# Uncomment to try other examples:
# blockr.core::serve(board2)  # Publication-ready grid facets
# blockr.core::serve(board3)  # Vertical facets with classic theme
