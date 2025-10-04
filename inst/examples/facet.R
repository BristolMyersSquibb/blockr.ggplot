library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Example 1: facet_wrap - Split scatter plot by cylinder count
# Visual preview shows estimated 3 facets in auto layout
board1 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      ncol = "2",  # 2 columns
      scales = "free_y"  # Free Y-axis scales per facet
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "facet", "data")
  )
)

# Example 2: facet_wrap with multiple variables
# Shows interaction between transmission type and cylinder count
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
      facet_type = "wrap",
      facets = c("cyl", "am"),  # Multiple facet variables
      ncol = "3",
      scales = "free",
      labeller = "label_both"  # Show variable names and values
    )
  ),
  links = c(
    new_link("data", "boxplot", "data"),
    new_link("boxplot", "facet", "data")
  )
)

# Example 3: facet_grid - Rows and columns layout
# Create a matrix of plots: cylinders (rows) × gears (columns)
board3 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "hp"
    ),
    facet = new_facet_block(
      facet_type = "grid",
      rows = "cyl",      # Cylinders in rows
      cols = "gear",     # Gears in columns
      scales = "free_y"  # Free Y scales
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "facet", "data")
  )
)

# Example 4: Histogram with facet_wrap and vertical direction
board4 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    histogram = new_ggplot_block(
      type = "histogram",
      x = "mpg",
      fill = "cyl",
      bins = 20
    ),
    facet = new_facet_block(
      facet_type = "wrap",
      facets = "cyl",
      nrow = "3",      # 3 rows (vertical stacking)
      dir = "v",       # Vertical direction
      scales = "free"
    )
  ),
  links = c(
    new_link("data", "histogram", "data"),
    new_link("histogram", "facet", "data")
  )
)

# Example 5: facet_grid with only rows (columns = .)
board5 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    density = new_ggplot_block(
      type = "density",
      x = "mpg",
      fill = "cyl"
    ),
    facet = new_facet_block(
      facet_type = "grid",
      rows = "cyl",     # Only row facets
      cols = character(),  # No column facets
      scales = "free_y",
      space = "free_y"  # Adjust panel heights based on data
    )
  ),
  links = c(
    new_link("data", "density", "data"),
    new_link("density", "facet", "data")
  )
)

# Launch board1 to see facet_wrap with visual preview
blockr.core::serve(board1)

# Uncomment to try other examples:
# blockr.core::serve(board2)  # Multiple facet variables
# blockr.core::serve(board3)  # facet_grid rows × columns
# blockr.core::serve(board4)  # Vertical facet_wrap
# blockr.core::serve(board5)  # facet_grid rows only