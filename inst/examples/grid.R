library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Create a dashboard with three charts combined in a grid
board <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    histogram = new_ggplot_block(type = "histogram", x = "mpg", bins = 15),
    grid = new_grid_block()
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("data", "histogram", "data"),
    new_link("scatter", "grid", "1"),
    new_link("histogram", "grid", "2")
  )
)

blockr.core::serve(board)
