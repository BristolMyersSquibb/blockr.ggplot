library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Create a simple visualization with custom theme
board <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    theme = new_theme_block(
      plot_bg = "#FFF8DC", # Yellowish background (cornsilk)
      show_minor_grid = FALSE
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

blockr.core::serve(board)
