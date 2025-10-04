library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Test 1: Gray theme preset
board1 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    theme = new_theme_block(
      base_theme = "gray" # All other options use gray theme defaults
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Test 2: Theme with minimal defaults (can switch in UI)
board2 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    theme = new_theme_block()
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Launch board1 to test gray theme
blockr.core::serve(board1)
