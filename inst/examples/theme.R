library(blockr.core)
library(blockr.ggplot)
devtools::load_all()

# Test 1: Gray theme preset
# NEW: UI shows Base Theme and Legend Position prominently
# Advanced options (colors, typography, grid) are collapsible
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

# Test 2: Theme with minimal defaults (can switch to 20+ themes in UI)
# NEW THEMES: ggthemes (Economist, FiveThirtyEight, Tufte, WSJ)
#             cowplot (Publication-ready themes)
#             hrbrthemes (Typography-focused)
#             ggpubr (Scientific publication)
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

# Test 3: The Economist theme (requires ggthemes package)
if (requireNamespace("ggthemes", quietly = TRUE)) {
  board3 <- new_board(
    blocks = c(
      data = new_dataset_block("mtcars", package = "datasets"),
      scatter = new_ggplot_block(
        type = "point",
        x = "wt",
        y = "mpg",
        color = "cyl"
      ),
      theme = new_theme_block(
        base_theme = "economist"
      )
    ),
    links = c(
      new_link("data", "scatter", "data"),
      new_link("scatter", "theme", "data")
    )
  )
}

# Launch board1 to test gray theme with new collapsible UI
blockr.core::serve(board1)
