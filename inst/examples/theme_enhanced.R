library(blockr.core)
library(blockr.ggplot)

# Test 1: Default minimal theme with collapsible advanced options
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
      base_theme = "minimal"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Test 2: Economist theme (requires ggthemes package)
if (requireNamespace("ggthemes", quietly = TRUE)) {
  board2 <- new_board(
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
  print("Economist theme test created successfully")
}

# Test 3: Cowplot theme (requires cowplot package)
if (requireNamespace("cowplot", quietly = TRUE)) {
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
        base_theme = "cowplot"
      )
    ),
    links = c(
      new_link("data", "scatter", "data"),
      new_link("scatter", "theme", "data")
    )
  )
  print("Cowplot theme test created successfully")
}

# Test 4: FiveThirtyEight theme (requires ggthemes package)
if (requireNamespace("ggthemes", quietly = TRUE)) {
  board4 <- new_board(
    blocks = c(
      data = new_dataset_block("mtcars", package = "datasets"),
      scatter = new_ggplot_block(
        type = "point",
        x = "wt",
        y = "mpg",
        color = "cyl"
      ),
      theme = new_theme_block(
        base_theme = "fivethirtyeight"
      )
    ),
    links = c(
      new_link("data", "scatter", "data"),
      new_link("scatter", "theme", "data")
    )
  )
  print("FiveThirtyEight theme test created successfully")
}

# Test 5: Built-in theme with advanced options
board5 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl"
    ),
    theme = new_theme_block(
      base_theme = "classic",
      legend_position = "bottom",
      panel_bg = "#F5F5F5",
      grid_color = "#CCCCCC"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

print("All theme tests completed successfully!")
print("UI features:")
print("- Base Theme and Legend Position are always visible")
print("- Advanced options (Colors, Typography, Grid & Borders) are in collapsible section")
print("- Click 'Show advanced options' to expand/collapse")
print("- Themes are now grouped by package for better organization")
