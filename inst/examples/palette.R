library(blockr.core)
devtools::load_all()

# Test 1: Viridis palette (always available)
# Using magma palette for fill aesthetic
board1 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "bar",
      x = "cyl",
      fill = "gear"
    ),
    theme = new_theme_block(
      palette_fill = "magma"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Test 2: Multiple palettes - viridis inferno for fill, plasma for colour
# Note: cyl is numeric in mtcars, so palette will auto-detect and use
# continuous version (plasma_c). gear is discrete (factor).
board2 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "point",
      x = "wt",
      y = "mpg",
      color = "cyl",
      fill = "gear",
      size = "hp"
    ),
    theme = new_theme_block(
      palette_fill = "inferno",
      palette_colour = "plasma"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Test 3: No palette (preserves upstream defaults)
board3 <- new_board(
  blocks = c(
    data = new_dataset_block("mtcars", package = "datasets"),
    scatter = new_ggplot_block(
      type = "boxplot",
      x = "cyl",
      y = "mpg",
      fill = "cyl"
    ),
    theme = new_theme_block(
      base_theme = "minimal",
      palette_fill = "none"
    )
  ),
  links = c(
    new_link("data", "scatter", "data"),
    new_link("scatter", "theme", "data")
  )
)

# Test 4: ggokabeito palette (if available)
# Okabe-Ito is colorblind-friendly
if (requireNamespace("ggokabeito", quietly = TRUE)) {
  board4 <- new_board(
    blocks = c(
      data = new_dataset_block("iris", package = "datasets"),
      scatter = new_ggplot_block(
        type = "point",
        x = "Sepal.Length",
        y = "Sepal.Width",
        color = "Species"
      ),
      theme = new_theme_block(
        palette_colour = "okabe_ito"
      )
    ),
    links = c(
      new_link("data", "scatter", "data"),
      new_link("scatter", "theme", "data")
    )
  )
}

# Test 5: Wes Anderson palette (if available)
# Using Royal1 palette from wesanderson package
if (requireNamespace("wesanderson", quietly = TRUE)) {
  board5 <- new_board(
    blocks = c(
      data = new_dataset_block("iris", package = "datasets"),
      scatter = new_ggplot_block(
        type = "bar",
        x = "Species",
        fill = "Species"
      ),
      theme = new_theme_block(
        base_theme = "minimal",
        palette_fill = "wes_royal1"
      )
    ),
    links = c(
      new_link("data", "scatter", "data"),
      new_link("scatter", "theme", "data")
    )
  )
}

# Launch board1 to test viridis magma palette
blockr.core::serve(board1)
