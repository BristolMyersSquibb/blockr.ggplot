test_that("build_theme_choices always includes ggplot2 themes", {
  choices <- build_theme_choices()

  # Check that Auto is present
  expect_equal(choices[["Auto (keep upstream)"]], "auto")

  # Check that ggplot2 themes are present
  ggplot2_themes <- choices[["ggplot2 (Built-in)"]]
  expect_true("minimal" %in% ggplot2_themes)
  expect_true("classic" %in% ggplot2_themes)
  expect_true("gray" %in% ggplot2_themes)
  expect_true("bw" %in% ggplot2_themes)
})

test_that("build_theme_choices only includes cowplot when available", {
  choices <- build_theme_choices()

  has_cowplot <- requireNamespace("cowplot", quietly = TRUE)
  has_cowplot_section <- "cowplot (Publication)" %in% names(choices)

  # Should match package availability
  expect_equal(has_cowplot, has_cowplot_section)
})

test_that("build_theme_choices only includes ggthemes when available", {
  choices <- build_theme_choices()

  has_ggthemes <- requireNamespace("ggthemes", quietly = TRUE)
  has_ggthemes_section <- "ggthemes (Publications)" %in% names(choices)

  # Should match package availability
  expect_equal(has_ggthemes, has_ggthemes_section)
})

test_that("build_theme_choices only includes ggpubr when available", {
  choices <- build_theme_choices()

  has_ggpubr <- requireNamespace("ggpubr", quietly = TRUE)
  has_ggpubr_section <- "ggpubr (Scientific)" %in% names(choices)

  # Should match package availability
  expect_equal(has_ggpubr, has_ggpubr_section)
})

test_that("get_theme_function returns empty string for auto", {
  result <- get_theme_function("auto")
  expect_equal(result, "")
})

test_that("get_theme_function returns correct ggplot2 themes", {
  expect_equal(get_theme_function("minimal"), "ggplot2::theme_minimal()")
  expect_equal(get_theme_function("classic"), "ggplot2::theme_classic()")
  expect_equal(get_theme_function("gray"), "ggplot2::theme_gray()")
  expect_equal(get_theme_function("bw"), "ggplot2::theme_bw()")
})

test_that("get_theme_function returns fallback for unavailable packages", {
  # Test with a theme that requires an unavailable package
  # We can't guarantee any specific package is unavailable, but we can
  # test that the function returns a valid theme string

  result <- get_theme_function("unknown_theme")
  expect_equal(result, "ggplot2::theme_minimal()")
})

test_that("get_theme_function handles cowplot themes appropriately", {
  has_cowplot <- requireNamespace("cowplot", quietly = TRUE)
  result <- get_theme_function("cowplot")

  if (has_cowplot) {
    expect_equal(result, "cowplot::theme_cowplot()")
  } else {
    # Should fallback to minimal if cowplot not available
    expect_equal(result, "ggplot2::theme_minimal()")
  }
})

test_that("get_theme_function handles ggthemes appropriately", {
  has_ggthemes <- requireNamespace("ggthemes", quietly = TRUE)
  result <- get_theme_function("economist")

  if (has_ggthemes) {
    expect_equal(result, "ggthemes::theme_economist()")
  } else {
    # Should fallback to minimal if ggthemes not available
    expect_equal(result, "ggplot2::theme_minimal()")
  }
})

test_that("get_theme_function handles ggpubr themes appropriately", {
  has_ggpubr <- requireNamespace("ggpubr", quietly = TRUE)
  result <- get_theme_function("pubr")

  if (has_ggpubr) {
    expect_equal(result, "ggpubr::theme_pubr()")
  } else {
    # Should fallback to minimal if ggpubr not available
    expect_equal(result, "ggplot2::theme_minimal()")
  }
})

# =============================================================================
# Palette tests - testServer
# =============================================================================

test_that("theme block with categorical fill palette - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create a ggplot with categorical fill (Species is a factor)
  input_plot <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
  ) +
    ggplot2::geom_boxplot()

  block <- new_theme_block(
    palette_fill = "viridis_d"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Verify it renders without error
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() input_plot))
  )
})

test_that("theme block with continuous colour palette - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create a ggplot with continuous colour (cyl is numeric)
  input_plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = wt, y = mpg, colour = cyl)
  ) +
    ggplot2::geom_point()

  block <- new_theme_block(
    palette_colour = "viridis_c"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Verify it renders without error
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() input_plot))
  )
})

test_that("theme block with both fill and colour palettes - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create a ggplot with categorical fill and continuous colour
  input_plot <- ggplot2::ggplot(
    iris,
    ggplot2::aes(
      x = Sepal.Length,
      y = Sepal.Width,
      fill = Species,
      colour = Petal.Length
    )
  ) +
    ggplot2::geom_point(shape = 21, size = 3)

  block <- new_theme_block(
    palette_fill = "magma_d",
    palette_colour = "plasma_c"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Verify it renders without error (both palettes applied)
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() input_plot))
  )
})

test_that("theme block with auto palette keeps upstream - testServer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # Create a ggplot with default colours
  input_plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = wt, y = mpg, colour = factor(cyl))
  ) +
    ggplot2::geom_point()

  block <- new_theme_block(
    palette_colour = "auto"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(inherits(result, "ggplot"))
      # Verify it renders without error (uses ggplot2 defaults)
      expect_no_error(ggplot2::ggplot_build(result))
    },
    args = list(x = block, data = list(data = function() input_plot))
  )
})
