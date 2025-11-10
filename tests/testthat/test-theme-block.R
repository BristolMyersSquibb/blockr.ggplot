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

# Palette function tests
test_that("build_palette_choices always includes viridis palettes", {
  discrete_choices <- build_palette_choices("discrete")
  continuous_choices <- build_palette_choices("continuous")

  # Check that (none) is present
  expect_equal(discrete_choices[["(none)"]], "none")
  expect_equal(continuous_choices[["(none)"]], "none")

  # Check that viridis palettes are present
  viridis_discrete <- discrete_choices[["viridis (Built-in)"]]
  expect_true("viridis" %in% viridis_discrete)
  expect_true("magma" %in% viridis_discrete)
  expect_true("inferno" %in% viridis_discrete)
  expect_true("plasma" %in% viridis_discrete)

  viridis_continuous <- continuous_choices[["viridis (Built-in)"]]
  expect_true("viridis_c" %in% viridis_continuous)
  expect_true("magma_c" %in% viridis_continuous)
  expect_true("inferno_c" %in% viridis_continuous)
  expect_true("plasma_c" %in% viridis_continuous)
})

test_that("build_palette_choices only includes ggokabeito when available", {
  discrete_choices <- build_palette_choices("discrete")

  has_ggokabeito <- requireNamespace("ggokabeito", quietly = TRUE)
  has_ggokabeito_section <-
    "ggokabeito (Colorblind-friendly)" %in% names(discrete_choices)

  # Should match package availability
  expect_equal(has_ggokabeito, has_ggokabeito_section)
})

test_that("build_palette_choices includes ggokabeito only for discrete", {
  discrete_choices <- build_palette_choices("discrete")
  continuous_choices <- build_palette_choices("continuous")

  # ggokabeito should never be in continuous choices
  has_ggokabeito_continuous <-
    "ggokabeito (Colorblind-friendly)" %in% names(continuous_choices)
  expect_false(has_ggokabeito_continuous)
})

test_that("build_palette_choices only includes wesanderson when available", {
  discrete_choices <- build_palette_choices("discrete")
  continuous_choices <- build_palette_choices("continuous")

  has_wesanderson <- requireNamespace("wesanderson", quietly = TRUE)
  has_wes_discrete <-
    "wesanderson (Movie-inspired)" %in% names(discrete_choices)
  has_wes_continuous <-
    "wesanderson (Movie-inspired)" %in% names(continuous_choices)

  # Should match package availability
  expect_equal(has_wesanderson, has_wes_discrete)
  expect_equal(has_wesanderson, has_wes_continuous)
})

test_that("get_palette_function returns empty string for none", {
  result <- get_palette_function("none", "fill")
  expect_equal(result, "")

  result <- get_palette_function("(none)", "colour")
  expect_equal(result, "")
})

test_that("get_palette_function returns correct viridis discrete palettes", {
  result <- get_palette_function("viridis", "fill")
  expect_equal(result, 'ggplot2::scale_fill_viridis_d(option = "viridis")')

  result <- get_palette_function("magma", "colour")
  expect_equal(result, 'ggplot2::scale_colour_viridis_d(option = "magma")')

  result <- get_palette_function("inferno", "fill")
  expect_equal(result, 'ggplot2::scale_fill_viridis_d(option = "inferno")')
})

test_that("get_palette_function returns correct viridis continuous palettes", {
  result <- get_palette_function("viridis_c", "fill")
  expect_equal(result, 'ggplot2::scale_fill_viridis_c(option = "viridis")')

  result <- get_palette_function("magma_c", "colour")
  expect_equal(result, 'ggplot2::scale_colour_viridis_c(option = "magma")')

  result <- get_palette_function("plasma_c", "fill")
  expect_equal(result, 'ggplot2::scale_fill_viridis_c(option = "plasma")')
})

test_that("get_palette_function handles ggokabeito appropriately", {
  has_ggokabeito <- requireNamespace("ggokabeito", quietly = TRUE)

  result_fill <- get_palette_function("okabe_ito", "fill")
  result_colour <- get_palette_function("okabe_ito", "colour")

  if (has_ggokabeito) {
    expect_equal(result_fill, "ggokabeito::scale_fill_okabe_ito()")
    expect_equal(result_colour, "ggokabeito::scale_colour_okabe_ito()")
  } else {
    # Should fallback to viridis if ggokabeito not available
    expect_equal(
      result_fill,
      'ggplot2::scale_fill_viridis_d(option = "viridis")'
    )
    expect_equal(
      result_colour,
      'ggplot2::scale_colour_viridis_d(option = "viridis")'
    )
  }
})

test_that("get_palette_function handles wesanderson discrete appropriately", {
  has_wesanderson <- requireNamespace("wesanderson", quietly = TRUE)

  result_fill <- get_palette_function("wes_royal1", "fill")
  result_colour <- get_palette_function("wes_zissou1", "colour")

  if (has_wesanderson) {
    expect_match(
      result_fill,
      "ggplot2::scale_fill_manual\\(values = wesanderson::wes_palette"
    )
    expect_match(
      result_colour,
      "ggplot2::scale_colour_manual\\(values = wesanderson::wes_palette"
    )
  } else {
    # Should fallback to viridis if wesanderson not available
    expect_equal(
      result_fill,
      'ggplot2::scale_fill_viridis_d(option = "viridis")'
    )
    expect_equal(
      result_colour,
      'ggplot2::scale_colour_viridis_d(option = "viridis")'
    )
  }
})

test_that("get_palette_function handles wesanderson continuous appropriately", {
  has_wesanderson <- requireNamespace("wesanderson", quietly = TRUE)

  result_fill <- get_palette_function("wes_royal1_c", "fill")
  result_colour <- get_palette_function("wes_zissou1_c", "colour")

  if (has_wesanderson) {
    expect_match(
      result_fill,
      "ggplot2::scale_fill_gradientn\\(colors = wesanderson::wes_palette"
    )
    expect_match(
      result_colour,
      "ggplot2::scale_colour_gradientn\\(colors = wesanderson::wes_palette"
    )
  } else {
    # Should fallback to viridis if wesanderson not available
    expect_equal(
      result_fill,
      'ggplot2::scale_fill_viridis_c(option = "viridis")'
    )
    expect_equal(
      result_colour,
      'ggplot2::scale_colour_viridis_c(option = "viridis")'
    )
  }
})

test_that("get_palette_function auto-converts palette based on scale_type", {
  # Test discrete to continuous conversion
  result <- get_palette_function("viridis", "fill", scale_type = "continuous")
  expect_equal(result, 'ggplot2::scale_fill_viridis_c(option = "viridis")')

  result <- get_palette_function("magma", "colour", scale_type = "continuous")
  expect_equal(result, 'ggplot2::scale_colour_viridis_c(option = "magma")')

  # Test continuous to discrete conversion
  result <- get_palette_function("viridis_c", "fill", scale_type = "discrete")
  expect_equal(result, 'ggplot2::scale_fill_viridis_d(option = "viridis")')

  result <- get_palette_function("plasma_c", "colour", scale_type = "discrete")
  expect_equal(result, 'ggplot2::scale_colour_viridis_d(option = "plasma")')

  # Test okabe_ito fallback for continuous
  result <- get_palette_function(
    "okabe_ito",
    "fill",
    scale_type = "continuous"
  )
  expect_equal(result, 'ggplot2::scale_fill_viridis_c(option = "viridis")')
})

test_that("detect_scale_type returns 'none' for unused aesthetics", {
  skip_if_not_installed("ggplot2")

  # Plot without fill or colour aesthetics
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  expect_equal(detect_scale_type(p, "fill"), "none")
  expect_equal(detect_scale_type(p, "colour"), "none")
})

test_that("detect_scale_type detects discrete scales", {
  skip_if_not_installed("ggplot2")

  # Plot with factor variable (discrete)
  p <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(x = wt, y = mpg, colour = as.factor(cyl))
  ) +
    ggplot2::geom_point()

  expect_equal(detect_scale_type(p, "colour"), "discrete")

  # Plot with character variable (discrete)
  mtcars_char <- mtcars
  mtcars_char$gear_char <- as.character(mtcars_char$gear)
  p2 <- ggplot2::ggplot(
    mtcars_char,
    ggplot2::aes(x = wt, y = mpg, colour = gear_char)
  ) +
    ggplot2::geom_point()

  expect_equal(detect_scale_type(p2, "colour"), "discrete")
})

test_that("detect_scale_type detects continuous scales", {
  skip_if_not_installed("ggplot2")

  # Plot with many unique numeric values (continuous)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, colour = hp)) +
    ggplot2::geom_point()

  expect_equal(detect_scale_type(p, "colour"), "continuous")

  # Plot with few unique numeric values (still continuous in ggplot2)
  # cyl has only 3 values (4, 6, 8) but it's numeric, so ggplot2 treats it
  # as continuous unless explicitly converted to factor
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, colour = cyl)) +
    ggplot2::geom_point()

  expect_equal(detect_scale_type(p2, "colour"), "continuous")
})
