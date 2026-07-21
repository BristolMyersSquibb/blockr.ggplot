# Golden characterization of the exact expression each block emits.
#
# These lock down the *generated code* (the deparsed `expr()`), byte for byte,
# across every branch of the ggplot / facet / theme blocks. They exist so the
# "drop parse(text = glue::glue()) in favour of real AST construction" refactor
# can be verified to change nothing an end user would ever see. If you
# deliberately change what a block emits, update the expected string here and
# say so in the commit.
#
# Comparison is on a width-independent single-line deparse so it is immune to
# deparse()'s line wrapping, not to the actual call structure.

norm_expr <- function(x) {
  s <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  gsub("\\s+", " ", trimws(s))
}

# Drive a real block server and return the normalised deparse of its `expr`.
capture_expr <- function(block, data = mtcars) {
  out <- NULL
  suppressWarnings(suppressMessages(
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()
        out <<- norm_expr(session$returned$expr())
      },
      args = list(x = block, data = list(data = function() data))
    )
  ))
  out
}

nonsyntactic_df <- function() {
  data.frame(
    a = 1:6,
    b = rnorm(6),
    `2025 Sales` = letters[1:6],
    grp = rep(c("x", "y"), 3),
    check.names = FALSE
  )
}

# ---------------------------------------------------------------------------
# ggplot block
# ---------------------------------------------------------------------------

test_that("ggplot_block emits the expected expression across chart types", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  nsy <- nonsyntactic_df()

  cases <- list(
    list(
      new_ggplot_block("point", x = "cyl"), mtcars,
      "ggplot2::ggplot() + ggplot2::geom_blank()"
    ),
    list(
      new_ggplot_block("point", x = "wt", y = "mpg"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("point", x = "wt", y = "mpg", color = "cyl", fill = "gear", size = "hp", shape = "am", alpha = "drat"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = wt, y = mpg, colour = cyl, fill = gear, size = hp, shape = as.factor(am), alpha = drat)) + ggplot2::geom_point() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("line", x = "wt", y = "mpg", group = "cyl", linetype = "am", color = "gear"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = wt, y = mpg, colour = gear, linetype = as.factor(am), group = cyl)) + ggplot2::geom_line() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("bar", x = "cyl"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = cyl)) + ggplot2::geom_bar(position = \"stack\") + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("bar", x = "cyl", y = "mpg"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = cyl, y = mpg)) + ggplot2::geom_col(position = \"stack\") + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("bar", x = "cyl", fill = "gear", position = "dodge"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = cyl, fill = as.factor(gear))) + ggplot2::geom_bar(position = \"dodge\") + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("histogram", x = "mpg", bins = 20, fill = "cyl", position = "identity"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = mpg, fill = as.factor(cyl))) + ggplot2::geom_histogram(bins = 20, position = \"identity\") + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("boxplot", x = "cyl", y = "mpg", fill = "am"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = cyl, y = mpg, fill = as.factor(am))) + ggplot2::geom_boxplot() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("violin", x = "cyl", y = "mpg"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = cyl, y = mpg)) + ggplot2::geom_violin() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("density", x = "mpg", fill = "cyl", density_alpha = 0.5), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = mpg, fill = as.factor(cyl), group = as.factor(cyl))) + ggplot2::geom_density(alpha = 0.5) + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("area", x = "wt", y = "mpg", fill = "cyl"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = wt, y = mpg, fill = cyl)) + ggplot2::geom_area() + ggplot2::theme_minimal()"
    ),
    list(
      new_ggplot_block("pie", x = "cyl"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = \"\", fill = as.factor(cyl))) + ggplot2::geom_bar(width = 1) + ggplot2::coord_polar(\"y\", start = 0) + ggplot2::theme_minimal() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())"
    ),
    list(
      new_ggplot_block("pie", x = "cyl", donut = TRUE), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = 2, fill = as.factor(cyl))) + ggplot2::geom_bar(width = 1) + ggplot2::coord_polar(\"y\", start = 0) + ggplot2::theme_minimal() + ggplot2::xlim(c(0.2, 2.5)) + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())"
    ),
    list(
      new_ggplot_block("pie", x = "cyl", fill = "gear"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = \"\", fill = as.factor(gear))) + ggplot2::geom_bar(width = 1) + ggplot2::coord_polar(\"y\", start = 0) + ggplot2::theme_minimal() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())"
    ),
    list(
      new_ggplot_block("pie", x = "cyl", y = "mpg"), mtcars,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = \"\", y = mpg, fill = as.factor(cyl))) + ggplot2::geom_col(width = 1) + ggplot2::coord_polar(\"y\", start = 0) + ggplot2::theme_minimal() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())"
    ),
    list(
      new_ggplot_block("point", x = "2025 Sales", y = "b"), nsy,
      "ggplot2::ggplot(.(data), ggplot2::aes(x = `2025 Sales`, y = b)) + ggplot2::geom_point() + ggplot2::theme_minimal()"
    )
  )

  for (case in cases) {
    expect_equal(capture_expr(case[[1]], case[[2]]), case[[3]])
  }
})

# ---------------------------------------------------------------------------
# facet block
# ---------------------------------------------------------------------------

test_that("facet_block emits the expected expression for wrap and grid", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  nsy <- nonsyntactic_df()

  cases <- list(
    list(new_facet_block("wrap"), nsy, ".(data)"),
    list(
      new_facet_block("wrap", facets = "cyl"), mtcars,
      ".(data) + ggplot2::facet_wrap(~cyl, scales = \"fixed\", labeller = \"label_value\")"
    ),
    list(
      new_facet_block("wrap", facets = c("cyl", "am"), ncol = "2", scales = "free"), mtcars,
      ".(data) + ggplot2::facet_wrap(~cyl + am, ncol = 2, scales = \"free\", labeller = \"label_value\")"
    ),
    list(
      new_facet_block("wrap", facets = "cyl", dir = "v", labeller = "label_both"), mtcars,
      ".(data) + ggplot2::facet_wrap(~cyl, scales = \"fixed\", labeller = \"label_both\", dir = \"v\")"
    ),
    list(
      new_facet_block("wrap", facets = "2025 Sales"), nsy,
      ".(data) + ggplot2::facet_wrap(~`2025 Sales`, scales = \"fixed\", labeller = \"label_value\")"
    ),
    list(new_facet_block("grid"), mtcars, ".(data)"),
    list(
      new_facet_block("grid", rows = "cyl"), mtcars,
      ".(data) + ggplot2::facet_grid(cyl ~ ., scales = \"fixed\", labeller = \"label_value\")"
    ),
    list(
      new_facet_block("grid", cols = "am"), mtcars,
      ".(data) + ggplot2::facet_grid(. ~ am, scales = \"fixed\", labeller = \"label_value\")"
    ),
    list(
      new_facet_block("grid", rows = "cyl", cols = "am"), mtcars,
      ".(data) + ggplot2::facet_grid(cyl ~ am, scales = \"fixed\", labeller = \"label_value\")"
    ),
    list(
      new_facet_block("grid", rows = c("cyl", "gear"), cols = "am", space = "free"), mtcars,
      ".(data) + ggplot2::facet_grid(cyl + gear ~ am, scales = \"fixed\", labeller = \"label_value\", space = \"free\")"
    ),
    list(
      new_facet_block("grid", rows = "2025 Sales", cols = "grp"), nsy,
      ".(data) + ggplot2::facet_grid(`2025 Sales` ~ grp, scales = \"fixed\", labeller = \"label_value\")"
    )
  )

  for (case in cases) {
    expect_equal(capture_expr(case[[1]], case[[2]]), case[[3]])
  }
})

# ---------------------------------------------------------------------------
# theme block
# ---------------------------------------------------------------------------

test_that("theme_block emits the expected expression across options", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  cases <- list(
    list(new_theme_block(), ".(data)"),
    list(new_theme_block(base_theme = "minimal"), ".(data) + ggplot2::theme_minimal()"),
    list(
      new_theme_block(base_theme = "gray"),
      ".(data) + ggplot2::theme_gray() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = \"#EBEBEB\"))"
    ),
    list(
      new_theme_block(panel_bg = "#fff", plot_bg = "#000"),
      ".(data) + ggplot2::theme(panel.background = ggplot2::element_rect(fill = \"#fff\"), plot.background = ggplot2::element_rect(fill = \"#000\"))"
    ),
    list(
      new_theme_block(base_size = "14", base_family = "serif"),
      ".(data) + ggplot2::theme(text = ggplot2::element_text(size = 14, family = \"serif\"))"
    ),
    list(
      new_theme_block(show_major_grid = "hide", show_minor_grid = "hide"),
      ".(data) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())"
    ),
    list(
      new_theme_block(show_major_grid = "show", show_minor_grid = "show", grid_color = "#ccc"),
      ".(data) + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = \"#ccc\"), panel.grid.minor = ggplot2::element_line(colour = \"#ccc\"))"
    ),
    list(
      new_theme_block(show_major_grid = "show"),
      ".(data) + ggplot2::theme(panel.grid.major = ggplot2::element_line())"
    ),
    list(
      new_theme_block(grid_color = "#ccc"),
      ".(data) + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = \"#ccc\"), panel.grid.minor = ggplot2::element_line(colour = \"#ccc\"))"
    ),
    list(
      new_theme_block(show_panel_border = "show"),
      ".(data) + ggplot2::theme(panel.border = ggplot2::element_rect(colour = \"grey50\", fill = NA))"
    ),
    list(
      new_theme_block(show_panel_border = "hide"),
      ".(data) + ggplot2::theme(panel.border = ggplot2::element_blank())"
    ),
    list(
      new_theme_block(legend_position = "bottom"),
      ".(data) + ggplot2::theme(legend.position = \"bottom\")"
    ),
    list(
      new_theme_block(palette_fill = "viridis_d"),
      ".(data) + ggplot2::scale_fill_viridis_d(option = \"viridis\")"
    ),
    list(
      new_theme_block(palette_fill = "magma_c"),
      ".(data) + ggplot2::scale_fill_viridis_c(option = \"magma\")"
    ),
    list(
      new_theme_block(palette_fill = "ggplot2"),
      ".(data) + ggplot2::scale_fill_discrete()"
    ),
    list(
      new_theme_block(palette_colour = "viridis_d"),
      ".(data) + ggplot2::scale_colour_viridis_d(option = \"viridis\")"
    ),
    list(
      new_theme_block(palette_colour = "ggplot2"),
      ".(data) + ggplot2::scale_colour_discrete()"
    ),
    list(
      new_theme_block(base_theme = "bw", panel_bg = "#eee", base_size = "12", base_family = "mono", show_major_grid = "show", grid_color = "#999", show_panel_border = "show", legend_position = "top", palette_fill = "viridis_d", palette_colour = "plasma_c"),
      ".(data) + ggplot2::theme_bw() + ggplot2::theme(panel.background = ggplot2::element_rect(fill = \"#eee\"), text = ggplot2::element_text(size = 12, family = \"mono\"), panel.grid.major = ggplot2::element_line(colour = \"#999\"), panel.grid.minor = ggplot2::element_line(colour = \"#999\"), panel.border = ggplot2::element_rect(colour = \"grey50\", fill = NA), legend.position = \"top\") + ggplot2::scale_fill_viridis_d(option = \"viridis\") + ggplot2::scale_colour_viridis_c(option = \"plasma\")"
    )
  )

  for (case in cases) {
    expect_equal(capture_expr(case[[1]]), case[[2]])
  }
})
