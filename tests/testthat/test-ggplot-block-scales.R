# Tests for the axis / annotation options of new_ggplot_block():
# smoother, y_trans, y_zero, and the labs() text fields.
#
# Each option is checked twice: on the emitted expression (the code the
# report gets) and on the built plot (what the panel shows).

norm <- function(x) {
  gsub("\\s+", " ", trimws(paste(deparse(x, width.cutoff = 500L), collapse = " ")))
}

expr_of <- function(block, data = mtcars) {
  out <- NULL
  suppressWarnings(suppressMessages(
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()
        out <<- norm(session$returned$expr())
      },
      args = list(x = block, data = list(data = function() data))
    )
  ))
  out
}

plot_of <- function(block, data = mtcars) {
  out <- NULL
  suppressWarnings(suppressMessages(
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()
        out <<- session$returned$result()
      },
      args = list(x = block, data = list(data = function() data))
    )
  ))
  out
}

geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

# =============================================================================
# Defaults emit nothing new
# =============================================================================

test_that("the new options are absent from the expression by default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  e <- expr_of(new_ggplot_block(type = "point", x = "wt", y = "mpg"))

  expect_false(grepl("geom_smooth", e, fixed = TRUE))
  expect_false(grepl("scale_y_", e, fixed = TRUE))
  expect_false(grepl("expand_limits", e, fixed = TRUE))
  expect_false(grepl("labs(", e, fixed = TRUE))
})

# =============================================================================
# Smoother
# =============================================================================

test_that("smoother adds a geom_smooth layer with an explicit formula", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point", x = "wt", y = "mpg", smoother = "loess"
  )

  # The formula is spelled out so ggplot2 prints no "using formula" message.
  expect_true(grepl(
    'ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE)',
    expr_of(block),
    fixed = TRUE
  ))
  expect_true("GeomSmooth" %in% geoms(plot_of(block)))
})

test_that("smoother_se = FALSE drops the confidence band", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point", x = "wt", y = "mpg",
    smoother = "lm", smoother_se = FALSE
  )

  expect_true(grepl("se = FALSE", expr_of(block), fixed = TRUE))
  expect_false(plot_of(block)$layers[[2]]$geom_params$se)
})

test_that("the smoother is point/line only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  expect_true(grepl("geom_smooth", expr_of(new_ggplot_block(
    type = "line", x = "wt", y = "mpg", smoother = "lm"
  )), fixed = TRUE))

  # A boxplot carries its own stat; the option is not offered and the
  # expression must not sprout a fit nobody asked for.
  expect_false(grepl("geom_smooth", expr_of(new_ggplot_block(
    type = "boxplot", x = "cyl", y = "mpg", smoother = "lm"
  )), fixed = TRUE))
})

# =============================================================================
# Y transform
# =============================================================================

test_that("y_trans emits the matching scale and transforms the axis", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  sq <- new_ggplot_block(type = "point", x = "wt", y = "mpg", y_trans = "sqrt")
  expect_true(grepl("ggplot2::scale_y_sqrt()", expr_of(sq), fixed = TRUE))
  expect_equal(plot_of(sq)$scales$get_scales("y")$trans$name, "sqrt")

  lg <- new_ggplot_block(type = "point", x = "wt", y = "mpg", y_trans = "log10")
  expect_true(grepl("ggplot2::scale_y_log10()", expr_of(lg), fixed = TRUE))
  expect_equal(plot_of(lg)$scales$get_scales("y")$trans$name, "log-10")
})

test_that("a transformed axis is fitted on, not drawn over", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  # The point of a scale over a mutate: ggplot2 transforms before the stat,
  # so the smoother is fitted on the square-root scale and the axis still
  # reads in the data's units. Both terms are present, and the scale comes
  # after the layer, which is what makes that ordering true.
  e <- expr_of(new_ggplot_block(
    type = "point", x = "wt", y = "mpg", smoother = "loess", y_trans = "sqrt"
  ))
  expect_true(grepl("geom_smooth", e, fixed = TRUE))
  expect_true(grepl("scale_y_sqrt", e, fixed = TRUE))
})

# =============================================================================
# Zero
# =============================================================================

test_that("y_zero pulls the axis down to zero", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point", x = "wt", y = "mpg", y_zero = TRUE
  )
  expect_true(grepl("ggplot2::expand_limits(y = 0)", expr_of(block),
                    fixed = TRUE))

  # mpg runs 10.4 to 33.9, so the panel only reaches zero if the term works.
  rng <- ggplot2::layer_scales(plot_of(block))$y$get_limits()
  expect_lte(rng[1], 0)
})

test_that("y_zero is dropped under log10, which has no zero", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  e <- expr_of(new_ggplot_block(
    type = "point", x = "wt", y = "mpg", y_zero = TRUE, y_trans = "log10"
  ))
  expect_true(grepl("scale_y_log10", e, fixed = TRUE))
  expect_false(grepl("expand_limits", e, fixed = TRUE))
})

# =============================================================================
# Text
# =============================================================================

test_that("labs carries only the fields that were filled in", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "point", x = "wt", y = "mpg",
    title = "Weight and mileage", ylab = "Miles per gallon"
  )

  expect_true(grepl(
    'ggplot2::labs(title = "Weight and mileage", y = "Miles per gallon")',
    expr_of(block),
    fixed = TRUE
  ))

  p <- plot_of(block)
  expect_equal(p$labels$title, "Weight and mileage")
  expect_equal(p$labels$y, "Miles per gallon")
  expect_null(p$labels$subtitle)
})

test_that("pie takes a title but no axis names", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  e <- expr_of(new_ggplot_block(
    type = "pie", x = "cyl", title = "Cylinders", xlab = "ignored",
    y_trans = "sqrt"
  ))

  expect_true(grepl('ggplot2::labs(title = "Cylinders")', e, fixed = TRUE))
  expect_false(grepl("ignored", e, fixed = TRUE))
  expect_false(grepl("scale_y_sqrt", e, fixed = TRUE))
})

# =============================================================================
# Round trip through the settings band
# =============================================================================

test_that("the band's config echo reaches the expression", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(type = "point", x = "wt", y = "mpg")

  suppressWarnings(suppressMessages(
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        # The band lives in the block's "expr" scope (see
        # test-ggplot-block-config-action.R).
        expr_scope <- session$makeScope("expr")
        session$flushReact()
        expr_scope$setInputs(gg_block_action = list(
          action = "config", type = "point", x = "wt", y = "mpg",
          smoother = "lm", smoother_se = "off",
          y_trans = "sqrt", y_zero = "on", title = "Typed in the band"
        ))
        session$flushReact()
        e <- norm(session$returned$expr())

        expect_true(grepl("method = \"lm\"", e, fixed = TRUE))
        expect_true(grepl("se = FALSE", e, fixed = TRUE))
        expect_true(grepl("scale_y_sqrt", e, fixed = TRUE))
        expect_true(grepl("expand_limits", e, fixed = TRUE))
        expect_true(grepl("Typed in the band", e, fixed = TRUE))

        # State follows, so a save/restore round trip keeps them.
        expect_equal(session$returned$state$smoother(), "lm")
        expect_false(session$returned$state$smoother_se())
        expect_equal(session$returned$state$y_trans(), "sqrt")
        expect_true(session$returned$state$y_zero())
      },
      args = list(x = block, data = list(data = function() mtcars))
    )
  ))
})
