test_that("a ggplot is described by what it built (#95)", {
  p <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(mpg, hp, colour = factor(cyl))
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::facet_wrap(~am) +
    ggplot2::labs(title = "Engine")

  out <- describe_ggplot(p)

  expect_type(out, "character")
  expect_identical(out[1L], "ggplot object")
  expect_contains(
    out,
    c(
      "layers: GeomPoint / StatIdentity, GeomSmooth / StatSmooth",
      "mapped aesthetics: x, y, colour",
      "labels: title=Engine; x=mpg; y=hp; colour=factor(cyl)",
      "facet: FacetWrap | panels: 2",
      "x range: 10.4 .. 33.9"
    )
  )
  expect_true(any(startsWith(out, "rows plotted per layer: 32, ")))
  expect_false(any(startsWith(out, "coord:")))
})

test_that("a discrete axis lists its levels, and a pie names its coord", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::coord_polar()

  out <- describe_ggplot(p)

  expect_contains(
    out,
    c(
      "layers: GeomBar / StatCount",
      "coord: CoordPolar",
      "rows plotted: 3",
      "x (discrete, 3 levels): 4, 6, 8"
    )
  )
})

test_that("a plot without layers says so", {
  out <- describe_ggplot(ggplot2::ggplot(mtcars))

  expect_contains(out, "layers: none")
  expect_false(any(startsWith(out, "rows plotted")))
})

test_that("a patchwork describes each of its plots", {
  a <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
  b <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()

  out <- describe_ggplot(patchwork::wrap_plots(a, b))

  expect_identical(out[1L], "patchwork of 2 ggplot objects")
  expect_contains(
    out,
    c(
      "plot 1:",
      "  layers: GeomPoint / StatIdentity",
      "plot 2:",
      "  layers: GeomBar / StatCount"
    )
  )
})

test_that("blockr.assistant's describe_result() reaches the method", {
  skip_if_not_installed("blockr.assistant")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()

  expect_identical(blockr.assistant::describe_result(p), describe_ggplot(p))
})

test_that("a ggplot block's result is described", {
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      out <- describe_ggplot(session$returned$result())
      expect_contains(out, "layers: GeomPoint / StatIdentity")
      expect_contains(out, "rows plotted: 32")
    },
    args = list(x = blk, data = list(data = function() mtcars))
  )
})
