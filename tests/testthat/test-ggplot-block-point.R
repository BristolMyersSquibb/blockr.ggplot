test_that("ggplot block constructor", {
  expect_s3_class(new_ggplot_block(), "ggplot_block")
})

test_that("ggplot block constructor", {
  expect_s3_class(new_ggplot_block(), "ggplot_transform_block")
})

test_that("ggplot block evaluates correctly", {
  block <- new_ggplot_block()

  testServer(
    app = block$expr_server,
    args = list(data = mtcars),
    expr = {
      session$setInputs(type = "point")
      session$setInputs(x = "mpg")
      session$setInputs(y = "disp")
      session$setInputs(color = "(none)")
      session$setInputs(fill = "(none)")
      session$setInputs(size = "(none)")
      session$setInputs(alpha = "(none)")

      plot <- eval(session$returned$expr())

      expect_equal(rlang::as_label(plot$mapping$x), "mpg")
      expect_equal(rlang::as_label(plot$mapping$y), "disp")
      expect_s3_class(plot$layers[[1]]$geom, "GeomPoint")

      session$setInputs(type = "line")
      session$setInputs(x = "qsec")
      session$setInputs(y = "wt")
      session$setInputs(linetype = "(none)")
      session$setInputs(group = "(none)")

      plot <- eval(session$returned$expr())

      expect_equal(rlang::as_label(plot$mapping$x), "qsec")
      expect_equal(rlang::as_label(plot$mapping$y), "wt")
      expect_s3_class(plot$layers[[1]]$geom, "GeomLine")
    }
  )
})
