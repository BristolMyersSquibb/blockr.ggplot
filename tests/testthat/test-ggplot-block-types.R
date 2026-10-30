# Every value the registry offers for `type` must draw a plot. The enum is
# read from the registry entry, so a value added there without a geom branch
# fails here.

ggplot_type_enum <- function() {
  entry <- blockr.core::available_blocks()[["ggplot_block"]]
  as.character(unclass(attr(entry, "arguments")$type)$type$enum)
}

test_that("registry lists the ggplot types", {
  types <- ggplot_type_enum()
  expect_true(length(types) > 0)
  expect_true("col" %in% types)
})

for (type in ggplot_type_enum()) {
  for (with_fill in c(FALSE, TRUE)) {
    label <- paste0(
      "type = '", type, "' builds",
      if (with_fill) " with fill and color" else ""
    )
    test_that(label, {
      skip_if_not_installed("ggplot2")

      block <- if (with_fill) {
        new_ggplot_block(
          type = type, x = "cyl", y = "mpg", fill = "gear", color = "am"
        )
      } else {
        new_ggplot_block(type = type, x = "cyl", y = "mpg")
      }

      testServer(
        blockr.core:::get_s3_method("block_server", block),
        {
          # The block draws the plot on flush; warnings dropped as below.
          suppressWarnings(session$flushReact())
          result <- session$returned$result()

          expect_s3_class(result, "ggplot")
          expect_true(length(result$layers) > 0)
          expect_false(
            inherits(result$layers[[1]]$geom, "GeomBlank"),
            info = type
          )
          # A missing aesthetic or unknown geom is an error. Warnings here
          # are about the mtcars columns (e.g. stacking on cyl) and are
          # dropped.
          built <- suppressWarnings(ggplot2::ggplot_build(result))
          expect_true(nrow(built$data[[1]]) > 0, info = type)
        },
        args = list(x = block, data = list(data = function() mtcars))
      )
    })
  }
}

test_that("col draws geom_col with x and y", {
  skip_if_not_installed("ggplot2")

  block <- new_ggplot_block(
    type = "col", x = "cyl", y = "mpg", fill = "gear", position = "dodge"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result$layers[[1]]$geom, "GeomCol")
      expect_s3_class(result$layers[[1]]$position, "PositionDodge")
      expect_equal(rlang::as_name(result$mapping$x), "cyl")
      expect_equal(rlang::as_name(result$mapping$y), "mpg")
      expect_equal(
        rlang::quo_get_expr(result$mapping$fill),
        quote(as.factor(gear))
      )
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
