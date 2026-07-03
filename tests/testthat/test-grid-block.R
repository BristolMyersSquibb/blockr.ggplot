# Tests for new_grid_block under blockr.core's variadic name-or-position
# convention (core #251/#261): `...args` arrives as a core `reactives`
# object and the expr references inputs via `.()` calls (expr_type
# "bquoted"), mirroring blockr.core's own rbind_block tests.

p_point <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
}
p_bar <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
}

test_that("grid block combines unnamed variadic inputs", {
  blk <- new_grid_block()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "patchwork")
    },
    args = list(
      x = blk,
      data = list(
        ...args = blockr.core:::reactives(p_point, p_bar)
      )
    )
  )
})

test_that("grid block combines named variadic inputs", {
  blk <- new_grid_block()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_s3_class(session$returned$result(), "patchwork")
    },
    args = list(
      x = blk,
      data = list(
        ...args = blockr.core:::reactives(a = p_point, b = p_bar)
      )
    )
  )
})

test_that("grid block applies layout and annotation settings", {
  blk <- new_grid_block()

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      expr$setInputs(
        gg_block_action = list(
          action = "config",
          ncol = "1",
          title = "Combined view",
          guides = "collect",
          tag_levels = "A"
        )
      )
      session$flushReact()

      state <- session$returned$state
      expect_equal(state$ncol(), "1")
      expect_equal(state$title(), "Combined view")
      expect_equal(state$guides(), "collect")
      expect_equal(state$tag_levels(), "A")

      result <- session$returned$result()
      expect_s3_class(result, "patchwork")
      annot <- result$patches$annotation
      expect_equal(annot$title, "Combined view")
      expect_equal(annot$tag_levels, "A")
    },
    args = list(
      x = blk,
      data = list(
        ...args = blockr.core:::reactives(p_point, p_bar)
      )
    )
  )
})
