# Tests for the JS-first config transport of new_ggplot_block: the settings
# band echoes the FULL config through a single `gg_block_action` input (see
# inst/js/gg-blocks.js), and the server applies it through identical()-guarded
# reactiveVal writes.

test_that("full config echo updates every field", {
  block <- new_ggplot_block(type = "point", x = "wt", y = "mpg")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      expr$setInputs(
        gg_block_action = list(
          action = "config",
          type = "point",
          x = "hp",
          y = "qsec",
          color = "cyl",
          fill = "am",
          size = "disp",
          shape = "gear",
          linetype = "",
          group = "",
          alpha = "drat",
          density_alpha = 0.5,
          position = "dodge",
          bins = 15,
          donut = "off"
        )
      )
      session$flushReact()

      state <- session$returned$state
      expect_equal(state$type(), "point")
      expect_equal(state$x(), "hp")
      expect_equal(state$y(), "qsec")
      expect_equal(state$color(), "cyl")
      expect_equal(state$fill(), "am")
      expect_equal(state$size(), "disp")
      expect_equal(state$shape(), "gear")
      expect_equal(state$linetype(), "(none)")
      expect_equal(state$group(), "(none)")
      expect_equal(state$alpha(), "drat")
      expect_equal(state$density_alpha(), 0.5)
      expect_equal(state$position(), "dodge")
      expect_equal(state$bins(), 15)
      expect_false(state$donut())
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("unchanged full echo does not invalidate the expression", {
  block <- new_ggplot_block(type = "point", x = "wt", y = "mpg")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()

      # The exact config the server would push to JS for this state.
      echo <- list(
        action = "config",
        type = "point",
        x = "wt",
        y = "mpg",
        color = "",
        fill = "",
        size = "",
        shape = "",
        linetype = "",
        group = "",
        alpha = "",
        density_alpha = 0.8,
        position = "stack",
        bins = 30,
        donut = "off"
      )

      before <- session$returned$expr()
      expr$setInputs(gg_block_action = echo)
      session$flushReact()

      # identical() guard: no reactiveVal was written, so the expression
      # object is unchanged (no invalidation-driven rebuild of state).
      expect_identical(session$returned$expr(), before)
      expect_equal(session$returned$state$x(), "wt")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("donut transport maps on/off to logical state", {
  block <- new_ggplot_block(type = "pie", x = "cyl")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      expect_false(session$returned$state$donut())

      expr$setInputs(
        gg_block_action = list(action = "config", donut = "on")
      )
      session$flushReact()
      expect_true(session$returned$state$donut())

      expr$setInputs(
        gg_block_action = list(action = "config", donut = "off")
      )
      session$flushReact()
      expect_false(session$returned$state$donut())
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("clearing an optional aesthetic maps '' back to '(none)'", {
  block <- new_ggplot_block(
    type = "point",
    x = "wt",
    y = "mpg",
    color = "cyl"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      expr <- session$makeScope("expr")
      session$flushReact()
      expect_equal(session$returned$state$color(), "cyl")

      expr$setInputs(
        gg_block_action = list(action = "config", color = "")
      )
      session$flushReact()

      expect_equal(session$returned$state$color(), "(none)")
      expect_null(session$returned$result()$mapping$colour)
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})

test_that("state saved with '(none)' sentinels restores identically", {
  # Simulates a board restore: blockr.core re-calls the constructor with the
  # serialized state values (including "(none)" sentinels).
  block <- new_ggplot_block(
    type = "bar",
    x = "cyl",
    y = "(none)",
    fill = "am",
    color = "(none)",
    position = "dodge"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      state <- session$returned$state
      expect_equal(state$type(), "bar")
      expect_equal(state$x(), "cyl")
      expect_equal(state$y(), "(none)")
      expect_equal(state$fill(), "am")
      expect_equal(state$color(), "(none)")
      expect_equal(state$position(), "dodge")

      result <- session$returned$result()
      expect_true(inherits(result, "ggplot"))
      expect_equal(rlang::as_name(result$mapping$x), "cyl")
    },
    args = list(x = block, data = list(data = function() mtcars))
  )
})
