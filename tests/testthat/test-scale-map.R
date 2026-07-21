# Board scale-map adoption: the expr gains a scale_*_manual() layer when the
# board option binds the mapped variable (mechanism in blockr.theme). The
# board-option lookup and blockr.theme resolver are mocked so these tests
# exercise blockr.ggplot's own wiring without a live board.

dep1 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")

test_that("ggplot block expr injects scale_fill_manual from the board map", {
  skip_if_not_installed("blockr.theme")

  df <- data.frame(TRT = c("A", "A", "B"), AVAL = c(1, 2, 3))
  blk <- new_ggplot_block(type = "bar", x = "TRT", fill = "TRT")

  local_mocked_bindings(
    get_board_option_or_null = function(opt, ...) {
      if (identical(opt, "scale_map")) {
        list(TRT = list(color = list(A = "#111111", B = "#222222")))
      }
    },
    .package = "blockr.core"
  )
  local_mocked_bindings(
    resolve_scales = function(...) {
      list(color = c(A = "#111111", B = "#222222"))
    },
    .package = "blockr.theme"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      txt <- dep1(session$returned$expr())
      expect_match(txt, "scale_fill_manual", fixed = TRUE)
      expect_match(txt, "#111111", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("no binding / no map -> no manual scale, ggplot defaults", {
  df <- data.frame(TRT = c("A", "B"), AVAL = c(1, 2))
  blk <- new_ggplot_block(type = "bar", x = "TRT", fill = "TRT")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      txt <- dep1(session$returned$expr())
      expect_no_match(txt, "scale_fill_manual", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("incomplete binding (unpinned level, no pool) is not injected", {
  skip_if_not_installed("blockr.theme")

  df <- data.frame(TRT = c("A", "B"), AVAL = c(1, 2))
  blk <- new_ggplot_block(type = "bar", x = "TRT", fill = "TRT")

  local_mocked_bindings(
    get_board_option_or_null = function(opt, ...) {
      if (identical(opt, "scale_map")) list(TRT = list(color = list(A = "#111111")))
    },
    .package = "blockr.core"
  )
  # Only level A resolves; B is missing, so the assignment is incomplete.
  local_mocked_bindings(
    resolve_scales = function(...) list(color = c(A = "#111111")),
    .package = "blockr.theme"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      txt <- dep1(session$returned$expr())
      # scale_*_manual errors on missing levels; partial pins fall back
      expect_no_match(txt, "scale_fill_manual", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

# gg_scale_map_call() builds the scale_*_manual() call directly (no text
# assembly). Verify the exact call it emits from resolved values.
test_that("gg_scale_map_call builds a scale_*_manual call from resolved values", {
  skip_if_not_installed("blockr.theme")

  df <- data.frame(TRT = c("A", "A", "B"), AVAL = c(1, 2, 3))
  vals <- c(A = "#111111", B = "#222222")

  local_mocked_bindings(
    get_board_option_or_null = function(opt, ...) if (identical(opt, "scale_map")) list(TRT = "stub"),
    .package = "blockr.core"
  )
  local_mocked_bindings(
    resolve_scales = function(...) list(color = vals),
    .package = "blockr.theme"
  )

  fill <- gg_scale_map_call(NULL, df, "TRT", "fill")
  expect_true(is.call(fill))
  expect_equal(
    dep1(fill),
    "ggplot2::scale_fill_manual(values = c(A = \"#111111\", B = \"#222222\"))"
  )

  colour <- gg_scale_map_call(NULL, df, "TRT", "colour")
  expect_equal(
    dep1(colour),
    "ggplot2::scale_colour_manual(values = c(A = \"#111111\", B = \"#222222\"))"
  )
})

test_that("gg_scale_map_call returns NULL when no variable is bound", {
  expect_null(gg_scale_map_call(NULL, mtcars, "(none)", "fill"))
  expect_null(gg_scale_map_call(NULL, mtcars, NULL, "fill"))
})
