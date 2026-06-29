# Board scale-map adoption: the expr gains a literal scale_*_manual() when
# the board option binds the mapped variable (mechanism in blockr.theme).

test_that("ggplot block expr injects scale_fill_manual from the board map", {
  skip_if_not_installed("blockr.theme")

  df <- data.frame(
    TRT = c("A", "A", "B"),
    AVAL = c(1, 2, 3)
  )
  blk <- new_ggplot_block(type = "bar", x = "TRT", fill = "TRT")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$userData$board_options <- list(
        scale_map = shiny::reactiveVal(
          list(TRT = list(color = list(A = "#111111", B = "#222222")))
        )
      )
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
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
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_no_match(txt, "scale_fill_manual", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("incomplete binding (unpinned level, no pool) is not injected", {
  skip_if_not_installed("blockr.theme")

  df <- data.frame(TRT = c("A", "B"), AVAL = c(1, 2))
  blk <- new_ggplot_block(type = "bar", x = "TRT", fill = "TRT")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$userData$board_options <- list(
        scale_map = shiny::reactiveVal(
          list(TRT = list(color = list(A = "#111111")))
        )
      )
      session$flushReact()
      txt <- paste(deparse(session$returned$expr()), collapse = " ")
      # scale_*_manual errors on missing levels; partial pins fall back
      expect_no_match(txt, "scale_fill_manual", fixed = TRUE)
    },
    args = list(x = blk, data = list(data = function() df))
  )
})
