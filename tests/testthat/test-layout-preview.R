test_that("layout_preview fits plots into the auto layout", {
  p <- layout_preview(2)
  expect_equal(c(p$rows, p$cols), c(1, 2))
  expect_equal(p$state, "fit")
  expect_equal(unlist(p$cells), c("1", "2"))
  expect_equal(p$status, "2 plots in a 1 × 2 grid")
})

test_that("layout_preview marks empty slots", {
  p <- layout_preview(3, ncol_val = "2")
  expect_equal(c(p$rows, p$cols), c(2, 2))
  expect_equal(p$state, "gaps")
  expect_equal(unlist(p$cells), c("1", "2", "3", ""))
  expect_match(p$status, "1 empty$")
})

test_that("layout_preview flags a layout too small for the plots", {
  p <- layout_preview(5, ncol_val = "2", nrow_val = "2")
  expect_equal(p$state, "invalid")
  expect_equal(c(p$rows, p$cols), c(2, 2))
  expect_match(p$status, "need 5 slots")
})

test_that("facet layout fills down when asked", {
  p <- facet_layout_preview("wrap", n_levels = 3, ncol_val = "2", dir = "v")
  expect_equal(unlist(p$cells), c("1", "3", "2", ""))
  expect_match(p$status, "panels")
})

test_that("facet grid layout is rows by columns", {
  p <- facet_layout_preview("grid", n_rows = 2, n_cols = 3)
  expect_equal(c(p$rows, p$cols), c(2, 3))
  expect_equal(p$state, "fit")
  expect_length(p$cells, 6)
})

test_that("large layouts send no cells", {
  p <- layout_preview(100)
  expect_length(p$cells, 0)
})
