test_that("grid_block constructor", {
  # Test basic constructor
  blk <- new_grid_block()
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with ncol parameter
  blk <- new_grid_block(ncol = "2")
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with nrow parameter
  blk <- new_grid_block(nrow = "2")
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with both ncol and nrow
  blk <- new_grid_block(ncol = "2", nrow = "2")
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test constructor with annotations
  blk <- new_grid_block(
    title = "Main Title",
    subtitle = "Subtitle",
    caption = "Caption"
  )
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("grid_block handles empty inputs", {
  # Test with character(0) inputs
  blk <- new_grid_block(
    ncol = character(),
    nrow = character(),
    title = character(),
    subtitle = character(),
    caption = character()
  )
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("grid_block with layout options", {
  # Test with tag_levels
  blk <- new_grid_block(tag_levels = "A")
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )

  # Test with guides option
  blk <- new_grid_block(guides = "collect")
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )
})

test_that("grid_block supports different tag levels", {
  tag_options <- c("A", "a", "1", "I", "i")

  for (tag in tag_options) {
    blk <- new_grid_block(tag_levels = tag)
    expect_s3_class(
      blk,
      c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
    )
  }
})

test_that("grid_block with all options", {
  # Test with all options
  blk <- new_grid_block(
    ncol = "3",
    nrow = "2",
    title = "Overall Title",
    subtitle = "Overall Subtitle",
    caption = "Figure caption",
    tag_levels = "A",
    guides = "collect"
  )
  expect_s3_class(
    blk,
    c("grid_block", "rbind_block", "ggplot_transform_block", "transform_block", "block")
  )
})

# Server-side tests
test_that("grid_block server input widgets update reactive values", {
  # Create sample ggplot objects
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block()$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Test ncol input
      session$setInputs(ncol = "2")
      expect_equal(ncol(), "2")

      # Test nrow input
      session$setInputs(nrow = "1")
      expect_equal(nrow(), "1")

      # Test title input
      session$setInputs(title = "My Title")
      expect_equal(title(), "My Title")

      # Test subtitle input
      session$setInputs(subtitle = "My Subtitle")
      expect_equal(subtitle(), "My Subtitle")

      # Test caption input
      session$setInputs(caption = "My Caption")
      expect_equal(caption(), "My Caption")

      # Test tag_levels input
      session$setInputs(tag_levels = "A")
      expect_equal(tag_levels(), "A")

      # Test guides input
      session$setInputs(guides = "collect")
      expect_equal(guides(), "collect")
    }
  )
})

test_that("grid_block state is correctly returned", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block()$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Check default values
      expect_equal(session$returned$state$ncol(), character())
      expect_equal(session$returned$state$nrow(), character())
      expect_equal(session$returned$state$guides(), "auto")

      # Update inputs and check state is updated
      session$setInputs(ncol = "2")
      expect_equal(session$returned$state$ncol(), "2")

      session$setInputs(nrow = "1")
      expect_equal(session$returned$state$nrow(), "1")

      session$setInputs(title = "Test Title")
      expect_equal(session$returned$state$title(), "Test Title")
    }
  )
})

test_that("grid_block expr evaluates correctly with two plots", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block()$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object (which inherits from ggplot)
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with layout options", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block(ncol = "2", nrow = "1")$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with title and subtitle", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block(
      title = "Main Title",
      subtitle = "Subtitle Text"
    )$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with tag_levels", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block(tag_levels = "A")$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with guides option", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, color = factor(cyl))) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt, color = factor(gear))) +
    ggplot2::geom_line()

  testServer(
    app = new_grid_block(guides = "collect")$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with three plots", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()
  plot3 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = qsec)) +
    ggplot2::geom_bar(stat = "identity")

  testServer(
    app = new_grid_block(ncol = "3")$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2),
      plot3 = reactive(plot3)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block expr evaluates correctly with all options", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()
  plot2 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
    ggplot2::geom_line()
  plot3 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = qsec)) +
    ggplot2::geom_bar(stat = "identity")
  plot4 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
    ggplot2::geom_boxplot()

  testServer(
    app = new_grid_block(
      ncol = "2",
      nrow = "2",
      title = "Main Title",
      subtitle = "Subtitle",
      caption = "Caption",
      tag_levels = "A",
      guides = "collect"
    )$expr_server,
    args = list(
      plot1 = reactive(plot1),
      plot2 = reactive(plot2),
      plot3 = reactive(plot3),
      plot4 = reactive(plot4)
    ),
    expr = {
      # Evaluate the expression
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a patchwork object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})

test_that("grid_block handles single plot", {
  plot1 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp)) +
    ggplot2::geom_point()

  testServer(
    app = new_grid_block()$expr_server,
    args = list(
      plot1 = reactive(plot1)
    ),
    expr = {
      # Evaluate the expression - should work with just one plot
      evaluated_expr <- eval(session$returned$expr())

      # Check that result is a ggplot object
      expect_s3_class(evaluated_expr, "ggplot")
    }
  )
})
