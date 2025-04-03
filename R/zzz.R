register_ggplot_blocks <- function() { # nocov start
  register_blocks(
    c(
      "new_boxplot_block",
      "new_histogram_block"
    ),
    name = c(
      "Boxplot block",
      "Histogram block"
    ),
    description = c(
      "Create a boxplot using ggplot2",
      "Create a histogram using ggplot2"
    ),
    category = c(
      "plot",
      "plot"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {

  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
