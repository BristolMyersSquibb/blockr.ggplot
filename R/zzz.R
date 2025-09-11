register_ggplot_blocks <- function() {
  # nocov start
  register_blocks(
    "new_chart_block",
    name = "Chart",
    description = "Create any chart type using ggplot2 with dynamic visualization selector",
    category = "plot",
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_ggplot_blocks()

  invisible(NULL)
} # nocov end
