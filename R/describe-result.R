#' Describe a ggplot result for blockr.assistant
#'
#' Every block in this package evaluates to a `ggplot` object (the grid block
#' to a `patchwork`, which is one too). Printing one draws it and returns no
#' text, so blockr.assistant's `describe_result()` default hands the model an
#' empty string. This method reads the built plot instead: what the plot
#' computed, rather than the block's arguments.
#'
#' Registered in `.onLoad()` with [vctrs::s3_register()], so blockr.assistant
#' stays a suggestion. The assistant caps the text and turns a failure into a
#' message, so this neither truncates nor guards.
#'
#' @param x A `ggplot` or `patchwork` object.
#' @param ... Ignored.
#'
#' @return Character vector of lines.
#' @noRd
describe_ggplot <- function(x, ...) {

  if (inherits(x, "patchwork")) {
    n <- length(x)
    return(
      c(
        paste0("patchwork of ", n, " ggplot objects"),
        unlist(
          lapply(seq_len(n), function(i) {
            c(paste0("plot ", i, ":"), paste0("  ", ggplot_lines(x[[i]])))
          })
        )
      )
    )
  }

  c("ggplot object", ggplot_lines(x))
}

ggplot_lines <- function(p) {

  # Building runs the stats, which message (geom_smooth's formula) and warn
  # (rows removed); the block surfaces those itself.
  quietly <- function(expr) suppressMessages(suppressWarnings(expr))

  b <- quietly(ggplot2::ggplot_build(p))

  layers <- vapply(
    p$layers,
    function(l) paste(class(l$geom)[1L], "/", class(l$stat)[1L]),
    character(1L)
  )

  aes <- unique(
    c(names(p$mapping), unlist(lapply(p$layers, function(l) names(l$mapping))))
  )

  labs <- if (exists("get_labs", envir = asNamespace("ggplot2"))) {
    quietly(ggplot2::get_labs(p))
  } else {
    p$labels
  }
  labs <- labs[intersect(c("title", "subtitle", aes, "caption"), names(labs))]
  labs <- Filter(
    function(x) is.character(x) && length(x) == 1L && nzchar(x),
    labs
  )

  # Without layers, ggplot_build() still returns one blank layer's data.
  rows <- if (length(layers)) vapply(b$data, nrow, integer(1L))

  coord <- class(p$coordinates)[1L]

  c(
    paste(
      "layers:",
      if (length(layers)) paste(layers, collapse = ", ") else "none"
    ),
    if (length(aes)) paste("mapped aesthetics:", paste(aes, collapse = ", ")),
    if (length(labs)) {
      paste0("labels: ", paste0(names(labs), "=", labs, collapse = "; "))
    },
    paste0(
      "facet: ", class(p$facet)[1L],
      " | panels: ", nrow(b$layout$layout)
    ),
    if (!identical(coord, "CoordCartesian")) paste("coord:", coord),
    if (length(rows)) {
      paste0(
        "rows plotted",
        if (length(rows) > 1L) " per layer: " else ": ",
        paste(rows, collapse = ", ")
      )
    },
    scale_line("x", b$layout$panel_scales_x),
    scale_line("y", b$layout$panel_scales_y)
  )
}

# The first panel's scale: under free facet scales the others differ, which
# the facet line already signals.
scale_line <- function(axis, scales) {

  if (!length(scales)) {
    return(NULL)
  }

  sc <- scales[[1L]]
  lim <- sc$get_limits()

  if (sc$is_discrete()) {
    lim <- lim[!is.na(lim)]
    shown <- utils::head(lim, 6L)
    paste0(
      axis, " (discrete, ", length(lim), " levels): ",
      paste(shown, collapse = ", "),
      if (length(lim) > length(shown)) ", ..."
    )
  } else if (is.numeric(lim) && all(is.finite(lim))) {
    paste0(axis, " range: ", signif(lim[1L], 4L), " .. ", signif(lim[2L], 4L))
  }
}
