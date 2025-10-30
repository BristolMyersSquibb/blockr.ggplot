#' A custom blockr ggplot2 theme
#'
#' This theme is based on `hrbrthemes::theme_ipsum()` which is issued under an
#' MIT license.
theme_blockr <- function() {
  custom_theme <- ggplot2::theme_minimal(
    base_family = "Arial Narrow",
    base_size = 11.5
  )

  custom_theme <- custom_theme +
    ggplot2::theme(
      # Legend
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      # Grid
      panel.grid = ggplot2::element_line(color = "#cccccc", linewidth = 0.2),
      panel.grid.major = ggplot2::element_line(
        color = "#cccccc",
        linewidth = 0.2
      ),
      panel.grid.minor = ggplot2::element_line(
        color = "#cccccc",
        linewidth = 0.15
      ),

      # Axes
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 11.5,
        margin = ggplot2::margin(t = 0)
      ),
      axis.text.y = ggplot2::element_text(
        size = 11.5,
        margin = ggplot2::margin(r = 0)
      ),
      axis.title = ggplot2::element_text(size = 9, family = "Arial Narrow"),
      axis.title.x = ggplot2::element_text(
        hjust = 1,
        size = 9,
        family = "Arial Narrow",
        face = "plain"
      ),
      axis.title.y = ggplot2::element_text(
        hjust = 1,
        size = 9,
        family = "Arial Narrow",
        face = "plain"
      ),
      axis.title.y.right = ggplot2::element_text(
        hjust = 1,
        size = 9,
        angle = 90,
        family = "Arial Narrow",
        face = "plain"
      ),

      # Facets
      strip.text = ggplot2::element_text(
        hjust = 0,
        size = 12,
        face = "plain",
        family = "Arial Narrow"
      ),
      panel.spacing = grid::unit(2, "lines"),

      # Titles
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = 18,
        margin = ggplot2::margin(b = 10),
        family = "Arial Narrow",
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = 12,
        margin = ggplot2::margin(b = 15),
        family = "Arial Narrow",
        face = "plain"
      ),
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = 9,
        margin = ggplot2::margin(t = 10),
        family = "Arial Narrow",
        face = "italic"
      ),
      plot.margin = ggplot2::margin(30, 30, 30, 30)
    )

  custom_theme
}
