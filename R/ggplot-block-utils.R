#' Get chart aesthetic configurations
#'
#' Returns a list defining the aesthetic mappings available for each ggplot2
#' chart type. This includes which aesthetics are required, optional, and
#' any chart-specific parameters.
#'
#' @keywords internal
chart_aesthetics <- function() {
  list(
    point = list(
      required = c("x", "y"),
      optional = c("color", "shape", "size", "alpha", "fill"),
      specific = list()
    ),
    bar = list(
      required = c("x"),
      optional = c("y", "fill", "color", "alpha"),
      specific = list(position = c("stack", "dodge", "fill"))
    ),
    line = list(
      required = c("x", "y"),
      optional = c("color", "linetype", "alpha", "group"),
      specific = list()
    ),
    boxplot = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    violin = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    density = list(
      required = c("x"),
      optional = c("fill", "group"),
      specific = list(density_alpha = TRUE)
    ),
    area = list(
      required = c("x", "y"),
      optional = c("fill", "alpha"),
      specific = list()
    ),
    histogram = list(
      required = c("x"),
      optional = c("fill", "color", "alpha"),
      specific = list(
        bins = TRUE,
        position = c("stack", "identity", "dodge")
      )
    ),
    pie = list(
      required = c("x"), # x is categories, but rendered as x = ""
      optional = c("y", "fill", "alpha"),
      specific = list() # Could add donut = TRUE/FALSE later
    )
  )
}

#' Create aesthetic label with required field indicator
#'
#' Generates a label for aesthetic inputs that shows an asterisk for required
#' fields based on the chart type configuration.
#'
#' @param name Character string, the display name for the aesthetic (e.g.,
#'   "X-axis")
#' @param field_id Character string, the aesthetic field identifier (e.g., "x",
#'   "y")
#' @param chart_type Character string, the chart type (e.g., "point", "bar")
#'
#' @return A character string or HTML tag object. Returns a styled HTML span
#'   with asterisk for required fields, otherwise returns the name unchanged.
#'
#' @keywords internal
make_aesthetic_label <- function(name, field_id, chart_type) {
  chart_config <- chart_aesthetics()[[chart_type]]
  if (!is.null(chart_config)) {
    is_required <- field_id %in% chart_config$required
    if (is_required) {
      return(tags$span(
        tags$strong(name),
        tags$span("*", style = "color: #dc3545; margin-left: 2px;")
      ))
    }
  }
  name
}

#' Generate responsive CSS for blockr blocks
#'
#' Creates CSS for responsive grid layout using 'block-' prefix.
#' Can be reused across different blockr packages.
#'
#' @return HTML style tag with responsive CSS
#' @keywords internal
block_responsive_css <- function() {
  tags$style(HTML(
    "
    .block-container {
      width: 100%;
      margin: 0px;
      padding: 0px;
      padding-bottom: 15px;
    }

    /* One shared grid across the whole form */
    .block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    /* Flatten wrappers so all controls share the same tracks */
    .block-section,
    .block-section-grid {
      display: contents;
    }

    /* Headings/help span full width */
    .block-section h4,
    .block-help-text {
      grid-column: 1 / -1;
    }

    .block-section h4 {
      margin-top: 5px;
      margin-bottom: 0px;
      font-size: 1.1rem;
      font-weight: 600;
      color: #333;
    }

    .block-section:not(:first-child) {
      margin-top: 20px;
    }

    .block-input-wrapper {
      width: 100%;
    }

    .block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    .block-help-text {
      margin-top: -10px;  # offset the margin-bottom of the previous element
      padding-top: 0px;
      font-size: 0.875rem;
      color: #666;
    }
    "
  ))
}

#' Control chart type buttons via CSS
#'
#' @return HTML style tag with CSS to control chart type buttons
#' @keywords internal
block_chart_type_css <- function() {
  tags$style(HTML(
    "
    .chart-type-selector {
    margin-top: 0 !important;
    padding-top: 0 !important;
    width: 100%;
  }
  .chart-type-selector .btn-group-toggle,
  .chart-type-selector .btn-group {
    display: grid !important;
    grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
    gap: 5px;
    margin: 0;
    width: 100% !important;
    max-width: 100%;
  }
  .chart-type-selector .btn {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 8px 12px;
    white-space: nowrap;
    width: 100%;
  }
  .chart-type-selector .btn i {
    font-size: 1.2em;
    margin-bottom: 4px;
  }
  .chart-type-selector .btn span {
    font-size: 0.85em;
    white-space: nowrap;
  }
    "
  ))
}

#' Generate container query script for responsive blocks
#'
#' Sets up container queries if supported by the browser.
#'
#' @return HTML script tag
#' @keywords internal
block_container_script <- function() {
  tags$script(HTML(
    "
    // Set up container queries if supported
    if ('container' in document.documentElement.style) {
      var container = document.querySelector('.block-container');
      if (container) container.style.containerType = 'inline-size';
    }
    "
  ))
}

#' Generate CSS for collapsible advanced section with animated expand/collapse
#'
#' Creates inline CSS for an advanced options section that can be expanded
#' or collapsed with smooth animation and a rotating chevron indicator.
#'
#' @param id Character string, the module namespace ID used to scope the CSS
#'
#' @return HTML style tag containing the scoped CSS rules
#'
#' @keywords internal
block_collapisble_section_css <- function(id) {
  tags$style(HTML(sprintf(
    "
    #%s-advanced-options {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
      grid-column: 1 / -1;
      display: grid;
      grid-template-columns: subgrid;
      gap: 15px;
    }
    #%s-advanced-options.expanded {
      max-height: 2000px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      display: flex;
      align-items: center;
      gap: 6px;
      grid-column: 1 / -1;
      color: #6c757d;
      font-size: 0.875rem;
    }
    .advanced-toggle .chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .advanced-toggle .chevron.rotated {
      transform: rotate(90deg);
    }
    ",
    id,
    id
  )))
}

#' Create collapsible section toggle button
#'
#' Generates a clickable toggle that expands/collapses an advanced options
#' section with an animated chevron indicator.
#'
#' @param id Character string, the module namespace ID
#'
#' @return A \code{div} containing the clickable toggle with chevron icon
#'
#' @keywords internal
block_collapsible_section_div <- function(id) {
  div(
    class = "block-section",
    div(
      class = "advanced-toggle text-muted",
      id = NS(id, "advanced-toggle"),
      onclick = sprintf(
        "
        const section = document.getElementById('%s');
        const chevron = document.querySelector('#%s .chevron');
        section.classList.toggle('expanded');
        chevron.classList.toggle('rotated');
        ",
        NS(id, "advanced-options"),
        NS(id, "advanced-toggle")
      ),
      tags$span(class = "chevron", "\u203A"),
      "Show advanced options"
    )
  )
}
