#' Generate responsive CSS for blockr blocks
#'
#' Creates CSS for responsive grid layout using 'block-' prefix.
#' Can be reused across different blockr packages.
#'
#' @return HTML style tag with responsive CSS
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
