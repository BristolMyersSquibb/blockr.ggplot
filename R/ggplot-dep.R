#' HTML dependencies for the JS-first ggplot block UI
#'
#' Mirrors blockr.viz/R/chart-dep.R: the base design-system layer (gear,
#' popover rows, pills, the Blockr namespace and Blockr.Select component)
#' comes from blockr.dplyr's exported dependency helpers; the settings band
#' and the DrilldownConfig engine are local copies of the blockr.viz assets
#' (see the CANONICAL SOURCE headers in inst/js and inst/css).
#'
#' Load order is enforced by the dependency structure: blockr.dplyr CSS/JS
#' (Blockr namespace + Select) -> settings-band.js (Blockr.checkbox) ->
#' drilldown-config.js (Blockr.DrilldownConfig) -> gg-blocks.js (uses both).
#'
#' @importFrom blockr.dplyr blockr_blocks_css_dep blockr_select_dep
#' @noRd
ggplot_block_deps <- function() {
  htmltools::tagList(
    blockr_blocks_css_dep(),
    blockr_select_dep(),
    htmltools::htmlDependency(
      name = "gg-settings-band",
      version = utils::packageVersion("blockr.ggplot"),
      src = system.file(package = "blockr.ggplot"),
      script = "js/settings-band.js",
      stylesheet = "css/settings-band.css"
    ),
    htmltools::htmlDependency(
      name = "gg-blocks-js",
      version = paste0(utils::packageVersion("blockr.ggplot"), ".2"),
      src = system.file("js", package = "blockr.ggplot"),
      # drilldown-config.js (the shared gear/settings-band engine) must load
      # BEFORE gg-blocks.js, which references Blockr.DrilldownConfig.
      script = c("drilldown-config.js", "gg-blocks.js")
    ),
    htmltools::htmlDependency(
      name = "gg-blocks-css",
      version = paste0(utils::packageVersion("blockr.ggplot"), ".2"),
      src = system.file("css", package = "blockr.ggplot"),
      stylesheet = "gg-blocks.css"
    )
  )
}
