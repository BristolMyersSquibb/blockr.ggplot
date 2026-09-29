#' HTML dependencies for the JS-first ggplot block UI
#'
#' blockr.ui brings the tokens and the shared controls (the `Blockr`
#' namespace: Select, menu, tooltip, checkbox, segmented control, gear tray,
#' commit-on-Enter fields). gg-blocks.js builds each block's face and gear
#' tray from them; gg-blocks.css lays out what is ggplot's own (chart-type
#' tiles, layout preview, colour field), scoped under `.gg-`.
#'
#' @importFrom blockr.ui controls_dep
#' @importFrom htmltools htmlDependency
#' @noRd
ggplot_block_deps <- memoise0(function() {
  htmltools::tagList(
    blockr.ui::controls_dep(),
    htmltools::htmlDependency(
      name = "gg-blocks-js",
      # Bump the suffix on every gg-blocks.js edit (asset cache).
      version = paste0(utils::packageVersion("blockr.ggplot"), ".1"),
      src = system.file("js", package = "blockr.ggplot"),
      script = "gg-blocks.js"
    ),
    htmltools::htmlDependency(
      name = "gg-blocks-css",
      # Bump the suffix on every gg-blocks.css edit (asset cache).
      version = paste0(utils::packageVersion("blockr.ggplot"), ".1"),
      src = system.file("css", package = "blockr.ggplot"),
      stylesheet = "gg-blocks.css"
    )
  )
})
