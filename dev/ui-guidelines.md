# UI Development Guidelines

This guide documents the UI architecture of
[blockr.ggplot](https://github.com/BristolMyersSquibb/blockr.ggplot) blocks
after the settings-band rework (2026-07), which aligned the package with the
blockr design system as piloted by **blockr.viz** (see
`blockr.docs/design-system/target/design-system.html` and
`blockr.viz/dev/table-and-chart-architecture.md`).

The pre-rework Shiny-first guidelines (responsive `block-form-grid`,
"Show advanced options" toggle, `block_responsive_css()`) are obsolete —
those helpers were removed from `R/utils.R`.

## Architecture: JS-first settings band

Every block follows the blockr.viz pattern:

- The constructor's `ui` (the expr-UI slot) renders **only** the html
  dependencies (`ggplot_block_deps()`, `R/ggplot-dep.R`) plus one empty
  container:

  ```r
  div(id = NS(id, "gg_block"), class = "gg-block-container",
      `data-gg-block` = "ggplot")   # or "theme" / "facet" / "grid"
  ```

- `inst/js/gg-blocks.js` binds the container (`Shiny.InputBinding`), builds
  a gear header (`.blockr-gear-header` > `.blockr-gear-btn`) and an
  **in-flow settings band** (`.blockr-settings`, design-system gear-panel
  proposal B: no body portal, no fixed positioning; opening pushes the plot
  down so the result stays visible).

- The band content is rendered by the shared **`Blockr.DrilldownConfig`**
  engine (`inst/js/drilldown-config.js`, copied from blockr.viz — see the
  CANONICAL SOURCE header) from a declarative spec per block in
  `gg-blocks.js` (`SPECS`): role catalog (`kind: column | select | columns |
  segmented | slider | text | color`) plus per-type sections
  (requiredMap / optionalMap / mapping / presentation).

- The plot itself stays `block_ui`'s server-rendered `plotOutput`
  **below** the container — unlike blockr.viz (client-side ECharts), JS
  owns only the configuration UI.

## R <-> JS protocol

- **R -> JS**: one `shiny::observe()` per block sends the custom message
  `gg-block-data` with `{id, block, columns, config}` (plus `choices` for
  runtime-dependent option lists, e.g. the theme block's `base_theme`).
  Column metadata is `{name, type, n_unique, label?, levels?}`; no data
  frame is shipped.

- **JS -> R**: every change echoes the FULL config through
  `Shiny.setInputValue('<id>_action', {action: 'config', ...})`. The server
  applies it in one `observeEvent(input$gg_block_action)` where every write
  goes through the `identical()` guard:

  ```r
  upd <- function(rv, v) if (!identical(isolate(rv()), v)) rv(v)
  ```

  This guard is mandatory — a blind `reactiveVal` write re-triggers the
  push observer and echoes back to JS (R->JS->R loop). See
  `blockr.viz/R/chart-block.R` for the original pattern and rationale.

- **External control / restore** work for free: any state write re-runs the
  push observer, and JS re-renders the band from the new config.

## Keep in sync

- `chart_aesthetics` (R/ggplot-block.R) <-> `GG_TYPE_ROLES`
  (inst/js/gg-blocks.js): the R list stays authoritative for expression
  generation; the JS mirror drives which controls the band shows.
- The typed `new_block_args()` registry in `R/zzz.R` (read by blockr.ai)
  must match the constructor signatures — the rework did not change any.
- Copied shared assets carry `CANONICAL SOURCE` headers
  (`drilldown-config.js`, `settings-band.js`, `settings-band.css`,
  the `dd-*` rules in `gg-blocks.css`): keep them in sync with blockr.viz
  until they graduate to blockr.ui. Local engine extensions (slider
  min/max/step/unit, `kind: 'color'`, select `ph` placeholder) are
  upstream candidates.

## CSS namespaces

- `.blockr-*` — reserved for the shared layer (blockr.dplyr today,
  blockr.ui later). Never mint new ones here.
- `.dd-*` — emitted by the copied DrilldownConfig engine; rules copied
  verbatim from `blockr.viz/inst/css/chart.css`. Do not rename.
- `.gg-*` — blockr.ggplot's own prefix (container, previews, color swatch).

## Previews (grid / facet)

The SVG layout previews (`create_grid_preview_svg()`,
`create_facet_preview_svg()`) survive as R-rendered `uiOutput`s placed
inside the container after the band (`div(class = "gg-preview", ...)`).
`gg-blocks.js` inserts the gear/band **before** existing children
(append-only DOM building — never clear the container), and
`gg-blocks.css` shows the preview only while the band is open.

## Testing

- Server tests drive the config transport, not per-field inputs:

  ```r
  expr <- session$makeScope("expr")
  expr$setInputs(gg_block_action = list(action = "config", x = "hp"))
  ```

- `tests/testthat/test-ggplot-block-config-action.R` covers the transport
  invariants (full echo, identical() guard, donut on/off mapping, "(none)"
  restore parity).
- JS is type-checked, no build step: `npx -p typescript tsc`
  (tsconfig.json; opt-in per file via `// @ts-check`).
