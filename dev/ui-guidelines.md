# UI Development Guidelines

This guide documents the UI architecture of
[blockr.ggplot](https://github.com/BristolMyersSquibb/blockr.ggplot) blocks.
They follow the blockr design system, written down in blockr.ui's
`vignettes/articles/design-system.Rmd`, and draw every control with
blockr.ui's shared components.

## Architecture: JS-first face and gear tray

- The constructor's `ui` (the expr-UI slot) renders **only** the html
  dependencies (`ggplot_block_deps()`, `R/ggplot-dep.R`: blockr.ui's
  `controls_dep()` plus `gg-blocks.js` and `gg-blocks.css`) and one empty
  container:

  ```r
  div(id = NS(id, "gg_block"), class = "gg-block-container",
      `data-gg-block` = "ggplot")   # or "theme" / "facet" / "grid"
  ```

- `inst/js/gg-blocks.js` binds the container (`Shiny.InputBinding`) and
  builds, top to bottom: the gear (`.blockr-gear-btn`), the gear tray
  (`.blockr-settings`, driven by `Blockr.gearTray`) and the face
  (`.gg-face`). Each block's `SPECS` entry lists its face and its tray
  sections as fields; `shape()` names what decides the set of controls, so
  a push that only moves values updates them in place and an open menu or
  a focused field survives it.

- What sits where:
  - ggplot: chart-type tiles and the mapping on the face (the spec keeps a
    chart-building block's type and mapping there); presentation in the
    tray. Optional mappings appear through "Add mapping" (`Blockr.menu`).
  - facet: Layout and the facet columns on the face; the rest and the
    layout preview in the tray.
  - theme: base theme, legend and palettes on the face; colours and text
    and lines in the tray.
  - grid: the face is the plot; layout (with the preview) and titles in
    the tray.

- Controls: column pickers and fixed sets are `Blockr.Select` (a fixed set
  shows its labels only), two or three values are `Blockr.segmented`,
  on/off is `Blockr.checkbox`, text and numbers commit on Enter or blur
  (`Blockr.textCommit`). The colour field is local (`.gg-colour`) until a
  second package needs it.

- The plot itself stays `block_ui`'s server-rendered `plotOutput`
  **below** the container, drawn on a white device so it stays light in
  the dark scheme.

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

- `chart_aesthetics` (R/ggplot-block.R) <-> `GG_TYPES`
  (inst/js/gg-blocks.js): the R list stays authoritative for expression
  generation; the JS mirror decides which mapping fields the face shows.
- The typed `new_block_args()` registry in `R/zzz.R` (read by blockr.ai)
  must match the constructor signatures.

## CSS

- `.blockr-*` classes belong to blockr.ui. Use them, never restyle them.
- `.gg-*` is blockr.ggplot's own prefix, and `--blockr-ggplot-*` its local
  settings. Read meaning tokens only (no palette steps, no legacy aliases,
  no literal colours), and check the dark scheme.

## Previews (grid / facet)

`layout_preview()` (R/grid-block.R) and `facet_layout_preview()` compute
the layout (rows, columns, a label per cell, a state of `fit`, `gaps` or
`invalid`, the status line) with `ggplot2::wrap_dims()`. It travels in the
push message as `preview`; `gg-blocks.js` draws it in the tray.

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
