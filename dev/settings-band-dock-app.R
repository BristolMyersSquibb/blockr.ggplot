# Dev app: all four settings-band blocks in a dock board with the DAG
# extension. Exercises the JS-first UI (gear -> in-flow band) for the
# ggplot, facet, theme and grid blocks in the real board environment
# (dock panels, DAG canvas, save/restore).
#
# Run: Rscript dev/settings-band-dock-app.R  -> http://localhost:3838

suppressMessages({
  library(blockr.core)
  library(blockr.dock)
})

devtools::load_all(".", quiet = TRUE)

# options(shiny.port = 3838, shiny.host = "0.0.0.0")

board <- new_dock_board(
  blocks = c(
    data  = new_dataset_block("mtcars"),
    scat  = new_ggplot_block(type = "point", x = "mpg", y = "hp"),
    bars  = new_ggplot_block(type = "bar", x = "cyl"),
    facet = new_facet_block(facet_type = "wrap", facets = "cyl"),
    theme = new_theme_block(base_theme = "minimal"),
    grid  = new_grid_block(ncol = "1", title = "Combined view")
  ),
  links = c(
    new_link("data", "scat", "data"),
    new_link("data", "bars", "data"),
    new_link("scat", "facet", "data"),
    new_link("bars", "theme", "data"),
    new_link("facet", "grid"),
    new_link("theme", "grid")
  ),
  stacks = c(
    prep    = new_stack(c("data"), name = "Data"),
    plots   = new_stack(c("scat", "bars"), name = "Plots"),
    combine = new_stack(c("facet", "theme", "grid"), name = "Compose")
  ),
  # NB: extensions must be a named list(), not c()
  extensions = list(
    dag = blockr.dag::new_dag_extension()
  )
)

serve(board, id = "ggplot_settings_band")
