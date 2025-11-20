library(blockr.core)
devtools::load_all()

serve(
  new_board(
    blocks = c(
      data = new_dataset_block("mtcars", package = "datasets"),
      scatter = new_ggplot_block(
        type = "point",
        x = "wt",
        y = "mpg",
        color = "cyl"
      )
    ),
    links = c(new_link("data", "scatter", "data"))
  )
)
