# Benchmark: Direct Input Pattern (Run in Session 2)
#
# Run this in one R session while benchmark_reactive.R runs in another.
# Both should complete around the same time if the test is valid.

library(blockr)
library(shiny)

# =============================================================================
# Pattern B: Direct Input - selection LOST on data change (BUG)
# =============================================================================
new_col_block_direct <- function(column = character(), ...) {
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_initialized <- reactiveVal(FALSE)

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            cols <- colnames(data())
            sel <- if (length(column) == 0 || !column %in% cols) cols[1] else column
            updateSelectInput(session, "column",
              choices = cols,
              selected = sel
            )
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            cols <- colnames(data())
            sel <- if (length(column) == 0 || !column %in% cols) cols[1] else column
            updateSelectInput(session, "column",
              choices = cols,
              selected = sel
            )
          }
        })

        list(
          expr = reactive({
            col <- input$column
            req(col)
            parse(text = glue::glue("dplyr::select(data, {col})"))[[1]]
          }),
          state = list(column = reactive(input$column))
        )
      })
    },
    function(id) {
      selectInput(NS(id, "column"), "Column (direct)", choices = NULL)
    },
    dat_val = function(data) stopifnot(is.data.frame(data)),
    class = "col_block_direct",
    ...
  )
}

# =============================================================================
# Register block
# =============================================================================
blockr.core::register_block(
  ctor = new_col_block_direct,
  name = "Column (direct)",
  description = "Pattern B - uses direct input",
  category = "transform",
  icon = "square",
  overwrite = TRUE
)

# =============================================================================
# Create workflow
# =============================================================================
n_blocks <- 20

blocks <- list(
  data = blockr.core::new_dataset_block(dataset = "mtcars")
)
links <- list()

for (i in seq_len(n_blocks)) {
  blocks[[paste0("direct_", i)]] <- new_col_block_direct()
  from <- if (i == 1) "data" else paste0("direct_", i - 1)
  links[[length(links) + 1]] <- new_link(from, paste0("direct_", i), "data")
}

# =============================================================================
# Run
# =============================================================================
cat("=== DIRECT Pattern Benchmark (Session 2) ===\n\n")
cat("Running", n_blocks, "blocks with direct input pattern\n")
cat("Start this alongside benchmark_reactive.R in another session\n\n")

run_app(blocks = blocks, links = links)
