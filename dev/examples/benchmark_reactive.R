# Benchmark: reactiveVal Pattern (Run in Session 1)
#
# Run this in one R session while benchmark_direct.R runs in another.
# Both should complete around the same time if the test is valid.

library(blockr)
library(shiny)

# =============================================================================
# Pattern A: reactiveVal - selection preserved on data change
# =============================================================================
new_col_block_reactive <- function(column = character(), ...) {
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_column <- reactiveVal(column)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$column, r_column(input$column))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            cols <- colnames(data())
            if (length(r_column()) == 0 || !r_column() %in% cols) {
              r_column(cols[1])
            }
            updateSelectInput(session, "column",
              choices = cols,
              selected = r_column()
            )
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            cols <- colnames(data())
            if (!r_column() %in% cols) {
              r_column(cols[1])
            }
            updateSelectInput(session, "column",
              choices = cols,
              selected = r_column()
            )
          }
        })

        list(
          expr = reactive({
            col <- r_column()
            req(col)
            parse(text = glue::glue("dplyr::select(data, {col})"))[[1]]
          }),
          state = list(column = r_column)
        )
      })
    },
    function(id) {
      selectInput(NS(id, "column"), "Column (reactiveVal)", choices = NULL)
    },
    dat_val = function(data) stopifnot(is.data.frame(data)),
    class = "col_block_reactive",
    ...
  )
}

# =============================================================================
# Register block
# =============================================================================
blockr.core::register_block(
  ctor = new_col_block_reactive,
  name = "Column (reactiveVal)",
  description = "Pattern A - uses reactiveVal",
  category = "transform",
  icon = "circle",
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
  blocks[[paste0("reactive_", i)]] <- new_col_block_reactive()
  from <- if (i == 1) "data" else paste0("reactive_", i - 1)
  links[[length(links) + 1]] <- new_link(from, paste0("reactive_", i), "data")
}

# =============================================================================
# Run
# =============================================================================
cat("=== REACTIVE Pattern Benchmark (Session 1) ===\n\n")
cat("Running", n_blocks, "blocks with reactiveVal pattern\n")
cat("Start this alongside benchmark_direct.R in another session\n\n")

run_app(blocks = blocks, links = links)
