# Benchmark: reactiveVal vs Direct Input Pattern
#
# This demonstrates the bug where user selections are lost with Pattern B.
# Both patterns share the same dataset - change it to see the difference.
#
# Each block selects a single column, so downstream blocks see only that column.
#
# TEST:
# 1. Run this script
# 2. Change column selection in blocks on BOTH sides (e.g., "mpg" -> "hp")
# 3. Change the dataset at the top
# 4. OBSERVE:
#    - Left branch (Pattern A): Selection PRESERVED
#    - Right branch (Pattern B): Selection RESET to first column (BUG!)

library(blockr)
library(shiny)

# =============================================================================
# Pattern A: reactiveVal - selection preserved on data change
# =============================================================================
new_col_block_reactive <- function(column = character(), ...) {
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Pattern A: Create reactiveVal
        r_column <- reactiveVal(column)
        r_initialized <- reactiveVal(FALSE)

        # Pattern A: Sync input -> reactiveVal
        observeEvent(input$column, r_column(input$column))

        # Initialize on first data
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

        # Pattern A: Read FROM reactiveVal when updating UI
        observeEvent(colnames(data()), {
          if (r_initialized()) {
            cols <- colnames(data())
            # Keep selection if still valid, otherwise default to first
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
# Pattern B: Direct Input - selection LOST on data change (BUG)
# =============================================================================
new_col_block_direct <- function(column = character(), ...) {
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Pattern B: NO reactiveVal
        r_initialized <- reactiveVal(FALSE)

        # Initialize on first data
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

        # Pattern B: Read FROM closure var - BUG: never updates!
        observeEvent(colnames(data()), {
          if (r_initialized()) {
            cols <- colnames(data())
            # BUG: 'column' is the closure var from construction, not current selection!
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
# Register blocks
# =============================================================================
blockr.core::register_block(
  ctor = new_col_block_reactive,
  name = "Column (reactiveVal)",
  description = "Pattern A - uses reactiveVal",
  category = "transform",
  icon = "circle",
  overwrite = TRUE
)

blockr.core::register_block(
  ctor = new_col_block_direct,
  name = "Column (direct)",
  description = "Pattern B - uses direct input",
  category = "transform",
  icon = "square",
  overwrite = TRUE
)

# =============================================================================
# Create workflow with two branches from same dataset
# =============================================================================
n_blocks <- 20

blocks <- list(
  data = blockr.core::new_dataset_block(dataset = "mtcars")
)
links <- list()

# Left branch: Pattern A (reactiveVal)
for (i in seq_len(n_blocks)) {
  blocks[[paste0("reactive_", i)]] <- new_col_block_reactive()
  from <- if (i == 1) "data" else paste0("reactive_", i - 1)
  links[[length(links) + 1]] <- new_link(from, paste0("reactive_", i), "data")
}

# Right branch: Pattern B (direct input)
for (i in seq_len(n_blocks)) {
  blocks[[paste0("direct_", i)]] <- new_col_block_direct()
  from <- if (i == 1) "data" else paste0("direct_", i - 1)
  links[[length(links) + 1]] <- new_link(from, paste0("direct_", i), "data")
}

# =============================================================================
# Run
# =============================================================================
cat("=== State Pattern Benchmark ===\n\n")
cat("Two branches from the same dataset:\n")
cat("  Left:  Pattern A (reactiveVal)\n")
cat("  Right: Pattern B (direct input)\n\n")
cat("Each block selects ONE column, passed to next block.\n\n")
cat("TEST:\n")
cat("  1. Change column selection on BOTH sides\n")
cat("  2. Change the dataset at the top\n")
cat("  3. Compare: left preserves selection, right resets\n\n")

run_app(blocks = blocks, links = links)
