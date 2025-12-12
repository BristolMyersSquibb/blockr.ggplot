# Line Plot Restoration Test
#
# Test for issue #75: line plot restoration with linetype aesthetic
# Run with: source("dev/examples/line_plot_restore_test.R")
#
# To test:
# 1. Run this script - a line plot should appear
# 2. Click "Store" in the UI to save the board state
# 3. Check that the JSON output doesn't have errors with linetype
# 4. Close and re-run with restore_from_store enabled (uncomment below)

library(blockr.core)
library(blockr.dock)

devtools::load_all(".")

# Serve line plot workflow using BOD dataset (Biochemical Oxygen Demand)
run_app(
    blocks = c(
      # Load data
      data = new_dataset_block("BOD", package = "datasets"),
      # Create line plot
      plot = new_ggplot_block(
        type = "line",
        x = "Time",
        y = "demand"
      )
    ),
    links = list(
      from = c("data"),
      to = c("plot"),
      input = c("data")
    )
)
