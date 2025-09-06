#!/usr/bin/env Rscript

# Test the updated chart_block with visual icons
library(blockr.ggplot)
library(blockr.core)

# Create a simple test app
app <- serve(
  new_chart_block(
    type = "point",
    x = "wt",
    y = "mpg",
    color = "cyl"
  ),
  data = list(data = mtcars)
)

cat("Chart block with visual selector is running!\n")
cat("Check the UI to see the new icon-based chart type selector.\n")