#!/usr/bin/env Rscript

#' Example usage of validate_block_screenshot function
#'
#' This script demonstrates how to use the validate_block_screenshot function
#' to test blockr blocks. This can be directly copied and used by LLMs or
#' developers to validate new block implementations.

# Load blockr.ggplot (assumes it's installed or use devtools::load_all())
library(blockr.ggplot)

# Example 1: Simple validation with default data (mtcars)
cat("=== Example 1: Simple scatter plot validation ===\n")
result <- validate_block_screenshot(
  new_chart_block(type = "point", x = "wt", y = "mpg", color = "cyl"),
  filename = "test-scatter.png"
)

if (result$success) {
  cat(sprintf("✓ Success! Screenshot saved to: %s\n", result$path))
} else {
  cat(sprintf("✗ Failed: %s\n", result$error))
}

# Example 2: Custom data with bar chart
cat("\n=== Example 2: Bar chart with iris data ===\n")
result <- validate_block_screenshot(
  new_chart_block(type = "bar", x = "Species", fill = "Species"),
  data = iris,
  filename = "test-iris-bar.png"
)

if (result$success) {
  cat(sprintf("✓ Success! Screenshot saved to: %s\n", result$path))
} else {
  cat(sprintf("✗ Failed: %s\n", result$error))
}

# Example 3: Testing a complex block configuration
cat("\n=== Example 3: Complex histogram with multiple aesthetics ===\n")
result <- validate_block_screenshot(
  new_chart_block(
    type = "histogram",
    x = "mpg",
    fill = "cyl",
    bins = 20,
    alpha = 0.7,
    position = "identity"
  ),
  filename = "test-complex-histogram.png"
)

if (result$success) {
  cat(sprintf("✓ Success! Screenshot saved to: %s\n", result$path))
} else {
  cat(sprintf("✗ Failed: %s\n", result$error))
}

# Example 4: Batch validation of multiple blocks
cat("\n=== Example 4: Batch validation of multiple blocks ===\n")
blocks <- list(
  scatter = new_chart_block(type = "point", x = "wt", y = "mpg"),
  bar = new_chart_block(type = "bar", x = "cyl"),
  line = new_chart_block(type = "line", x = "wt", y = "mpg"),
  boxplot = new_chart_block(type = "boxplot", x = "cyl", y = "mpg")
)

results <- validate_blocks_batch(blocks)
print(results)

# Example 5: Testing a potentially broken block (for debugging)
cat("\n=== Example 5: Testing edge case - empty parameters ===\n")
tryCatch({
  result <- validate_block_screenshot(
    new_chart_block(type = "point"),  # Minimal parameters
    filename = "test-minimal.png"
  )
  
  if (result$success) {
    cat(sprintf("✓ Block works even with minimal parameters!\n"))
  } else {
    cat(sprintf("✗ Block failed with minimal parameters: %s\n", result$error))
  }
}, error = function(e) {
  cat(sprintf("✗ Error creating block: %s\n", e$message))
})

# Example 6: Custom output directory
cat("\n=== Example 6: Custom output directory ===\n")
custom_dir <- tempdir()
result <- validate_block_screenshot(
  new_chart_block(type = "density", x = "mpg", fill = "cyl"),
  filename = "test-density.png",
  output_dir = custom_dir
)

if (result$success) {
  cat(sprintf("✓ Screenshot saved to custom directory: %s\n", result$path))
} else {
  cat(sprintf("✗ Failed: %s\n", result$error))
}

# Example 7: Function that can be directly called by LLM
cat("\n=== Example 7: LLM-friendly function call ===\n")
# This is a simple one-liner that an LLM can easily generate and modify:
validate_new_block <- function() {
  # LLM can modify this block specification as needed
  block <- new_chart_block(
    type = "violin",
    x = "cyl",
    y = "mpg",
    fill = "cyl",
    alpha = 0.8
  )
  
  # Validate and return result
  result <- validate_block_screenshot(block, filename = "llm-test-block.png")
  
  # Simple success/failure message
  if (result$success) {
    return(sprintf("✓ Block validated successfully! Screenshot: %s", result$path))
  } else {
    return(sprintf("✗ Block validation failed: %s", result$error))
  }
}

# Run the LLM-friendly function
output <- validate_new_block()
cat(output, "\n")

cat("\n=== All examples completed ===\n")
cat("Check the 'man/figures' directory for generated screenshots.\n")