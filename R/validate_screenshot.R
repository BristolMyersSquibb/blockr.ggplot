#' Validate a blockr block by generating a screenshot
#'
#' This function creates a temporary Shiny app with the provided block,
#' takes a screenshot, and returns the result. It's designed to be a
#' simple, direct way to test whether a block implementation works correctly.
#'
#' @param block A blockr block object (e.g., from new_chart_block())
#' @param data Data to use for the block (default: mtcars)
#' @param filename Name for the screenshot file (default: auto-generated)
#' @param output_dir Directory to save screenshot (default: "man/figures")
#' @param width Screenshot width in pixels (default: 800)
#' @param height Screenshot height in pixels (default: 600)
#' @param delay Seconds to wait for app to load (default: 5)
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return A list with components:
#'   - success: Logical indicating if screenshot was created successfully
#'   - path: Full path to the screenshot file (NULL if failed)
#'   - error: Error message if failed (NULL if successful)
#'   - filename: Name of the screenshot file
#'
#' @examples
#' \dontrun{
#' # Simple usage with default data (mtcars)
#' result <- validate_block_screenshot(
#'   new_chart_block(type = "point", x = "wt", y = "mpg")
#' )
#'
#' # With custom data
#' result <- validate_block_screenshot(
#'   new_chart_block(type = "bar", x = "Species"),
#'   data = iris,
#'   filename = "iris-bar-chart.png"
#' )
#'
#' # Check if successful
#' if (result$success) {
#'   cat("Screenshot saved to:", result$path)
#' } else {
#'   cat("Failed:", result$error)
#' }
#'
#' # Test a new block implementation
#' my_block <- new_chart_block(
#'   type = "histogram",
#'   x = "mpg",
#'   bins = 20,
#'   fill = "cyl"
#' )
#' result <- validate_block_screenshot(my_block, filename = "test-histogram.png")
#' }
#'
#' @export
validate_block_screenshot <- function(
  block,
  data = mtcars,
  filename = NULL,
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 5,
  verbose = TRUE
) {
  # Check dependencies
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    return(list(
      success = FALSE,
      path = NULL,
      error = "webshot2 package is required. Install with: install.packages('webshot2')",
      filename = filename
    ))
  }

  if (!requireNamespace("blockr.core", quietly = TRUE)) {
    return(list(
      success = FALSE,
      path = NULL,
      error = "blockr.core package is required",
      filename = filename
    ))
  }

  # Auto-generate filename if not provided
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    block_class <- class(block)[1]
    filename <- sprintf("%s_%s.png", block_class, timestamp)
  }

  # Ensure filename has .png extension
  if (!grepl("\\.png$", filename)) {
    filename <- paste0(filename, ".png")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Full output path
  output_path <- file.path(output_dir, filename)

  if (verbose) {
    cat(sprintf(
      "Generating screenshot for block of class '%s'...\n",
      class(block)[1]
    ))
  }

  # Wrap data in the expected list format
  if (!is.list(data) || !("data" %in% names(data))) {
    data_list <- list(data = data)
  } else {
    data_list <- data
  }

  # Try to create the screenshot
  result <- tryCatch(
    {
      # Create temporary directory for the app
      temp_dir <- tempfile("blockr_validation_")
      dir.create(temp_dir)

      # Save data to RDS file to avoid deparse issues
      saveRDS(data_list, file.path(temp_dir, "data.rds"))

      # Save block to RDS file to avoid deparse issues
      saveRDS(block, file.path(temp_dir, "block.rds"))

      # Create minimal app.R file
      app_content <- sprintf(
        '
library(blockr.core)

# Load the blockr.ggplot package
# Try to load from development first, fall back to installed version
tryCatch(
  devtools::load_all("%s"),
  error = function(e) {
    library(blockr.ggplot)
  }
)

# Load data and block
data <- readRDS("data.rds")
block <- readRDS("block.rds")

# Run the app
blockr.core::serve(
  block,
  data = data
)
        ',
        normalizePath(".")
      )

      writeLines(app_content, file.path(temp_dir, "app.R"))

      # Increase timeout for Shiny app launching
      old_timeout <- getOption("webshot.app.timeout", 60)
      options(webshot.app.timeout = 120)
      on.exit(options(webshot.app.timeout = old_timeout), add = TRUE)

      # Take screenshot
      webshot2::appshot(
        app = temp_dir,
        file = output_path,
        vwidth = width,
        vheight = height,
        delay = delay
      )

      # Cleanup
      unlink(temp_dir, recursive = TRUE)

      # Check if file was created
      if (file.exists(output_path)) {
        if (verbose) {
          cat(sprintf("✓ Screenshot saved to: %s\n", output_path))
        }

        list(
          success = TRUE,
          path = normalizePath(output_path),
          error = NULL,
          filename = filename
        )
      } else {
        list(
          success = FALSE,
          path = NULL,
          error = "Screenshot file was not created",
          filename = filename
        )
      }
    },
    error = function(e) {
      # Cleanup on error
      if (exists("temp_dir") && dir.exists(temp_dir)) {
        unlink(temp_dir, recursive = TRUE)
      }

      if (verbose) {
        cat(sprintf("✗ Failed to create screenshot: %s\n", e$message))
      }

      list(
        success = FALSE,
        path = NULL,
        error = e$message,
        filename = filename
      )
    }
  )

  return(result)
}

#' Batch validate multiple blocks with screenshots
#'
#' Convenience function to validate multiple blocks at once and generate
#' a summary report of which blocks work and which don't.
#'
#' @param blocks Named list of blocks to validate
#' @param data Data to use for all blocks (can also be a named list matching block names)
#' @param output_dir Directory to save screenshots (default: "man/figures")
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return A data frame with validation results for each block
#'
#' @examples
#' \dontrun{
#' # Test multiple blocks
#' blocks <- list(
#'   scatter = new_chart_block(type = "point", x = "wt", y = "mpg"),
#'   bar = new_chart_block(type = "bar", x = "cyl"),
#'   histogram = new_chart_block(type = "histogram", x = "mpg")
#' )
#'
#' results <- validate_blocks_batch(blocks)
#' print(results)
#'
#' # With different data for each block
#' blocks <- list(
#'   iris_scatter = new_chart_block(type = "point", x = "Sepal.Length", y = "Sepal.Width"),
#'   mtcars_bar = new_chart_block(type = "bar", x = "cyl")
#' )
#'
#' data_list <- list(
#'   iris_scatter = iris,
#'   mtcars_bar = mtcars
#' )
#'
#' results <- validate_blocks_batch(blocks, data = data_list)
#' }
#'
#' @export
validate_blocks_batch <- function(
  blocks,
  data = mtcars,
  output_dir = "man/figures",
  verbose = TRUE
) {
  if (!is.list(blocks)) {
    stop("blocks must be a list")
  }

  # Get block names
  block_names <- names(blocks)
  if (is.null(block_names)) {
    block_names <- paste0("block_", seq_along(blocks))
    names(blocks) <- block_names
  }

  # Prepare data for each block
  if (is.list(data) && !is.data.frame(data)) {
    # data is a named list
    data_list <- data
  } else {
    # data is a single dataset, use for all blocks
    data_list <- setNames(
      rep(list(data), length(blocks)),
      block_names
    )
  }

  # Validate each block
  results <- lapply(block_names, function(name) {
    if (verbose) {
      cat(sprintf("\nValidating block '%s'...\n", name))
    }

    block_data <- if (name %in% names(data_list)) {
      data_list[[name]]
    } else {
      data # fallback to default data
    }

    result <- validate_block_screenshot(
      block = blocks[[name]],
      data = block_data,
      filename = paste0(name, ".png"),
      output_dir = output_dir,
      verbose = verbose
    )

    data.frame(
      block_name = name,
      success = result$success,
      screenshot = ifelse(result$success, result$filename, NA),
      error = ifelse(is.null(result$error), "", result$error),
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  results_df <- do.call(rbind, results)

  if (verbose) {
    cat("\n=== Validation Summary ===\n")
    cat(sprintf("Total blocks: %d\n", nrow(results_df)))
    cat(sprintf("Successful: %d\n", sum(results_df$success)))
    cat(sprintf("Failed: %d\n", sum(!results_df$success)))

    if (any(!results_df$success)) {
      cat("\nFailed blocks:\n")
      failed <- results_df[!results_df$success, ]
      for (i in seq_len(nrow(failed))) {
        cat(sprintf("  - %s: %s\n", failed$block_name[i], failed$error[i]))
      }
    }
  }

  return(results_df)
}
