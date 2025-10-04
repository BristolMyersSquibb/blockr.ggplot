# Writing blockr.ggplot Blocks: Developer Guide

This guide explains how to create new blocks for blockr.ggplot. It's designed for both human developers and AI code assistants.

## Table of Contents

1. [What is a Block?](#what-is-a-block)
2. [Block Anatomy](#block-anatomy)
3. [Step-by-Step: Creating a Block](#step-by-step-creating-a-block)
4. [blockr.ggplot-Specific Patterns](#blockrggplot-specific-patterns)
5. [Complete Example](#complete-example)
6. [Block Completeness Checklist](#block-completeness-checklist)
7. [Common Pitfalls & Solutions](#common-pitfalls--solutions)
8. [Testing & Validation](#testing--validation)

---

## What is a Block?

A **block** is a Shiny module that performs a single task in a data analysis workflow. In blockr.ggplot, blocks create ggplot2 visualizations.

Blocks can be connected in a **DAG** (directed acyclic graph) to create powerful workflows. A block receives data from upstream blocks, performs its operation, and passes results to downstream blocks.

**Key characteristics:**
- Built on Shiny modules
- Returns two special values: `expr` (expression) and `state` (reactive values)
- Self-contained: UI + Server + Constructor
- Composable: Can be linked with other blocks

---

## Block Anatomy

Every block consists of **three components**:

### 1. UI Function

Defines the user interface using standard Shiny UI functions.

```r
ui <- function(id) {
  tagList(
    selectInput(
      NS(id, "x"),
      label = "X-axis",
      choices = x,
      selected = x
    ),
    selectInput(
      NS(id, "color"),
      label = "Color By",
      choices = c("(none)", color),
      selected = if (length(color) == 0) "(none)" else color
    )
  )
}
```

**Requirements:**
- Must take single `id` argument
- Use `NS(id, "input_name")` for all input IDs
- Return `shiny.tag` or `shiny.tag.list` objects
- Use `tagList()` to combine multiple UI elements

### 2. Server Function

Handles reactive logic and returns `expr` and `state`.

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    r_x <- reactiveVal(x)
    r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)

    # Update reactive values from inputs
    observeEvent(input$x, r_x(input$x))
    observeEvent(input$color, r_color(input$color))

    # Update UI when data changes
    observeEvent(colnames(data()), {
      updateSelectInput(session, "x", choices = colnames(data()), selected = r_x())
      updateSelectInput(session, "color", choices = c("(none)", colnames(data())), selected = r_color())
    })

    # Return expr and state
    list(
      expr = reactive({
        # Build expression (see Expression Building section)
        text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {r_x()}))")
        parse(text = text)[[1]]
      }),
      state = list(
        x = r_x,
        color = r_color
      )
    )
  })
}
```

**Key points:**
- Takes `id` and data inputs (`data` for transform blocks)
- Returns `moduleServer()` call
- Must return `list(expr = reactive(...), state = list(...))`
- State names must match constructor parameters

### 3. Constructor Function

Wraps UI and server, initializes the block.

```r
new_my_plot_block <- function(x = character(), color = character(), ...) {
  new_ggplot_block(
    server = server,
    ui = ui,
    class = "my_plot_block",
    allow_empty_state = c("color"),  # List optional fields!
    ...
  )
}
```

**Requirements:**
- Expose all UI-controllable parameters as arguments
- DO NOT expose data inputs (handled by server)
- Call appropriate `new_*_block()` constructor
- Forward `...` to parent constructor
- List optional fields in `allow_empty_state`

---

## Step-by-Step: Creating a Block

### Step 1: Define Parameters

Identify what the user should control:

```r
new_scatter_plot_block <- function(
  x = character(),           # Required aesthetic
  y = character(),           # Required aesthetic
  color = character(),       # Optional aesthetic
  size = character(),        # Optional aesthetic
  alpha = 1,                 # Fixed parameter
  ...
)
```

### Step 2: Create UI Function

```r
ui <- function(id) {
  div(class = "row",
    div(class = "col-md-6",
      h4("Scatter Plot Configuration"),
      selectInput(NS(id, "x"), "X-axis", choices = x, selected = x),
      selectInput(NS(id, "y"), "Y-axis", choices = y, selected = y),
      selectInput(NS(id, "color"), "Color By",
                  choices = c("(none)", color),
                  selected = if (length(color) == 0) "(none)" else color)
    ),
    div(class = "col-md-6",
      sliderInput(NS(id, "alpha"), "Transparency",
                  min = 0, max = 1, value = alpha, step = 0.1)
    )
  )
}
```

### Step 3: Create Server Function

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # 1. Initialize reactive values
    r_x <- reactiveVal(x)
    r_y <- reactiveVal(y)
    r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
    r_alpha <- reactiveVal(alpha)

    # 2. Update reactive values from inputs
    observeEvent(input$x, r_x(input$x))
    observeEvent(input$y, r_y(input$y))
    observeEvent(input$color, r_color(input$color))
    observeEvent(input$alpha, r_alpha(input$alpha))

    # 3. Update column-dependent inputs when data changes
    observeEvent(colnames(data()), {
      cols <- colnames(data())
      updateSelectInput(session, "x", choices = cols, selected = r_x())
      updateSelectInput(session, "y", choices = cols, selected = r_y())
      updateSelectInput(session, "color",
                       choices = c("(none)", cols),
                       selected = r_color())
    })

    # 4. Build expression
    list(
      expr = reactive({
        # Build aesthetics conditionally
        aes_parts <- c(
          glue::glue("x = {r_x()}"),
          glue::glue("y = {r_y()}")
        )
        if (r_color() != "(none)") {
          aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
        }
        aes_text <- paste(aes_parts, collapse = ", ")

        # Build complete plot expression
        plot_text <- glue::glue(
          "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
          "ggplot2::geom_point(alpha = {r_alpha()})"
        )
        parse(text = plot_text)[[1]]
      }),
      state = list(
        x = r_x,
        y = r_y,
        color = r_color,
        alpha = r_alpha
      )
    )
  })
}
```

### Step 4: Create Constructor

```r
new_scatter_plot_block <- function(
  x = character(),
  y = character(),
  color = character(),
  alpha = 1,
  ...
) {
  new_ggplot_block(
    server = server,
    ui = ui,
    class = "scatter_plot_block",
    allow_empty_state = c("color"),  # color can be "(none)"
    ...
  )
}
```

### Step 5: Add Documentation

```r
#' Create a scatter plot block
#'
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Optional column for color aesthetic
#' @param alpha Transparency (0-1)
#' @param ... Forwarded to \code{\link[blockr.core]{new_plot_block}}
#' @export
```

---

## blockr.ggplot-Specific Patterns

### Expression Building: parse() + glue()

**ALWAYS use `parse(text = glue::glue())` pattern - NEVER use bquote()**

```r
# ✅ CORRECT - Use parse/glue
expr = reactive({
  text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes(x = {r_x()}))")
  parse(text = text)[[1]]
})

# ❌ WRONG - Don't use bquote
expr = reactive({
  bquote(ggplot2::ggplot(data, ggplot2::aes(x = .(x))), list(x = as.name(r_x())))
})
```

**Why parse/glue?**
- More readable code
- Easier conditional logic
- Better debugging (can print the text)

### Reactive Value Naming

- Use `r_*` prefix: `r_x`, `r_color`, `r_alpha`
- Use simple input IDs: `"x"`, `"color"` (not `"xcol"` or `"colorcol"`)
- Input IDs must match across UI and server

### Handling Optional Fields

Optional fields use the `"(none)"` pattern:

```r
# 1. Initialize with "(none)" if empty
r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)

# 2. Include "(none)" in choices
selectInput(NS(id, "color"), "Color By",
           choices = c("(none)", color),
           selected = if (length(color) == 0) "(none)" else color)

# 3. Check before using in expression
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}

# 4. CRITICAL: List in allow_empty_state
new_my_block(..., allow_empty_state = c("color"))
```

### Update Pattern for Column Choices

Only update column-dependent inputs in `observeEvent(colnames(data()))`:

```r
observeEvent(colnames(data()), {
  cols <- colnames(data())

  # Update column-based selectInputs
  updateSelectInput(session, "x", choices = cols, selected = r_x())
  updateSelectInput(session, "color",
                   choices = c("(none)", cols),
                   selected = r_color())

  # DON'T update non-column inputs here:
  # - Static selectInputs (position = c("stack", "dodge"))
  # - TextInputs, CheckboxInputs, SliderInputs
  # These only need: initial UI value + observeEvent for updates
})
```

### State List Requirements

**ALL reactive values MUST be in state list:**

```r
state = list(
  x = r_x,              # Required field
  y = r_y,              # Required field
  color = r_color,      # Optional field
  fill = r_fill,        # Optional field
  alpha = r_alpha,      # Parameter
  position = r_position # Parameter
  # Every constructor argument must be here!
)
```

### allow_empty_state Parameter

**CRITICAL:** Any field using `"(none)"` pattern MUST be in `allow_empty_state`:

```r
new_my_block <- function(x = character(), color = character(), fill = character(), ...) {
  new_ggplot_block(
    # ...
    allow_empty_state = c("color", "fill"),  # All optional aesthetics!
    ...
  )
}
```

**Why?** blockr.core blocks evaluation when state fields are empty. `allow_empty_state` tells the framework which fields can be `"(none)"`.

---

## Complete Example

Here's a complete, production-ready block:

```r
#' Create a box plot block
#'
#' @param x Column for x-axis (categories)
#' @param y Column for y-axis (values)
#' @param fill Optional column for fill aesthetic
#' @param color Optional column for color aesthetic
#' @param alpha Transparency (0-1)
#' @param ... Forwarded to \code{\link[blockr.core]{new_plot_block}}
#' @export
new_boxplot_block <- function(
  x = character(),
  y = character(),
  fill = character(),
  color = character(),
  alpha = 1,
  ...
) {
  ui <- function(id) {
    div(class = "row m-3",
      div(class = "col-md-6",
        h4("Box Plot Configuration"),
        selectInput(NS(id, "x"), "X-axis (Categories)",
                   choices = x, selected = x),
        selectInput(NS(id, "y"), "Y-axis (Values)",
                   choices = y, selected = y),
        selectInput(NS(id, "fill"), "Fill By",
                   choices = c("(none)", fill),
                   selected = if (length(fill) == 0) "(none)" else fill),
        selectInput(NS(id, "color"), "Color By",
                   choices = c("(none)", color),
                   selected = if (length(color) == 0) "(none)" else color)
      ),
      div(class = "col-md-6",
        h4("Appearance"),
        sliderInput(NS(id, "alpha"), "Transparency",
                   min = 0, max = 1, value = alpha, step = 0.1)
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # Initialize reactive values
      r_x <- reactiveVal(x)
      r_y <- reactiveVal(y)
      r_fill <- reactiveVal(if (length(fill) == 0) "(none)" else fill)
      r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)
      r_alpha <- reactiveVal(alpha)

      # Update from inputs
      observeEvent(input$x, r_x(input$x))
      observeEvent(input$y, r_y(input$y))
      observeEvent(input$fill, r_fill(input$fill))
      observeEvent(input$color, r_color(input$color))
      observeEvent(input$alpha, r_alpha(input$alpha))

      # Update column choices
      observeEvent(colnames(data()), {
        cols <- colnames(data())
        updateSelectInput(session, "x", choices = cols, selected = r_x())
        updateSelectInput(session, "y", choices = cols, selected = r_y())
        updateSelectInput(session, "fill",
                         choices = c("(none)", cols), selected = r_fill())
        updateSelectInput(session, "color",
                         choices = c("(none)", cols), selected = r_color())
      })

      # Build expression
      list(
        expr = reactive({
          # Validate required fields
          req(r_x(), r_y())

          # Build aesthetics
          aes_parts <- c(
            glue::glue("x = {r_x()}"),
            glue::glue("y = {r_y()}")
          )
          if (r_fill() != "(none)") {
            aes_parts <- c(aes_parts, glue::glue("fill = {r_fill()}"))
          }
          if (r_color() != "(none)") {
            aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
          }
          aes_text <- paste(aes_parts, collapse = ", ")

          # Build plot
          plot_text <- glue::glue(
            "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
            "ggplot2::geom_boxplot(alpha = {r_alpha()})"
          )
          parse(text = plot_text)[[1]]
        }),
        state = list(
          x = r_x,
          y = r_y,
          fill = r_fill,
          color = r_color,
          alpha = r_alpha
        )
      )
    })
  }

  new_ggplot_block(
    server = server,
    ui = ui,
    class = "boxplot_block",
    allow_empty_state = c("fill", "color"),
    ...
  )
}
```

---

## Block Completeness Checklist

Use this before considering a block complete:

### UI/UX Standards
- [ ] Bootstrap grid layout (`div(class="row")`, `div(class="col-md-6")`)
- [ ] Section header (`h4("Block Name Configuration")`)
- [ ] Organized controls (aesthetics together, options together)
- [ ] Professional styling (`class="m-3"` for margins)
- [ ] Help text for complex functionality (`helpText()`)

### Technical Implementation
- [ ] Required field validation with `req()`
- [ ] Modern `"(none)"` pattern for optional aesthetics
- [ ] Complete state (ALL constructor parameters)
- [ ] Reactive values with `r_*` prefix
- [ ] Proper initialization with `"(none)"` defaults
- [ ] Column-dependent inputs update in `observeEvent(colnames(data()))`
- [ ] Expression building uses `glue::glue()` pattern
- [ ] NO column type filtering (use all `colnames(data())`)
- [ ] `allow_empty_state` lists ALL optional fields

### Feature Completeness
- [ ] All relevant aesthetics for plot type
- [ ] Common aesthetics (`color`, `linewidth` where relevant)
- [ ] Geom-specific aesthetics (stroke, fill, width, etc.)
- [ ] Advanced options (position, flip, etc.)
- [ ] Sensible defaults

### Documentation
- [ ] Complete `@param` documentation
- [ ] Descriptive UI labels
- [ ] Consistent parameter naming
- [ ] Help text for non-obvious behavior

### Testing
- [ ] Constructor tests (basic, with parameters, with all parameters)
- [ ] All tests pass (`devtools::test()`)

---

## Common Pitfalls & Solutions

### Issue: Plot doesn't render despite correct UI

**Symptom:** UI appears, inputs work, but plot area is blank.

**Cause:** Missing `allow_empty_state` for optional fields.

**Solution:**
```r
new_my_block(..., allow_empty_state = c("color", "fill", "y"))  # List ALL optional fields
```

### Issue: "object not found" errors in expression

**Cause:** Using `"(none)"` value in ggplot aesthetic without checking.

**Solution:**
```r
# Always check before adding to aesthetics
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}
```

### Issue: Inputs don't update when data changes

**Cause:** Forgot to observe data changes or using wrong reactive.

**Solution:**
```r
observeEvent(colnames(data()), {  # Use colnames(data()), not data()
  updateSelectInput(session, "x", choices = colnames(data()), selected = r_x())
})
```

### Issue: State not persisting/restoring

**Cause:** State list doesn't include all constructor parameters.

**Solution:**
```r
# Every parameter in constructor MUST be in state
state = list(x = r_x, y = r_y, color = r_color, alpha = r_alpha)
```

### Issue: Column filtering breaks dropdown

**Cause:** Filtering columns by type (numeric_cols, factor_cols).

**Solution:**
```r
# DON'T filter by type - let ggplot2 handle validation
updateSelectInput(session, "x", choices = colnames(data()))  # All columns!
```

---

## Testing & Validation

### Manual Testing

Test your block in isolation:

```r
library(blockr.core)
library(blockr.ggplot)

serve(
  new_my_plot_block(x = "wt", y = "mpg", color = "cyl"),
  data = list(data = mtcars)
)
```

### Screenshot Validation

**ALWAYS use the `blockr-validate-blocks` agent for validation:**

The validation agent:
- Generates screenshots of your block with realistic test data
- Shows both UI and rendered plot
- Identifies common issues automatically
- Creates comprehensive validation reports

**Working block:** Screenshot shows UI + plot
**Broken block:** Screenshot shows only UI, no plot

### Unit Tests

Every block needs comprehensive tests:

```r
test_that("my_plot_block constructor", {
  # Basic constructor
  blk <- new_my_plot_block()
  expect_s3_class(blk, c("my_plot_block", "ggplot_block", "plot_block", "block"))

  # With parameters
  blk <- new_my_plot_block(x = "col1", y = "col2")
  expect_s3_class(blk, "my_plot_block")

  # With all parameters
  blk <- new_my_plot_block(x = "col1", y = "col2", color = "col3", alpha = 0.5)
  expect_s3_class(blk, "my_plot_block")
})
```

Run tests: `devtools::test()`

---

## Additional Resources

- **blockr.core vignettes:** Core concepts and advanced patterns
  - `vignette("create-block", package = "blockr.core")`
  - `vignette("extend-blockr", package = "blockr.core")`

- **Example blocks:** Study existing implementations in `R/` directory
  - `R/ggplot-block.R` - Universal ggplot block with type selection
  - Individual geom blocks for specific patterns

- **CLAUDE.md:** Detailed technical notes and troubleshooting

- **blockr.dplyr:** Reference for transform block patterns

---

## Quick Reference Card

### Block Structure Template

```r
new_my_block <- function(param1 = default1, param2 = default2, ...) {
  ui <- function(id) {
    tagList(
      selectInput(NS(id, "param1"), "Label", choices = param1, selected = param1)
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_param1 <- reactiveVal(param1)
      observeEvent(input$param1, r_param1(input$param1))

      list(
        expr = reactive({
          text <- glue::glue("expression_here")
          parse(text = text)[[1]]
        }),
        state = list(param1 = r_param1)
      )
    })
  }

  new_ggplot_block(server, ui, class = "my_block", ...)
}
```

### Key Rules

1. **Expression:** Use `parse(text = glue::glue())`
2. **Optional fields:** Use `"(none)"` + `allow_empty_state`
3. **State:** Include ALL constructor parameters
4. **Updates:** Only column inputs in `observeEvent(colnames(data()))`
5. **Naming:** `r_*` for reactive values, simple names for input IDs
6. **Validation:** Use `req()` for required fields
7. **Testing:** Write tests + use validation agent
