# ggplot2 Blocks: Package-Specific Guide

**ggplot-specific patterns and extensions for [blockr.ggplot](https://github.com/BristolMyersSquibb/blockr.ggplot)**

> **📚 Prerequisites:** Read [blocks-core-guide.md](blocks-core-guide.md) first for universal block concepts

This guide covers ggplot2-specific patterns for building visualization blocks in blockr.ggplot.

## Table of Contents

1. [Expression Building: parse() + glue()](#expression-building-parse--glue)
2. [Handling Optional Aesthetics](#handling-optional-aesthetics)
3. [Column Updates Pattern](#column-updates-pattern)
4. [Complete ggplot Example](#complete-ggplot-example)
5. [ggplot Block Checklist](#ggplot-block-checklist)
6. [Common ggplot Pitfalls](#common-ggplot-pitfalls)
7. [Quick Reference](#quick-reference)

---

## Expression Building: parse() + glue()

### The Mandatory Pattern

**ALWAYS use `parse(text = glue::glue())` - NEVER use bquote()**

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

### Why parse/glue?

1. **Ecosystem Consistency**: All [blockr](https://github.com/BristolMyersSquibb/blockr) packages use this pattern
2. **More Readable**: Code looks like actual R code
3. **Easier Conditionals**: Simple if/else for building expressions
4. **Better Debugging**: Can `print()` or `cat()` the generated text

### Building Complex Expressions

**Conditional Aesthetics:**

```r
expr = reactive({
  # Build aesthetics conditionally
  aes_parts <- c(glue::glue("x = {r_x()}"), glue::glue("y = {r_y()}"))

  if (r_color() != "(none)") {
    aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
  }

  if (r_size() != "(none)") {
    aes_parts <- c(aes_parts, glue::glue("size = {r_size()}"))
  }

  aes_text <- paste(aes_parts, collapse = ", ")

  # Build complete plot
  plot_text <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_point()")
  parse(text = plot_text)[[1]]
})
```

**With Geom Arguments:**

```r
expr = reactive({
  # Aesthetics
  aes_text <- glue::glue("x = {r_x()}, y = {r_y()}")

  # Geom arguments (static, not in aes)
  geom_args <- glue::glue("alpha = {r_alpha()}, size = {r_size_fixed()}")

  # Compose
  plot_text <- glue::glue(
    "ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ",
    "ggplot2::geom_point({geom_args})"
  )
  parse(text = plot_text)[[1]]
})
```

**Adding Layers Conditionally:**

```r
expr = reactive({
  base_plot <- glue::glue("ggplot2::ggplot(data, ggplot2::aes({aes_text})) + ggplot2::geom_point()")

  # Add smooth layer if requested
  if (r_add_smooth()) {
    plot_text <- glue::glue("({base_plot}) + ggplot2::geom_smooth()")
  } else {
    plot_text <- base_plot
  }

  parse(text = plot_text)[[1]]
})
```

---

## Handling Optional Aesthetics

### The "(none)" Pattern

Optional aesthetics (color, fill, size, etc.) use a special `"(none)"` pattern:

**4-Step Implementation:**

```r
# 1. Initialize with "(none)" if empty
r_color <- reactiveVal(if (length(color) == 0) "(none)" else color)

# 2. Include "(none)" in UI choices
selectInput(
  NS(id, "color"),
  "Color By",
  choices = c("(none)", color),
  selected = if (length(color) == 0) "(none)" else color
)

# 3. Check before using in expression
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}

# 4. CRITICAL: List in allow_empty_state
new_my_block(..., allow_empty_state = c("color", "fill", "size"))
```

### allow_empty_state Parameter

**Why it's critical:**

[blockr.core](https://github.com/BristolMyersSquibb/blockr.core) blocks block evaluation when ANY state field is empty or invalid. The `allow_empty_state` parameter tells [blockr](https://github.com/BristolMyersSquibb/blockr) which fields are allowed to be `"(none)"`.

```r
new_my_ggplot_block <- function(
  x = character(),
  y = character(),
  color = character(),
  fill = character(),
  ...
) {
  new_ggplot_block(
    server = server,
    ui = ui,
    class = "my_ggplot_block",
    # List ALL optional aesthetics that use "(none)" pattern
    allow_empty_state = c("color", "fill"),
    ...
  )
}
```

**Rule:** If a field can be `"(none)"`, it MUST be in `allow_empty_state`

### Reactive Value Naming

Conventions for ggplot blocks:

- **Reactive values:** `r_*` prefix (`r_x`, `r_color`, `r_alpha`)
- **Input IDs:** Simple names (`"x"`, `"color"`, not `"xcol"` or `"colorcol"`)
- **Must match:** Input IDs must match observable names in server

---

## Column Updates Pattern

### Update Only Column-Dependent Inputs

**DON'T update all inputs when data changes. ONLY update column-based selectInputs:**

```r
observeEvent(colnames(data()), {
  cols <- colnames(data())

  # ✅ Update column-based selectInputs
  updateSelectInput(session, "x", choices = cols, selected = r_x())
  updateSelectInput(session, "y", choices = c("(none)", cols), selected = r_y())
  updateSelectInput(session, "color", choices = c("(none)", cols), selected = r_color())

  # ❌ DON'T update these here:
  # - Static selectInputs (position with fixed choices)
  # - TextInputs (title, labels)
  # - CheckboxInputs (flip_coords, show_legend)
  # - SliderInputs (alpha, size)
  # These only need: initial UI value + observeEvent for updates
})
```

### No Column Type Filtering

**Critical Rule: Use ALL columns, let ggplot2 handle validation**

```r
# ✅ CORRECT - All columns
updateSelectInput(session, "x", choices = colnames(data()))

# ❌ WRONG - Don't filter by type
numeric_cols <- names(data())[sapply(data(), is.numeric)]
updateSelectInput(session, "x", choices = numeric_cols)  # NO!
```

**Why?** ggplot2 can handle type mismatches gracefully and provides better error messages. Pre-filtering hides columns users might legitimately want to use.

---

## Complete ggplot Example

Production-ready boxplot block with all patterns:

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
    tagList(
      block_responsive_css(),

      div(
        class = "block-container",
        div(
          class = "block-form-grid",

          # Main Section
          div(
            class = "block-section",
            tags$h4("Box Plot Configuration"),
            div(
              class = "block-section-grid",
              div(class = "block-input-wrapper",
                  selectInput(NS(id, "x"), "X-axis (Categories)",
                             choices = x, selected = x)
              ),
              div(class = "block-input-wrapper",
                  selectInput(NS(id, "y"), "Y-axis (Values)",
                             choices = y, selected = y)
              ),
              div(class = "block-input-wrapper",
                  selectInput(NS(id, "fill"), "Fill By",
                             choices = c("(none)", fill),
                             selected = if (length(fill) == 0) "(none)" else fill)
              ),
              div(class = "block-input-wrapper",
                  selectInput(NS(id, "color"), "Color By",
                             choices = c("(none)", color),
                             selected = if (length(color) == 0) "(none)" else color)
              )
            )
          ),

          # Appearance Section
          div(
            class = "block-section",
            tags$h4("Appearance"),
            div(
              class = "block-section-grid",
              div(class = "block-input-wrapper",
                  sliderInput(NS(id, "alpha"), "Transparency",
                             min = 0, max = 1, value = alpha, step = 0.1)
              )
            )
          )
        )
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

      # Update column choices when data changes
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

          # Build aesthetics conditionally
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

          # Build plot with parse/glue
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
    allow_empty_state = c("fill", "color"),  # Both optional aesthetics
    ...
  )
}
```

---

## ggplot Block Checklist

### ggplot-Specific Requirements

- [ ] Expression uses `parse(text = glue::glue())` pattern (NO bquote)
- [ ] Optional aesthetics use `"(none)"` pattern
- [ ] ALL optional aesthetics listed in `allow_empty_state`
- [ ] Check `!= "(none)"` before adding to aesthetics
- [ ] Column updates only in `observeEvent(colnames(data()))`
- [ ] NO column type filtering (use all `colnames(data())`)
- [ ] Reactive values use `r_*` prefix
- [ ] State list includes ALL constructor parameters

### Aesthetic Standards

**Core aesthetics by geom:**

| Geom | Required | Optional |
|------|----------|----------|
| geom_point | x, y | color, shape, size, alpha, fill |
| geom_col/bar | x | y, fill, color, alpha, position |
| geom_line | x, y | color, linetype, alpha, group |
| geom_boxplot | x, y | fill, color, alpha |
| geom_violin | x, y | fill, color, alpha |
| geom_density | x | fill, color, alpha |
| geom_area | x, y | fill, color, alpha |

### UI Standards (see ui-guidelines.md)

- [ ] Use responsive grid layout (`block_responsive_css()`)
- [ ] Bootstrap classes for sections
- [ ] Light/gray button colors (`status = "light"`)
- [ ] Professional, minimalist styling

---

## Common ggplot Pitfalls

### Issue: Plot doesn't render

**Symptom:** UI appears, inputs work, but plot area is blank

**Cause:** Missing `allow_empty_state` for optional aesthetics

**Solution:**
```r
new_my_block(..., allow_empty_state = c("color", "fill", "size"))  # ALL optional!
```

### Issue: "object not found" errors

**Cause:** Using `"(none)"` in ggplot aesthetic without checking

**Solution:**
```r
# Always check before adding
if (r_color() != "(none)") {
  aes_parts <- c(aes_parts, glue::glue("colour = {r_color()}"))
}
```

### Issue: Dropdown becomes empty

**Cause:** Filtering columns by type

**Solution:**
```r
# Use ALL columns
updateSelectInput(session, "x", choices = colnames(data()))
```

### Issue: Inputs don't update with new data

**Cause:** Using wrong reactive trigger

**Solution:**
```r
# Use colnames(data()), not data()
observeEvent(colnames(data()), {
  updateSelectInput(session, "x", choices = colnames(data()), selected = r_x())
})
```

---

## Additional Resources

- **Core concepts:** [blocks-core-guide.md](blocks-core-guide.md)
- **UI patterns:** [ui-guidelines.md](ui-guidelines.md)
- **Quick tips:** [../CLAUDE.md](../CLAUDE.md)
- **Examples:** `../R/ggplot-block.R`, individual geom blocks
