# UI Development Guidelines

This guide documents the UI patterns and design principles for blockr.ggplot blocks. Follow these guidelines to create consistent, professional, and responsive block interfaces.

## Table of Contents

1. [UI Philosophy](#ui-philosophy)
2. [Responsive Layout System](#responsive-layout-system)
3. [Advanced Options Toggle](#advanced-options-toggle)
4. [Two-Column Layout with Preview Sidebar](#two-column-layout-with-preview-sidebar)
5. [Preview Components](#preview-components)
6. [Color & Styling](#color--styling)
7. [Complete Examples](#complete-examples)
8. [Quick Reference](#quick-reference)

---

## UI Philosophy

### Core Principles

1. **Minimalist & Professional**: Clean, uncluttered interfaces with subtle styling
2. **Responsive**: Automatically adapts from narrow (1 column) to wide (4 columns)
3. **Consistent**: All inputs have uniform width and spacing
4. **Progressive Disclosure**: Hide advanced options by default
5. **Visual Feedback**: Show previews and status indicators where helpful

### Design Goals

- **Uniform Input Width**: All controls (select, slider, checkbox) have same width
- **Light Color Palette**: Gray/white tones, no flashy colors
- **Subtle Indicators**: Status colors used sparingly and with low opacity
- **Clean Typography**: Clear hierarchy with section headers
- **Below-Input Previews**: Visual feedback appears under related controls

---

## Responsive Layout System

### The Grid Pattern

Use the `block_responsive_css()` function for automatic responsive layout:

```r
ui <- function(id) {
  tagList(
    # Add responsive CSS
    block_responsive_css(),

    div(
      class = "block-container",
      div(
        class = "block-form-grid",

        # Your sections and inputs here
      )
    )
  )
}
```

### How It Works

**CSS Grid with Auto-Fit:**
```css
.block-form-grid {
  display: grid;
  gap: 15px;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
}
```

**Responsive Behavior:**
- **Narrow (< 250px)**: 1 column
- **Medium (250-500px)**: 1 column
- **Wide (500-750px)**: 2 columns
- **Very Wide (750-1000px)**: 3 columns
- **Extra Wide (> 1000px)**: 4 columns

**Key Features:**
- All inputs automatically share same grid tracks
- Uniform width across different input types
- No manual media queries needed
- Sections use `display: contents` to flatten structure

### Section Structure

```r
# Section with header
div(
  class = "block-section",
  tags$h4("Section Title"),
  div(
    class = "block-section-grid",

    # Inputs go here - each wrapped in block-input-wrapper
    div(
      class = "block-input-wrapper",
      selectInput(NS(id, "param"), "Label", choices = ...)
    ),

    div(
      class = "block-input-wrapper",
      sliderInput(NS(id, "value"), "Value", ...)
    )
  )
)
```

**CSS Behavior:**
- `.block-section` and `.block-section-grid` use `display: contents`
- This "flattens" them so inputs participate directly in parent grid
- Headers (`h4`) span full width: `grid-column: 1 / -1`
- Result: uniform input widths regardless of input type

### Help Text

```r
div(
  class = "block-help-text",
  "Explanatory text goes here"
)
```

Help text automatically:
- Spans full width
- Uses subtle gray color (#666)
- Has proper spacing from inputs above

---

## Advanced Options Toggle

### The Pattern

Hide advanced options behind a collapsible section with smooth animation:

```r
ui <- function(id) {
  tagList(
    # CSS for collapsible section
    tags$style(HTML(sprintf("
      #%s-advanced-options {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease-out;
        grid-column: 1 / -1;
        display: grid;
        grid-template-columns: subgrid;
        gap: 15px;
      }
      #%s-advanced-options.expanded {
        max-height: 2000px;
        overflow: visible;
        transition: max-height 0.5s ease-in;
      }
      .advanced-toggle {
        cursor: pointer;
        user-select: none;
        padding: 8px 0;
        display: flex;
        align-items: center;
        gap: 6px;
        grid-column: 1 / -1;
        color: #6c757d;
        font-size: 0.875rem;
      }
      .advanced-toggle .chevron {
        transition: transform 0.2s;
        display: inline-block;
        font-size: 14px;
        font-weight: bold;
      }
      .advanced-toggle .chevron.rotated {
        transform: rotate(90deg);
      }
    ", id, id))),

    # Toggle button
    div(
      class = "block-section",
      div(
        class = "advanced-toggle text-muted",
        id = NS(id, "advanced-toggle"),
        onclick = sprintf("
          const section = document.getElementById('%s');
          const chevron = document.querySelector('#%s .chevron');
          section.classList.toggle('expanded');
          chevron.classList.toggle('rotated');
        ", NS(id, "advanced-options"), NS(id, "advanced-toggle")),
        tags$span(class = "chevron", "\u203A"),  # Right chevron
        "Show advanced options"
      )
    ),

    # Collapsible section
    div(
      id = NS(id, "advanced-options"),

      # Advanced inputs go here
      div(
        class = "block-section",
        tags$h4("Advanced Settings"),
        div(
          class = "block-section-grid",
          # ... advanced inputs ...
        )
      )
    )
  )
}
```

### Key Features

- **Smooth Animation**: `max-height` transition (300ms collapse, 500ms expand)
- **Chevron Rotation**: Rotates 90° when expanded
- **Subgrid Layout**: Advanced section inherits parent grid
- **Full Width**: Spans all columns when expanded
- **Subtle Styling**: Gray text, small font

### Usage Guidelines

**Use for:**
- Non-essential customization options
- Advanced/expert settings
- Options that most users won't need

**Keep in basic section:**
- Required fields
- Core functionality
- Commonly used options

---

## Two-Column Layout with Preview Sidebar

### The Pattern

For blocks with previews or supplementary content, use a two-column layout: inputs (2/3 width) on left, preview/sidebar (1/3 width) on right. This layout is responsive and collapses to a single column on narrow screens.

### Structure

```r
ui <- function(id) {
  tagList(
    block_responsive_css(),

    # Two-column responsive CSS
    tags$style(HTML("
      /* Two-column layout - responsive wrapper */
      @media (min-width: 700px) {
        .my-layout-wrapper {
          display: grid;
          grid-template-columns: 2fr 1fr;
          gap: 20px;
          align-items: start;
        }
        .my-preview-sidebar {
          position: sticky;
          top: 20px;
        }
      }

      @media (max-width: 699px) {
        .my-layout-wrapper {
          display: block;
        }
        .my-preview-sidebar {
          margin-top: 20px;
        }
      }
    ")),

    div(
      class = "block-container",

      # Two-column wrapper
      div(
        class = "my-layout-wrapper",

        # Left: Inputs (2/3 width)
        div(
          class = "my-inputs",
          div(
            class = "block-form-grid",

            # All sections and inputs here
            div(
              class = "block-section",
              tags$h4("Options"),
              div(
                class = "block-section-grid",
                div(class = "block-input-wrapper",
                    selectInput(NS(id, "x"), "X-axis", ...)
                )
              )
            )
          )
        ),

        # Right: Preview/Sidebar (1/3 width)
        div(
          class = "my-preview-sidebar",
          uiOutput(NS(id, "preview"))
        )
      )
    )
  )
}
```

### Responsive Behavior

| Screen Width | Layout | Description |
|-------------|--------|-------------|
| **≥ 700px** | Side-by-side | Inputs (2/3) left, preview (1/3) right with sticky positioning |
| **< 700px** | Stacked | Inputs full-width on top, preview full-width below |

**Key CSS Features:**
- **Grid layout**: `grid-template-columns: 2fr 1fr` creates 2:1 ratio
- **Sticky preview**: `position: sticky; top: 20px` keeps preview visible while scrolling
- **Responsive gap**: `gap: 20px` for spacing between columns
- **Block fallback**: On narrow screens, wrapper uses `display: block` for stacking

### When to Use

Use the two-column layout when:
- ✅ Block has visual preview or supplementary information
- ✅ Preview is helpful but not essential for input selection
- ✅ Preview doesn't require full width to be useful
- ✅ You want preview visible while changing inputs (wide screens)

**Examples:**
- **Facet block**: Layout preview shows facet grid configuration
- **ggplot block**: Chart preview or aesthetic guidance
- **Plot grid block**: Grid arrangement preview

Don't use when:
- ❌ Preview requires full width (use below-input placement instead)
- ❌ Block has no preview/sidebar content
- ❌ Content is primarily vertical (tables, lists)

### Implementation Pattern

**Step 1: Create wrapper classes**
```r
# Use descriptive, unique class names
div(class = "facet-layout-wrapper",       # Wrapper
  div(class = "facet-inputs", ...),       # Left column
  div(class = "facet-preview-sidebar",    # Right column
    ...)
)
```

**Step 2: Add responsive CSS**
```css
/* Wide screens: side-by-side */
@media (min-width: 700px) {
  .{name}-layout-wrapper {
    display: grid;
    grid-template-columns: 2fr 1fr;
    gap: 20px;
    align-items: start;
  }
  .{name}-preview-sidebar {
    position: sticky;
    top: 20px;
  }
}

/* Narrow screens: stacked */
@media (max-width: 699px) {
  .{name}-layout-wrapper {
    display: block;
  }
  .{name}-preview-sidebar {
    margin-top: 20px;
  }
}
```

**Step 3: Wrap form grid**
```r
# The left column contains block-form-grid
div(
  class = "{name}-inputs",
  div(
    class = "block-form-grid",
    # All your existing sections
  )
)
```

### Naming Convention

Follow this naming pattern for consistency:

| Element | Class Name | Purpose |
|---------|-----------|----------|
| Wrapper | `.{name}-layout-wrapper` | Container for two columns |
| Left Column | `.{name}-inputs` | Contains form grid with inputs |
| Right Column | `.{name}-preview-sidebar` | Contains preview/supplementary content |

**Example:**
- Facet block: `.facet-layout-wrapper`, `.facet-inputs`, `.facet-preview-sidebar`
- ggplot block: `.ggplot-layout-wrapper`, `.ggplot-inputs`, `.ggplot-preview-sidebar`

### Complete Example

See the facet block implementation in [facet-block.R](../R/facet-block.R) for a complete working example with:
- Two-column responsive wrapper
- Sticky preview sidebar
- SVG preview with status indicators
- Proper HTML structure and closing tags

---

## Preview Components

### SVG Layout Previews

Visual previews show layout configurations below the related inputs.

#### Example: Grid/Facet Preview

```r
# In server function
output$layout_preview <- renderUI({
  preview <- create_preview_svg(...)  # Your preview logic

  tags$div(
    tags$div(
      class = "preview-svg-container",
      preview$svg
    ),
    tags$div(
      class = paste("preview-status", preview$status_class),
      preview$status_text
    )
  )
})

# In UI
div(
  class = "preview-container",
  uiOutput(NS(id, "layout_preview"))
)
```

#### Preview SVG Pattern

```r
create_preview_svg <- function(...) {
  # Calculate dimensions
  cell_size <- 60
  n_rows <- 2
  n_cols <- 3

  # Create SVG cells
  cells <- list()
  for (row in 0:(n_rows - 1)) {
    for (col in 0:(n_cols - 1)) {
      cells[[length(cells) + 1]] <- tags$rect(
        x = col * cell_size,
        y = row * cell_size,
        width = cell_size - 4,
        height = cell_size - 4,
        fill = "rgba(76, 175, 80, 0.3)",  # Light green
        stroke = "#4CAF50",                # Green
        `stroke-width` = "2",
        rx = "3"
      )
    }
  }

  svg <- tags$svg(
    width = n_cols * cell_size,
    height = n_rows * cell_size,
    style = "border: 1px solid #ddd; background: white; border-radius: 4px;",
    do.call(tagList, cells)
  )

  list(
    svg = svg,
    status_text = "2 × 3 grid (6 total)",
    status_class = "valid"
  )
}
```

### Preview CSS

```css
/* Preview container */
.preview-svg-container {
  text-align: center;
  margin-bottom: 8px;
}

/* Status text below preview */
.preview-status {
  font-size: 0.875rem;
  color: #6c757d;
  text-align: center;
  padding: 6px 8px;
  border-radius: 4px;
  background-color: #f8f9fa;
}

.preview-status.valid {
  color: #28a745;
}

.preview-status.warning {
  color: #ffc107;
}

.preview-status.error {
  color: #dc3545;
}
```

### Status Colors

Use subtle, low-opacity fills with solid strokes:

| Status | Fill | Stroke | When to Use |
|--------|------|--------|-------------|
| Valid (Green) | `rgba(76, 175, 80, 0.3)` | `#4CAF50` | Perfect configuration |
| Warning (Blue) | `rgba(33, 150, 243, 0.3)` | `#2196F3` | Works, but has empty slots |
| Error (Red) | `rgba(244, 67, 54, 0.3)` | `#f44336` | Invalid configuration |

### Preview Placement

**Always place previews:**
- ✅ **Below** the relevant inputs
- ✅ In a separate column or section
- ✅ With status text below the visual
- ❌ Not above inputs
- ❌ Not interrupting input flow

---

## Color & Styling

### Button Styles

**Use light/gray buttons for all controls:**

```r
# shinyWidgets buttons
shinyWidgets::radioGroupButtons(
  inputId = NS(id, "type"),
  label = "Chart Type",
  choices = c("Scatter", "Bar", "Line"),
  status = "light",  # ← Always use "light"
  size = "sm"
)
```

**Status colors available:**
- `status = "light"` → Gray/white (preferred for blockr)
- ❌ `status = "primary"` → Blue (too flashy)
- ❌ `status = "success"` → Green (too flashy)
- ❌ `status = "warning"` → Orange (too flashy)

### Color Palette

**UI Elements (Minimalist Grays):**
```css
/* Primary text */
color: #333;

/* Secondary/muted text */
color: #6c757d;

/* Help text */
color: #666;

/* Borders */
border-color: #ddd;

/* Backgrounds */
background: #f8f9fa;
background: white;
```

**Status Colors (Subtle Usage):**
```css
/* Success/Valid - Green */
color: #28a745;          /* text */
fill: rgba(76, 175, 80, 0.3);  /* SVG fill */
stroke: #4CAF50;         /* SVG stroke */

/* Warning - Blue */
color: #ffc107;
fill: rgba(33, 150, 243, 0.3);
stroke: #2196F3;

/* Error - Red */
color: #dc3545;
fill: rgba(244, 67, 54, 0.3);
stroke: #f44336;
```

### Typography

```css
/* Section headers */
h4 {
  font-size: 1.1rem;
  font-weight: 600;
  color: #333;
  margin-top: 5px;
  margin-bottom: 0px;
}

/* Help text */
.block-help-text {
  font-size: 0.875rem;
  color: #666;
}

/* Advanced toggle */
.advanced-toggle {
  font-size: 0.875rem;
  color: #6c757d;
}

/* Preview status */
.preview-status {
  font-size: 0.875rem;
  color: #6c757d;
}
```

### Spacing & Layout

```css
/* Grid gap between inputs */
gap: 15px;

/* Section spacing */
.block-section:not(:first-child) {
  margin-top: 20px;
}

/* Input margins */
.block-input-wrapper .form-group {
  margin-bottom: 10px;
}

/* Container padding */
.block-container {
  padding-bottom: 15px;
}
```

---

## Complete Examples

### Example 1: Basic Block with Responsive Layout

```r
new_my_block <- function(x = character(), y = character(), ...) {
  new_ggplot_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        # ... server logic ...
      })
    },
    ui = function(id) {
      tagList(
        block_responsive_css(),

        div(
          class = "block-container",
          div(
            class = "block-form-grid",

            # Main Section
            div(
              class = "block-section",
              tags$h4("Basic Options"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  selectInput(NS(id, "x"), "X-axis", choices = x)
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(NS(id, "y"), "Y-axis", choices = y)
                )
              )
            )
          )
        )
      )
    },
    class = "my_block",
    ...
  )
}
```

### Example 2: Block with Advanced Options

```r
ui <- function(id) {
  tagList(
    block_responsive_css(),

    # Advanced toggle CSS
    tags$style(HTML(sprintf("
      #%s-advanced-options {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease-out;
        grid-column: 1 / -1;
        display: grid;
        grid-template-columns: subgrid;
        gap: 15px;
      }
      #%s-advanced-options.expanded {
        max-height: 2000px;
        transition: max-height 0.5s ease-in;
      }
      .advanced-toggle {
        cursor: pointer;
        padding: 8px 0;
        display: flex;
        align-items: center;
        gap: 6px;
        grid-column: 1 / -1;
        color: #6c757d;
        font-size: 0.875rem;
      }
      .advanced-toggle .chevron {
        transition: transform 0.2s;
      }
      .advanced-toggle .chevron.rotated {
        transform: rotate(90deg);
      }
    ", id, id))),

    div(
      class = "block-container",
      div(
        class = "block-form-grid",

        # Basic Section
        div(
          class = "block-section",
          tags$h4("Basic Options"),
          div(
            class = "block-section-grid",
            div(class = "block-input-wrapper",
                selectInput(NS(id, "x"), "X-axis", choices = ...)
            )
          )
        ),

        # Advanced Toggle
        div(
          class = "block-section",
          div(
            class = "advanced-toggle",
            id = NS(id, "advanced-toggle"),
            onclick = sprintf("
              const section = document.getElementById('%s');
              const chevron = document.querySelector('#%s .chevron');
              section.classList.toggle('expanded');
              chevron.classList.toggle('rotated');
            ", NS(id, "advanced-options"), NS(id, "advanced-toggle")),
            tags$span(class = "chevron", "\u203A"),
            "Show advanced options"
          )
        ),

        # Advanced Section (Collapsible)
        div(
          id = NS(id, "advanced-options"),
          div(
            class = "block-section",
            tags$h4("Advanced Options"),
            div(
              class = "block-section-grid",
              div(class = "block-input-wrapper",
                  sliderInput(NS(id, "alpha"), "Transparency", ...)
              )
            )
          )
        )
      )
    )
  )
}
```

### Example 3: Block with Preview

```r
ui <- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    block_responsive_css(),

    # Preview CSS
    tags$style(HTML("
      .preview-svg-container {
        text-align: center;
        margin-bottom: 8px;
      }
      .preview-status {
        font-size: 0.875rem;
        color: #6c757d;
        text-align: center;
        padding: 6px 8px;
        border-radius: 4px;
        background-color: #f8f9fa;
      }
      .preview-status.valid { color: #28a745; }
      .preview-status.warning { color: #ffc107; }
      .preview-status.error { color: #dc3545; }
    ")),

    div(
      class = "block-container",
      div(
        class = "block-form-grid",

        # Inputs Section
        div(
          class = "block-section",
          tags$h4("Layout"),
          div(
            class = "block-section-grid",
            div(class = "block-input-wrapper",
                selectInput(NS(id, "ncol"), "Columns", ...)
            ),
            div(class = "block-input-wrapper",
                selectInput(NS(id, "nrow"), "Rows", ...)
            )
          )
        ),

        # Preview Section (below inputs)
        div(
          class = "block-section",
          tags$h4("Preview"),
          div(
            class = "block-section-grid",
            div(
              style = "grid-column: 1 / -1;",
              uiOutput(NS(id, "layout_preview"))
            )
          )
        )
      )
    )
  )
}
```

---

## Quick Reference

### CSS Classes

| Class | Purpose | Behavior |
|-------|---------|----------|
| `.block-container` | Outer wrapper | Padding, margins |
| `.block-form-grid` | Main grid | Responsive columns |
| `.block-section` | Section wrapper | `display: contents` |
| `.block-section-grid` | Section grid | `display: contents` |
| `.block-input-wrapper` | Input wrapper | Full width |
| `.block-help-text` | Help text | Full width, gray |
| `.advanced-toggle` | Toggle button | Clickable, animated |
| `.preview-svg-container` | SVG preview | Centered |
| `.preview-status` | Status text | Colored, padded |

### Required Functions

```r
# Add responsive CSS
block_responsive_css()

# Namespace inputs
NS(id, "input_name")
```

### Grid Behavior

- **1 column**: Width < 250px
- **2 columns**: Width 250-750px
- **3 columns**: Width 750-1000px
- **4 columns**: Width > 1000px

### Color Reference

**Buttons:** Always `status = "light"`

**Status Colors:**
- Green: `#4CAF50` / `rgba(76, 175, 80, 0.3)`
- Blue: `#2196F3` / `rgba(33, 150, 243, 0.3)`
- Red: `#f44336` / `rgba(244, 67, 54, 0.3)`

**UI Grays:**
- Primary: `#333`
- Secondary: `#6c757d`
- Help: `#666`
- Border: `#ddd`
- Background: `#f8f9fa`

### Typography Scale

- Headers: `1.1rem`, `font-weight: 600`
- Help/Status: `0.875rem`
- Toggle: `0.875rem`

### Animation Timings

- Collapse: `300ms ease-out`
- Expand: `500ms ease-in`
- Chevron: `200ms`

---

## Best Practices Summary

✅ **Do:**
- Use `block_responsive_css()` for all blocks
- Keep buttons `status = "light"`
- Place previews below inputs
- Use subtle colors (low opacity fills)
- Hide advanced options by default
- Maintain uniform input widths

❌ **Don't:**
- Use flashy button colors
- Place previews above inputs
- Use custom responsive code (use the grid)
- Mix different width systems
- Show all options at once
- Use bright, saturated colors

---

## Related Documentation

- **Block Development:** [writing-blocks.md](writing-blocks.md)
- **Quick Reference:** [../CLAUDE.md](../CLAUDE.md)
- **Example Blocks:**
  - [facet-block.R](../R/facet-block.R) - Advanced toggle + preview
  - [plot-grid-block.R](../R/plot-grid-block.R) - Grid preview
  - [ggplot-block.R](../R/ggplot-block.R) - Dynamic UI

---

**Remember:** The goal is a clean, professional, minimalist interface that adapts gracefully to any width. Let the responsive grid system do the work!
