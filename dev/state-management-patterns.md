# State Management Patterns in blockr

This document compares two approaches to managing state in blockr blocks.

## The Two Patterns

### Pattern A: reactiveVal (Recommended)

Used by: `blockr.core`, `blockr.dplyr`

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cols <- reactive(colnames(data()))

    # 1. Create reactiveVal initialized from constructor param
    r_x <- reactiveVal(x)

    # 2. Sync input changes TO reactiveVal
    observeEvent(input$x, r_x(input$x))

    # 3. When updating UI, read FROM reactiveVal
    observeEvent(cols(), {
      updateSelectInput(session, "x",
        choices = cols(),
        selected = r_x()
      )
    })

    # 4. Expression reads from reactiveVal
    list(
      expr = reactive(bquote(fn(.(val)), list(val = r_x()))),
      state = list(x = r_x)
    )
  })
}
```

**Data flow:**
```
Constructor param (x) --> reactiveVal(x) <-- observeEvent(input$x)
                                |
                                v
                          expr uses r_x()
                                |
                                v
                    state returns r_x (the reactiveVal)
```

### Pattern B: Direct Input (Not Recommended)

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cols <- reactive(colnames(data()))

    # 1. NO reactiveVal created

    # 2. When updating UI, use closure variable
    observeEvent(cols(), {
      updateSelectInput(session, "x",
        choices = cols(),
        selected = x
      )
    })

    # 3. Expression reads directly from input$
    list(
      expr = reactive(bquote(fn(.(val)), list(val = input$x))),
      state = list(x = reactive(input$x))
    )
  })
}
```

**Data flow:**
```
Constructor param (x) --> updateSelectInput(selected = x) --> input$x
                                                                 |
                                                                 v
                                                        expr uses input$x
                                                                 |
                                                                 v
                                             state returns reactive(input$x)
```

## Comparison

| Aspect | Pattern A (reactiveVal) | Pattern B (Direct Input) |
|--------|------------------------|-------------------------|
| Source of truth | `reactiveVal` (server) | `input$` (client) |
| Initial value | Stored in reactiveVal | Written to client via updateSelectInput |
| User changes | input -> reactiveVal via observeEvent | input is the state |
| UI updates | Read from reactiveVal | Read from closure variable |
| Lines of code | More | Less |

## Why Pattern A is Required

### 1. Headless/Serverless Execution

Pattern A: State lives in `reactiveVal` on the server. This enables:
- Evaluating expressions without a UI
- Batch processing pipelines
- API-driven block execution
- Server-side rendering

Pattern B: State lives in `input$` which **requires a connected browser client**. No client = no `input$` = no state = cannot evaluate expressions.

**This is a fundamental architectural limitation.**

### 2. Performance at Scale

Pattern A: State changes are **server-side memory operations**
```
input change --> reactiveVal update (in-memory) --> expression evaluates
```

Pattern B: State changes require **client round-trips**
```
input change --> updateSelectInput (server->client) --> input$ (client->server) --> expression
```

With 100 blocks, each with 10 inputs, Pattern B requires ~1000 round-trips vs ~1000 in-memory operations. The latency adds up.

### 3. Bug: User Selections Lost on Data Refresh

Pattern B has a concrete bug when upstream data changes.

- [Issue #70](https://github.com/BristolMyersSquibb/blockr.ggplot/issues/70) - Reproducible example
- [Video demonstration](https://app.clickup.com/4730439/chat/r/6-900601386711-8/t/80150025153628) - Side-by-side comparison showing the bug
- Run `dev/examples/benchmark_state_patterns.R` to reproduce locally

**Root cause:**
```r
# Pattern B - BUG
observeEvent(cols(), {
  updateSelectInput(session, "x",
    choices = cols(),
    selected = x
  )
})
```

The closure variable `x` is set at construction time and never updates. When `cols()` triggers, the selection resets to the original value, discarding user changes.

Pattern A doesn't have this bug because `selected = r_x()` reads the current value from the reactiveVal.

### 4. Testability

Pattern A allows unit testing with `testServer`:
```r
testServer(new_head_block(n = 10), {
  expect_equal(session$returned$state$n(), 10)
})
```

Pattern B requires a full Shiny session because `input$` doesn't exist without UI.

## Reference Implementation

See `blockr.core`'s `new_head_block()` in `R/transform-head.R`:

```r
new_head_block <- function(n = 6L, direction = c("head", "tail"), ...) {
  direction <- match.arg(direction)

  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        # reactiveVal initialized from constructor
        nrw <- reactiveVal(n)
        til <- reactiveVal(isTRUE(direction == "tail"))

        # Sync inputs to reactiveVals
        observeEvent(input$n, nrw(input$n))
        observeEvent(input$tail, til(input$tail))

        # Update UI reading from reactiveVal
        observeEvent(nrow(data()), {
          updateNumericInput(
            inputId = "n",
            value = nrw(),
            min = 1L,
            max = nrow(data())
          )
        })

        list(
          expr = reactive(
            if (isTRUE(til())) {
              bquote(utils::tail(data, n = .(n)), list(n = nrw()))
            } else {
              bquote(utils::head(data, n = .(n)), list(n = nrw()))
            }
          ),
          state = list(
            n = nrw,
            direction = reactive(if (isTRUE(til())) "tail" else "head")
          )
        )
      })
    },
    # ... UI function ...
  )
}
```

## Summary

| Argument | Strength |
|----------|----------|
| Testability | Moderate |
| Consistency across packages | Moderate |
| Bug: user selection lost on data refresh | Strong |
| **Headless execution impossible** | **Critical** |
| **Performance at scale** | **Strong** |

**Recommendation:** Always use Pattern A (reactiveVal) for blockr blocks.
