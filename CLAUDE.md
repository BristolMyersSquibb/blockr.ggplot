# Claude Development Notes for blockr.ggplot

> **📚 Developer guides:** [dev/README.md](dev/README.md)
> - Core concepts: [blocks-core-guide.md](dev/blocks-core-guide.md)
> - ggplot patterns: [ggplot-blocks-guide.md](dev/ggplot-blocks-guide.md)
> - UI guidelines: [ui-guidelines.md](dev/ui-guidelines.md)

## Must Follow

1. Build expressions as language objects with `bquote()` / `quote()` /
   `call()` / `as.call()` — never `parse(text = glue::glue())` or any
   string assembly. `as.name(col)` reproduces non-syntactic column names
   (no manual backticking); `gg_add()` folds a list of layers into a
   `a + b + c` chain. See `ggplot-block.R`, `facet-block.R`,
   `theme-block.R`, `grid-block.R`. `test-expr-golden.R` locks the exact
   emitted code for every branch — update it if you change output.
2. Expressions must set `expr_type = "bquoted"` and refer to the input as
   `.(data)` (i.e. `call(".", quote(data))`), never a bare `data`.
   Otherwise the generated code comes out as `with(list(data = plot),
   (data))` instead of `plot`. A passthrough is `quote(.(data))`.
3. Add optional fields to `allow_empty_state = c("color", "fill", ...)`
4. Include ALL constructor params in state list
5. Never add `Author:` or `Maintainer:` to DESCRIPTION

## Common Fixes

- Plot won't render → Add to `allow_empty_state`
- Object not found → Check `!= "(none)"` before using
- Inputs don't update → Use `observeEvent(colnames(data()))`

## More

See [dev/README.md](dev/README.md) for complete documentation structure.
