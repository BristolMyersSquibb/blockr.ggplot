# Claude Development Notes for blockr.ggplot

> **📚 Developer guides:** [dev/README.md](dev/README.md)
> - Core concepts: [blocks-core-guide.md](dev/blocks-core-guide.md)
> - ggplot patterns: [ggplot-blocks-guide.md](dev/ggplot-blocks-guide.md)
> - UI guidelines: [ui-guidelines.md](dev/ui-guidelines.md)

## Must Follow

1. Use `parse(text = glue::glue())` for expressions
2. Add optional fields to `allow_empty_state = c("color", "fill", ...)`
3. Include ALL constructor params in state list
4. Never add `Author:` or `Maintainer:` to DESCRIPTION

## Common Fixes

- Plot won't render → Add to `allow_empty_state`
- Object not found → Check `!= "(none)"` before using
- Inputs don't update → Use `observeEvent(colnames(data()))`

## More

See [dev/README.md](dev/README.md) for complete documentation structure.
