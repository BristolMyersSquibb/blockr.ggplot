# Claude Development Notes for blockr.ggplot

> **📚 Complete guide:** [dev/writing-blocks.md](dev/writing-blocks.md)

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

See [dev/writing-blocks.md](dev/writing-blocks.md) for full guide and examples.
