# Developer Documentation

This folder contains internal developer documentation for blockr.ggplot. These files are **not** included in the package build (excluded via `.Rbuildignore`).

## What's Here

### [writing-blocks.md](writing-blocks.md)
**Complete guide to creating blockr.ggplot blocks**

Essential reading for anyone developing new blocks. Covers:
- Block anatomy (UI, Server, Constructor)
- Step-by-step block creation
- blockr.ggplot-specific patterns (parse/glue, optional fields, state management)
- Complete working examples
- Comprehensive checklists
- Common pitfalls and solutions
- Testing and validation

**Use this when:**
- Creating a new block
- Understanding blockr architecture
- Debugging block issues
- Learning blockr.ggplot patterns

## Quick Start for Developers

1. **Read** [writing-blocks.md](writing-blocks.md) to understand block architecture
2. **Study** existing blocks in `R/` directory (e.g., `R/ggplot-block.R`)
3. **Follow** the patterns and checklists
4. **Test** your blocks thoroughly
5. **Validate** with the `blockr-validate-blocks` agent

## Key Principles

- **Parse/Glue:** Always use `parse(text = glue::glue())` for expressions
- **Optional Fields:** List in `allow_empty_state` parameter
- **State Management:** Include ALL constructor parameters in state list
- **No Type Filtering:** Use all `colnames(data())`, let ggplot2 handle validation

## Related Resources

- **Quick reference:** `../CLAUDE.md` (concise patterns and fixes)
- **Core concepts:** blockr.core vignettes (`create-block`, `extend-blockr`)
- **Examples:** `../inst/examples/` directory
- **Block code:** `../R/` directory

## For AI Assistants

This documentation is optimized for both human developers and AI code assistants. The [writing-blocks.md](writing-blocks.md) guide contains:
- Complete patterns and templates
- Step-by-step instructions
- Common pitfalls with solutions
- Quick reference sections

Use this as your primary reference when working on blockr.ggplot blocks.
