# Developer Documentation

This folder contains internal developer documentation for blockr.ggplot. These files are **not** included in the package build (excluded via `.Rbuildignore`).

## What's Here

### [blocks-core-guide.md](blocks-core-guide.md)
**Universal guide to blockr block development (start here)**

Core concepts applicable to all [blockr](https://github.com/BristolMyersSquibb/blockr) packages. Covers:
- What is a block? (universal concept)
- Block anatomy (UI, Server, Constructor)
- Step-by-step block creation
- Testing and validation
- [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) vignette references

**Use this when:**
- New to blockr development
- Understanding block fundamentals
- Learning universal patterns
- Creating blocks for any blockr package

### [ggplot-blocks-guide.md](ggplot-blocks-guide.md)
**ggplot2-specific patterns and extensions**

Package-specific guide for [blockr.ggplot](https://github.com/BristolMyersSquibb/blockr.ggplot). Covers:
- Expression building with `parse(text = glue::glue())`
- `"(none)"` pattern for optional aesthetics
- `allow_empty_state` parameter usage
- Column update patterns
- Complete ggplot examples
- ggplot-specific checklists and pitfalls

**Use this when:**
- Building ggplot visualization blocks
- Understanding ggplot-specific patterns
- Debugging ggplot block issues

### [ui-guidelines.md](ui-guidelines.md)
**UI development guidelines and design patterns**

Comprehensive guide to building consistent, professional block UIs. Covers:
- Responsive layout system (auto 1-4 column grid)
- Advanced/basic options toggle pattern
- Preview components with SVG
- Color palette and styling (minimalist gray/white)
- Complete working examples

**Use this when:**
- Designing block UI
- Implementing responsive layouts
- Adding advanced options
- Creating visual previews
- Ensuring consistent styling

## Quick Start for Developers

1. **Read** [blocks-core-guide.md](blocks-core-guide.md) to understand universal block concepts
2. **Read** [ggplot-blocks-guide.md](ggplot-blocks-guide.md) for ggplot-specific patterns
3. **Review** [ui-guidelines.md](ui-guidelines.md) for UI design patterns
4. **Study** existing blocks in `../R/` directory (e.g., `R/ggplot-block.R`)
5. **Test** your blocks thoroughly
6. **Validate** with the `blockr-validate-blocks` agent

## Key Principles

- **Parse/Glue:** Always use `parse(text = glue::glue())` for expressions
- **Optional Fields:** List in `allow_empty_state` parameter
- **State Management:** Include ALL constructor parameters in state list
- **No Type Filtering:** Use all `colnames(data())`, let ggplot2 handle validation

## Related Resources

- **Quick reference:** `../CLAUDE.md` (concise patterns and fixes)
- **Core concepts:** [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) vignettes (`create-block`, `extend-blockr`)
- **Examples:** `../inst/examples/` directory
- **Block code:** `../R/` directory

## For AI Assistants

This documentation is optimized for both human developers and AI code assistants:

- **[blocks-core-guide.md](blocks-core-guide.md)** - Universal patterns (reusable across blockr packages)
- **[ggplot-blocks-guide.md](ggplot-blocks-guide.md)** - ggplot-specific patterns
- **[ui-guidelines.md](ui-guidelines.md)** - UI design patterns

Each guide contains:
- Complete patterns and templates
- Step-by-step instructions
- Common pitfalls with solutions
- Quick reference sections

Start with the core guide, then apply package-specific patterns.
