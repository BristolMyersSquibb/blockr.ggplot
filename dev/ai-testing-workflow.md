# AI-Assisted Testing Workflow for blockr.ggplot

**A guide for humans on how to work with AI (Claude Code) to write comprehensive test suites**

Based on successful testing workflow from blockr.dplyr.

---

## TL;DR

1. **Use Claude Code LOCALLY** - Remote/PR workflows don't work (need interactivity)
2. **Point AI to documentation** - Tell it to read `/dev/` guides and study blockr.dplyr tests
3. **Work on ONE block at a time** - Start small, iterate
4. **Test every single input** - Tell AI to verify each UI element affects output correctly
5. **Debug together** - When stuck, use testServer to analyze reactive values step-by-step

---

## Why Local Claude Code?

### ❌ What Doesn't Work: PR/Remote Workflow

Mike tried this THREE times:
- [PR #45](https://github.com/BristolMyersSquibb/blockr.ggplot/pull/45) - Claude Code Web
- [PR #59](https://github.com/BristolMyersSquibb/blockr.ggplot/pull/59) - Claude Code Web
- [PR #60](https://github.com/BristolMyersSquibb/blockr.ggplot/pull/60) - Agentic Copilot

**Result:** More than half the tests failed. Would take more time to debug than to write from scratch.

### ✅ What Works: Local Interactive Workflow

**Use local Claude Code** because:
- **You need to redirect AI every few minutes** - No prompt is perfect
- **AI needs to iterate** - Try test → fails → investigate → fix → repeat
- **You need real-time discussion** - "Is this test wrong or is the implementation wrong?"

**Christoph's promise:** "I can build you a full test suite in 1 to 2 hours, with every input tested."

The difference? **Pair programming with AI locally**, not fire-and-forget PR generation.

---

## Step-by-Step Workflow

### Step 1: Initial Setup Prompt

Start by pointing AI to the right resources:

```
I want to write comprehensive tests for blockr.ggplot blocks.

1. Read the documentation in /dev/ carefully:
   - blocks-core-guide.md - Core block concepts
   - ggplot-blocks-guide.md - ggplot-specific patterns
   - testing-guide.md - Testing patterns and testServer usage
   - ui-guidelines.md - UI patterns

2. Study blockr.dplyr tests - there are 700+ tests that test ALL inputs:
   - Located at ~/git/blockr/blockr.dplyr/tests/testthat/
   - These show the pattern we want to follow
   - Use testServer to test every input element
   - Verify each input affects the output correctly

3. You can also reference blockr.core for core patterns

4. IMPORTANT: We use testServer for testing, NOT shinytest2
   - testServer can test UI interactions via session$setInputs()
   - It's fast and doesn't require a browser
   - Only use shinytest2 for visual regression (very rare)

Ready? Let's start with a single block.
```

### Step 2: Choose ONE Block to Start

**Don't try to test everything at once.** Pick a single, simple block:

```
Let's start by writing tests for the scatter plot (geom_point)
variant of ggplot_block.

Focus on testing:
- Constructor works
- Expression generation includes geom_point()
- ALL input elements (x, y, color, size, alpha, etc.)
- Each input affects the output correctly

Work on just this one chart type first. We'll iterate on others later.
```

**Why one block?** If AI tries to do everything, it will:
- Generate boilerplate that doesn't actually test anything
- Miss edge cases
- Create tests that pass but don't verify correctness

### Step 3: Test Every Single Input

This is the key insight from blockr.dplyr. **Every UI element needs a test.**

```
For the scatter plot block, I want you to test EVERY input:

1. Chart type selector (make sure "point" generates geom_point)
2. X-axis column selector
3. Y-axis column selector
4. Color aesthetic (including "(none)")
5. Size aesthetic (including "(none)")
6. Shape aesthetic
7. Alpha slider
8. Any other UI elements

For EACH input:
- Use testServer with session$setInputs() to change it
- Verify the expression changes correctly
- Verify the output plot has the expected property

Show me tests for the first 3 inputs, then we'll review before continuing.
```

**Why show first 3?** Catch pattern problems early before generating 50 tests.

### Step 4: When Tests Fail - Debug Together

When you run the tests and some fail:

```
I ran the tests and test-ggplot-scatter.R is failing on line 47.

The test expects color aesthetic in the expression, but it's not there.

Can you help me debug this?

Use testServer to:
1. Check what session$returned$state$color() actually contains
2. Check what the expr() reactive generates
3. Trace through the reactive chain to see where color gets lost

Is the test wrong, or is the implementation wrong?
```

AI can use testServer to inspect:
- Reactive values at each step
- Expression generation
- State management
- Input/output flow

### Step 5: Discuss Implementation Issues

If the **implementation** is wrong (not the test):

```
It looks like the implementation doesn't handle color = "(none)" correctly.

The block should:
- Initialize r_color with "(none)" if color is empty
- Only add color to aesthetics if r_color() != "(none)"
- Include "color" in allow_empty_state parameter

But it's currently [describe what's actually happening].

Let's discuss the fix before I implement it. What do you think?
```

**Don't let AI blindly fix the implementation.** You understand the domain - AI helps with the mechanics.

### Step 6: Scale Up with "Do This for All"

Once **one** input test works perfectly, you can scale up:

```
Perfect! The test for the color aesthetic works great.

Now do this for ALL aesthetics:
- size
- shape
- alpha
- fill

Use the exact same pattern as the color test.
```

Once **one** block works perfectly, scale to all blocks:

```
Great! Scatter plot tests are all passing and cover every input.

Now do this for ALL chart types:
- bar charts (geom_bar/geom_col)
- line plots
- box plots
- histograms
- density plots
- violin plots

Use the same pattern for each.
```

**Why this works:**
- AI has a **concrete working example** to copy
- Pattern is proven to work in your codebase
- Less room for AI to make wrong assumptions
- Much faster than explaining each variation

**Pro tip:** Once you have one test file working perfectly, you can even say:

```
Now create test files for ALL remaining blocks following this exact pattern.
```

AI will replicate the structure reliably because it has a working template.

---

## Key Phrases to Use with AI

### Good Prompts

✅ **"Read the /dev/ documentation first"** - Points AI to your guides

✅ **"Study the blockr.dplyr tests as examples"** - Shows the pattern to follow

✅ **"Work on ONE block at a time"** - Prevents overwhelming boilerplate

✅ **"Test EVERY input element"** - Ensures comprehensive coverage

✅ **"Show me the first 3 tests"** - Catch problems early

✅ **"Use testServer to debug this"** - Leverages AI's ability to trace reactivity

✅ **"Is the test wrong or the implementation wrong?"** - Forces analysis

✅ **"Let's discuss the fix first"** - Maintains your control

### Bad Prompts

❌ **"Write a comprehensive test suite"** - Too vague, AI will generate shallow tests

❌ **"Test all the blocks"** - Too broad, AI will create boilerplate

❌ **"Make this test pass"** - AI might fix the test instead of finding the bug

❌ **"Just fix it"** - Bypasses the learning/validation step

---

## Common Patterns AI Should Follow

### Pattern 1: Constructor Test

```r
test_that("scatter plot block constructor", {
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")
  expect_s3_class(blk, c("ggplot_block", "plot_block", "block"))
})
```

### Pattern 2: Expression Generation Test

```r
test_that("scatter plot generates geom_point expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(blk$expr_server, args = list(data = input_data), {
    session$flushReact()

    expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
    expect_true(grepl("geom_point", expr_str))
    expect_true(grepl("x = mpg", expr_str))
    expect_true(grepl("y = hp", expr_str))
  })
})
```

### Pattern 3: Input Interaction Test

```r
test_that("changing color aesthetic updates expression", {
  input_data <- reactive(mtcars)
  blk <- new_ggplot_block(type = "point", x = "mpg", y = "hp", color = "(none)")

  testServer(blk$expr_server, args = list(data = input_data), {
    session$flushReact()

    # Initially no color
    expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
    expect_false(grepl("colour =", expr_str))

    # Change color to "cyl"
    session$setInputs(color = "cyl")
    session$flushReact()

    # Now should have color
    expr_str <- paste0(deparse(session$returned$expr()), collapse = "")
    expect_true(grepl("colour = cyl", expr_str))
    expect_equal(session$returned$state$color(), "cyl")
  })
})
```

### Pattern 4: Output Verification Test

```r
test_that("scatter plot creates valid ggplot object", {
  block <- new_ggplot_block(type = "point", x = "mpg", y = "hp")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    args = list(x = block, data = list(data = function() mtcars)),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_s3_class(result, "ggplot")
      expect_equal(class(result$layers[[1]]$geom)[1], "GeomPoint")
      expect_true("x" %in% names(result$mapping))
      expect_true("y" %in% names(result$mapping))
    }
  )
})
```

---

## Advanced: Debugging Reactive Flow

When things get complex, use testServer to trace the entire reactive chain:

```
The test is failing and I'm not sure why.

Can you use testServer to trace through ALL the reactive values:

1. What does input$type contain after setInputs?
2. What does r_type() reactive return?
3. What does session$returned$state$type() return?
4. What does session$returned$expr() generate?
5. At which step does the value get lost or change unexpectedly?

Show me the debugging code using testServer to inspect each step.
```

Example debugging session:

```r
testServer(blk$expr_server, args = list(data = input_data), {
  session$flushReact()

  # Inspect initial state
  cat("Initial type:", session$returned$state$type(), "\n")
  cat("Initial expr:", deparse(session$returned$expr()), "\n")

  # Change input
  session$setInputs(type = "bar")
  session$flushReact()

  # Inspect after change
  cat("After setInputs type:", session$returned$state$type(), "\n")
  cat("After setInputs expr:", deparse(session$returned$expr()), "\n")

  # Now we can see where the problem is!
})
```

This is described in Shiny documentation on testServer - AI can help write these debugging traces.

---

## What to Expect

### Time Investment

- **First block (scatter):** 30-45 minutes (learning the pattern)
- **Subsequent blocks:** 10-15 minutes each (pattern is established)
- **Total for full suite:** 1-2 hours

### Coverage

Following this workflow should give you:
- ✅ 100% of inputs tested
- ✅ Expression generation verified
- ✅ Output correctness verified
- ✅ Edge cases covered (empty, "(none)", missing data)
- ✅ State management tested
- ✅ Reactive updates tested

### Comparison to PR Workflow

- **PR workflow:** 3+ attempts, 50%+ test failures, hours of debugging
- **Local workflow:** 1-2 hours, all tests pass, complete coverage

---

## Summary

**For humans working with AI on test suites:**

1. **Use local Claude Code** - You need constant interaction
2. **Point to docs and examples** - blockr.dplyr has 700+ test examples
3. **Start small** - One block, one input at a time
4. **Review early** - First 3 tests set the pattern
5. **Debug together** - Use testServer to trace reactive flow
6. **Discuss fixes** - You decide implementation, AI helps mechanics
7. **Iterate systematically** - Expand once pattern works

**The key insight:** Testing with AI is **pair programming**, not **prompt-and-pray**.

You provide domain knowledge and direction. AI provides:
- Code generation speed
- Pattern consistency
- Debugging assistance
- Comprehensive coverage

Together, you can build a complete test suite in hours that would take days alone.

---

## References

- **Technical guide:** See `testing-guide.md` for testServer patterns and technical details
- **Block patterns:** See `blocks-core-guide.md` and `ggplot-blocks-guide.md`
- **Examples:** Study `~/git/blockr/blockr.dplyr/tests/testthat/` for 700+ working tests
- **Shiny docs:** [testServer documentation](https://shiny.rstudio.com/reference/shiny/latest/testServer.html)

---

**Ready to start? Open Claude Code locally and use the prompts above!**
