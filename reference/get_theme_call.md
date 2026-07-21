# Get the base theme call for a given theme name

Maps theme names to their corresponding ggplot2 (or add-on package)
function calls, returned as language objects. Falls back to
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
when the requested theme's package is not available.

## Usage

``` r
get_theme_call(theme_name)
```

## Arguments

- theme_name:

  Character string naming the theme

## Value

A language object (the theme function call), or NULL in "auto" mode
(keep the upstream theme).
