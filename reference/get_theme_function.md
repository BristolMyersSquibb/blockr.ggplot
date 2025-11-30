# Get theme function call for a given theme name

Maps theme names to their corresponding ggplot2 function calls. Returns
a fallback theme if the requested theme's package is not available.

## Usage

``` r
get_theme_function(theme_name)
```

## Arguments

- theme_name:

  Character string naming the theme

## Value

Character string with the theme function call
