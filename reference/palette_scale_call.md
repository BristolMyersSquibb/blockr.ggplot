# Build a color-palette scale call for the theme block

Build a color-palette scale call for the theme block

## Usage

``` r
palette_scale_call(value, aesthetic)
```

## Arguments

- value:

  Palette selector: "auto" (no scale, returns NULL), "ggplot2" (reset to
  the default discrete scale), or a viridis option with a type suffix
  such as "viridis_d" / "magma_c".

- aesthetic:

  Either "fill" or "colour".

## Value

A language object (the scale call), or NULL when `value` is "auto".
