# Board scale-map adoption (convention: blockr.docs/patterns/scale-map.md).
# The mechanism lives in blockr.theme, consumed behind a Suggests guard:
# without blockr.theme, without a "scale_map" board option, or without a
# binding for the mapped variable, plots keep ggplot's default colors.
# ggplot `fill` consumes the `color` channel of a binding (channels are
# semantic; there is no separate fill binding).

# Build a "ggplot2::scale_*_manual(values = ...)" text fragment for `var`,
# or NULL when nothing applies. Resolved values are injected as literals so
# the block's expr stays a self-contained reproduction of what was shown.
# scale_*_manual() errors on missing levels, so only a complete assignment
# (every level resolved, e.g. pins + pool/board palette) is injected.
gg_scale_map_text <- function(session, data, var, aesthetic) {
  if (is.null(var) || identical(var, "(none)") || !nzchar(var)) {
    return(NULL)
  }
  if (!requireNamespace("blockr.theme", quietly = TRUE)) {
    return(NULL)
  }
  if (!is.data.frame(data) || !var %in% names(data)) {
    return(NULL)
  }

  map <- tryCatch(
    blockr.core::get_board_option_or_null("scale_map", session),
    error = function(e) NULL
  )
  if (is.null(map) || !length(map)) {
    return(NULL)
  }

  col <- data[[var]]
  lvls <- if (is.factor(col)) {
    levels(col)
  } else {
    lv <- unique(as.character(col))
    lv[!is.na(lv)]
  }
  if (!length(lvls)) {
    return(NULL)
  }

  res <- tryCatch(
    blockr.theme::resolve_scales(map, var, levels = lvls),
    error = function(e) NULL
  )
  vals <- res$color
  if (is.null(vals) || !all(lvls %in% names(vals))) {
    return(NULL)
  }

  fun <- switch(
    aesthetic,
    fill = "scale_fill_manual",
    colour = "scale_colour_manual"
  )
  pairs <- paste(
    sprintf('"%s" = "%s"', names(vals), unname(vals)),
    collapse = ", "
  )
  glue::glue("ggplot2::{fun}(values = c({pairs}))")
}
