#' Universal ggplot block with selectable visualization types
#'
#' A flexible block that allows users to select from various ggplot2 geoms
#' and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "point"). Options: "point", "bar",
#'   "line", "boxplot", "violin", "density", "area", "histogram", "pie"
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color aesthetic
#' @param fill Column for fill aesthetic
#' @param size Column for size aesthetic
#' @param shape Column for shape aesthetic
#' @param linetype Column for linetype aesthetic
#' @param group Column for group aesthetic
#' @param alpha Column for alpha aesthetic (variable transparency)
#' @param density_alpha Fixed alpha value for density plots (default 0.8)
#' @param position Position adjustment for certain geoms
#' @param bins Number of bins for histogram
#' @param donut Whether to create donut chart when type is "pie" (default FALSE)
#' @param ... Forwarded to \code{\link[blockr.core]{new_plot_block}}
#'
#' @return A plot block object of class `ggplot_block`.
#'
#' @examples
#' # Create a scatter plot block
#' new_ggplot_block(type = "point", x = "mpg", y = "hp")
#'
#' # Create a bar chart block
#' new_ggplot_block(type = "bar", x = "cyl")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_ggplot_block(), list(data = mtcars))
#' }
#'
#' @export
new_ggplot_block <- function(
  type = "point",
  x = character(),
  y = character(),
  color = character(),
  fill = character(),
  size = character(),
  shape = character(),
  linetype = character(),
  group = character(),
  alpha = character(),
  density_alpha = 0.8,
  position = "stack",
  bins = 30,
  donut = FALSE,
  ...
) {


  # Normalize aesthetic values - empty/NULL/NA becomes "(none)"
  normalize_aes <- function(x) {
    if (!isTruthy(x)) "(none)" else x
  }

  # Define which aesthetics are valid for each chart type.
  # NB: this list is mirrored by GG_TYPE_ROLES in inst/js/gg-blocks.js (the
  # settings-band UI) — keep both in sync.
  chart_aesthetics <- list(
    point = list(
      required = c("x", "y"),
      optional = c("color", "shape", "size", "alpha", "fill"),
      specific = list()
    ),
    bar = list(
      required = c("x"),
      optional = c("y", "fill", "color", "alpha"),
      specific = list(position = c("stack", "dodge", "fill"))
    ),
    line = list(
      required = c("x", "y"),
      optional = c("color", "linetype", "alpha", "group"),
      specific = list()
    ),
    boxplot = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    violin = list(
      required = c("x", "y"),
      optional = c("fill", "color", "alpha"),
      specific = list()
    ),
    density = list(
      required = c("x"),
      optional = c("fill", "group"),
      specific = list(density_alpha = TRUE)
    ),
    area = list(
      required = c("x", "y"),
      optional = c("fill", "alpha"),
      specific = list()
    ),
    histogram = list(
      required = c("x"),
      optional = c("fill", "color", "alpha"),
      specific = list(
        bins = TRUE,
        position = c("stack", "identity", "dodge")
      )
    ),
    pie = list(
      required = c("x"), # x is categories, but rendered as x = ""
      optional = c("y", "fill", "alpha"),
      specific = list() # Could add donut = TRUE/FALSE later
    )
  )

  new_ggplot_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          # Initialize reactive values
          # (normalize aesthetics to handle restoration)
          r_type <- reactiveVal(type)
          r_x <- reactiveVal(x)
          r_y <- reactiveVal(normalize_aes(y))
          r_color <- reactiveVal(normalize_aes(color))
          r_fill <- reactiveVal(normalize_aes(fill))
          r_size <- reactiveVal(normalize_aes(size))
          r_shape <- reactiveVal(normalize_aes(shape))
          r_linetype <- reactiveVal(normalize_aes(linetype))
          r_group <- reactiveVal(normalize_aes(group))
          r_alpha <- reactiveVal(normalize_aes(alpha))
          r_density_alpha <- reactiveVal(density_alpha)
          r_position <- reactiveVal(position)
          r_bins <- reactiveVal(bins)
          r_donut <- reactiveVal(donut)

          # Column metadata for the JS settings band (same
          # name/type/n_unique/label/levels shape as blockr.viz's chart
          # block; factor level order travels as data).
          r_col_meta <- reactive({
            d <- data()
            req(is.data.frame(d))
            lapply(names(d), function(col) {
              vals <- d[[col]]
              lbl <- attr(vals, "label")
              res <- list(
                name = col,
                type = if (is.numeric(vals)) "numeric" else "categorical",
                n_unique = length(unique(vals))
              )
              if (!is.null(lbl) && nzchar(lbl)) res$label <- lbl
              if (is.factor(vals)) res$levels <- as.list(levels(vals))
              res
            })
          })

          # State sentinel "(none)" (and the empty constructor default) maps
          # to "" for the JS side; state itself keeps "(none)" so
          # serialization is unchanged.
          snd <- function(v) {
            if (length(v) != 1 || identical(v, "(none)")) "" else v
          }

          # Push columns + config to JS. A single observe (the shape the
          # blockr.dock lazy-eval gating pairs with — see blockr.viz
          # chart-block.R). No data frame is shipped: the plot renders
          # server-side, JS only needs column metadata for its pickers.
          observe({
            session$sendCustomMessage("gg-block-data", list(
              id = session$ns("gg_block"),
              block = "ggplot",
              columns = r_col_meta(),
              config = list(
                type = r_type(),
                x = snd(r_x()),
                y = snd(r_y()),
                color = snd(r_color()),
                fill = snd(r_fill()),
                size = snd(r_size()),
                shape = snd(r_shape()),
                linetype = snd(r_linetype()),
                group = snd(r_group()),
                alpha = snd(r_alpha()),
                density_alpha = r_density_alpha(),
                position = r_position(),
                bins = r_bins(),
                donut = if (isTRUE(r_donut())) "on" else "off"
              )
            ))
          })

          # JS -> R: the band echoes the FULL config on every change. Only
          # write when a value actually changed — a blind reactiveVal set
          # invalidates the push observe above, which re-sends to JS, which
          # echoes back: an R->JS->R loop. Same identical() guard as
          # blockr.viz's chart block. This also gives external control for
          # free: an externally set state reactiveVal re-runs the push
          # observe and the band re-renders.
          upd <- function(rv, v) {
            if (!identical(isolate(rv()), v)) rv(v)
          }

          observeEvent(input$gg_block_action, {
            msg <- input$gg_block_action
            if (!identical(msg$action, "config")) {
              return()
            }
            if (!is.null(msg$type)) upd(r_type, msg$type)
            if (!is.null(msg$x)) upd(r_x, msg$x)
            if (!is.null(msg$y)) upd(r_y, normalize_aes(msg$y))
            if (!is.null(msg$color)) upd(r_color, normalize_aes(msg$color))
            if (!is.null(msg$fill)) upd(r_fill, normalize_aes(msg$fill))
            if (!is.null(msg$size)) upd(r_size, normalize_aes(msg$size))
            if (!is.null(msg$shape)) upd(r_shape, normalize_aes(msg$shape))
            if (!is.null(msg$linetype)) {
              upd(r_linetype, normalize_aes(msg$linetype))
            }
            if (!is.null(msg$group)) upd(r_group, normalize_aes(msg$group))
            if (!is.null(msg$alpha)) upd(r_alpha, normalize_aes(msg$alpha))
            if (!is.null(msg$density_alpha)) {
              upd(r_density_alpha, as.numeric(msg$density_alpha))
            }
            if (!is.null(msg$position)) upd(r_position, msg$position)
            if (!is.null(msg$bins)) upd(r_bins, as.numeric(msg$bins))
            if (!is.null(msg$donut)) upd(r_donut, identical(msg$donut, "on"))
          })

          list(
            expr = reactive({
              current_type <- r_type()
              chart_config <- chart_aesthetics[[current_type]]

              # Validate required fields
              if (!isTruthy(r_x()) || length(r_x()) == 0) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }

              # Check if y is required and missing
              if (
                "y" %in%
                  chart_config$required &&
                  (r_y() == "(none)" || !isTruthy(r_y()))
              ) {
                return(quote(ggplot2::ggplot() + ggplot2::geom_blank()))
              }

              # Build aesthetics (use backticks for non-syntactic column names)
              aes_parts <- c(glue::glue("x = {backtick_if_needed(r_x())}"))

              # Add y if not "(none)" and valid for this chart
              if (
                r_y() != "(none)" &&
                  "y" %in% c(chart_config$required, chart_config$optional)
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("y = {backtick_if_needed(r_y())}")
                )
              }

              # Add optional aesthetics if valid and not "(none)"
              if (r_color() != "(none)" && "color" %in% chart_config$optional) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("colour = {backtick_if_needed(r_color())}")
                )
              }
              if (r_fill() != "(none)" && "fill" %in% chart_config$optional) {
                # For histograms, bars, pie, etc., convert to factor
                # for discrete colors (stat_count needs grouping)
                stat_types <- c(
                  "histogram", "bar", "boxplot", "violin", "density", "pie"
                )
                if (current_type %in% stat_types) {
                  aes_parts <- c(
                    aes_parts,
                    glue::glue(
                      "fill = as.factor({backtick_if_needed(r_fill())})"
                    )
                  )
                } else {
                  aes_parts <- c(
                    aes_parts,
                    glue::glue("fill = {backtick_if_needed(r_fill())}")
                  )
                }
              }
              if (r_size() != "(none)" && "size" %in% chart_config$optional) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("size = {backtick_if_needed(r_size())}")
                )
              }
              if (r_shape() != "(none)" && "shape" %in% chart_config$optional) {
                # Shape requires discrete/factor variables
                aes_parts <- c(
                  aes_parts,
                  glue::glue(
                    "shape = as.factor({backtick_if_needed(r_shape())})"
                  )
                )
              }
              if (
                r_linetype() != "(none)" &&
                  "linetype" %in% chart_config$optional
              ) {
                # Linetype requires discrete/factor variables
                aes_parts <- c(
                  aes_parts,
                  glue::glue(
                    "linetype = as.factor({backtick_if_needed(r_linetype())})"
                  )
                )
              }
              # For density plots, always set group to match fill
              # This ensures proper grouping for statistical transformation
              if (current_type == "density") {
                if (r_fill() != "(none)") {
                  aes_parts <- c(
                    aes_parts,
                    glue::glue(
                      "group = as.factor({backtick_if_needed(r_fill())})"
                    )
                  )
                }
              } else if (
                r_group() != "(none)" && "group" %in% chart_config$optional
              ) {
                # For non-density plots, use user-specified group if provided
                aes_parts <- c(
                  aes_parts,
                  glue::glue("group = {backtick_if_needed(r_group())}")
                )
              }
              # Alpha: for density plots, use fixed alpha parameter,
              # not aesthetic mapping
              if (
                current_type != "density" &&
                  r_alpha() != "(none)" &&
                  "alpha" %in% chart_config$optional
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("alpha = {backtick_if_needed(r_alpha())}")
                )
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build chart-specific call
              if (current_type == "bar") {
                if (r_y() == "(none)") {
                  geom_call <- glue::glue(
                    "ggplot2::geom_bar(position = '{r_position()}')"
                  )
                } else {
                  geom_call <- glue::glue(
                    "ggplot2::geom_col(position = '{r_position()}')"
                  )
                }
              } else if (current_type == "histogram") {
                geom_call <- glue::glue(
                  "ggplot2::geom_histogram(bins = {r_bins()}, ",
                  "position = '{r_position()}')"
                )
              } else if (current_type == "point") {
                geom_call <- "ggplot2::geom_point()"
              } else if (current_type == "line") {
                geom_call <- "ggplot2::geom_line()"
              } else if (current_type == "boxplot") {
                geom_call <- "ggplot2::geom_boxplot()"
              } else if (current_type == "violin") {
                geom_call <- "ggplot2::geom_violin()"
              } else if (current_type == "density") {
                # Use fixed alpha value for density plots
                geom_call <- glue::glue(
                  "ggplot2::geom_density(alpha = {r_density_alpha()})"
                )
              } else if (current_type == "area") {
                geom_call <- "ggplot2::geom_area()"
              } else if (current_type == "pie") {
                # PIE CHART: Special handling required

                # Override x aesthetic: empty string for pie, numeric for donut
                if (r_donut()) {
                  # Numeric for donut (allows xlim)
                  aes_parts[1] <- "x = 2"
                } else {
                  # Empty string for regular pie
                  aes_parts[1] <- 'x = ""'
                }

                # Ensure fill aesthetic uses the category column
                # (from x or fill)
                fill_added <- FALSE
                for (i in seq_along(aes_parts)) {
                  if (grepl("^fill = ", aes_parts[i])) {
                    fill_added <- TRUE
                    break
                  }
                }
                if (!fill_added) {
                  # Use x column for fill if no fill aesthetic specified
                  # Wrap in as.factor() so stat_count knows it's grouping
                  aes_parts <- c(
                    aes_parts,
                    glue::glue("fill = as.factor({backtick_if_needed(r_x())})")
                  )
                }

                # Rebuild aes_text with modified parts
                aes_text <- paste(aes_parts, collapse = ", ")

                # Choose geom based on y
                if (r_y() != "(none)") {
                  geom_call <- "ggplot2::geom_col(width = 1)"
                } else {
                  geom_call <- "ggplot2::geom_bar(width = 1)"
                }
              } else {
                # Fallback
                geom_call <- "ggplot2::geom_point()"
              }

              text <- glue::glue(
                # `.(data)` (not a bare `data`) so blockr.core's bquoted
                # export substitutes the upstream block's name in place:
                # `ggplot2::ggplot(sub, ...)` rather than wrapping the whole
                # call in `with(list(data = sub), ...)`. See expr_type below.
                "ggplot2::ggplot(.(data), ggplot2::aes({aes_text})) + ",
                "{geom_call}"
              )

              # Add theme_minimal() as default for all charts
              if (current_type == "pie") {
                # Pie charts: add polar coordinates and theme
                text <- glue::glue(
                  "{text} + ggplot2::coord_polar('y', start = 0) + ",
                  "ggplot2::theme_minimal()"
                )

                # Add donut hole if requested
                if (r_donut()) {
                  text <- glue::glue("{text} + ggplot2::xlim(c(0.2, 2.5))")
                }

                # For better pie chart appearance, remove axis elements
                text <- glue::glue(
                  "{text} + ggplot2::theme(",
                  "axis.title = ggplot2::element_blank(), ",
                  "axis.text = ggplot2::element_blank(), ",
                  "axis.ticks = ggplot2::element_blank())"
                )
              } else {
                # Regular charts: apply theme_minimal()
                text <- glue::glue("{text} + ggplot2::theme_minimal()")
              }

              # Board scale map (blockr.theme via Suggests): inject manual
              # scales for bound discrete variables so the same level shows
              # the same color as in every other consumer. Pie maps fill
              # from x when no fill aesthetic is set (mirrors aes assembly
              # above). NULL (no theme / no map / no complete binding) keeps
              # ggplot defaults.
              fill_var <- if (r_fill() != "(none)") {
                r_fill()
              } else if (current_type == "pie") {
                r_x()
              }
              for (sm in c(
                gg_scale_map_text(session, data(), fill_var, "fill"),
                if (r_color() != "(none)") {
                  gg_scale_map_text(session, data(), r_color(), "colour")
                }
              )) {
                text <- glue::glue("{text} + {sm}")
              }

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y,
              color = r_color,
              fill = r_fill,
              size = r_size,
              shape = r_shape,
              linetype = r_linetype,
              group = r_group,
              alpha = r_alpha,
              density_alpha = r_density_alpha,
              position = r_position,
              bins = r_bins,
              donut = r_donut
            )
          )
        }
      )
    },
    function(id) {
      # JS-first UI (settings-band pattern from blockr.viz): the expr slot
      # renders only the html dependencies plus an empty container; the gear
      # header and in-flow settings band are built by inst/js/gg-blocks.js.
      # The plot itself stays block_ui's plotOutput below this container, so
      # opening the band pushes it down and the result remains visible.
      tagList(
        ggplot_block_deps(),
        div(
          id = NS(id, "gg_block"),
          class = "gg-block-container",
          `data-gg-block` = "ggplot"
        )
      )
    },
    class = "ggplot_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = c(
      "y",
      "color",
      "fill",
      "size",
      "shape",
      "linetype",
      "group",
      "alpha"
    ),
    ...
  )
}
