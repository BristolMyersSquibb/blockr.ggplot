#' Scatter plot block constructor
#'
#' This block creates scatter plots using [ggplot2::geom_point()]. Supports multiple
#' aesthetic mappings including size, color, shape, and optional trend lines.
#'
#' @param x Column for x-axis (numeric variable)
#' @param y Column for y-axis (numeric variable) 
#' @param color Column for color aesthetic (optional)
#' @param size Column for size aesthetic (optional, can also be fixed value)
#' @param shape Column for shape aesthetic (optional)
#' @param alpha Transparency level (default 0.8)
#' @param add_smooth Whether to add smooth trend line (default FALSE)
#' @param title Plot title (optional)
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_scatter_plot_block <- function(x = character(), y = character(),
                                  color = character(), size = character(),
                                  shape = character(), alpha = 0.8,
                                  add_smooth = FALSE, title = character(), ...) {
  new_ggplot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          cols <- reactive(colnames(data()))

          x_col <- reactiveVal(x)
          y_col <- reactiveVal(y)
          color_col <- reactiveVal(color)
          size_col <- reactiveVal(size)
          shape_col <- reactiveVal(shape)
          alpha_val <- reactiveVal(alpha)
          add_smooth_val <- reactiveVal(add_smooth)
          plot_title <- reactiveVal(title)

          observeEvent(input$xcol, x_col(input$xcol))
          observeEvent(input$ycol, y_col(input$ycol))
          observeEvent(input$colorcol, color_col(input$colorcol))
          observeEvent(input$sizecol, size_col(input$sizecol))
          observeEvent(input$shapecol, shape_col(input$shapecol))
          observeEvent(input$alpha, alpha_val(input$alpha))
          observeEvent(input$add_smooth, add_smooth_val(input$add_smooth))
          observeEvent(input$title, plot_title(input$title))

          observeEvent(
            cols(),
            {
              numeric_cols <- cols()[sapply(data(), is.numeric)]
              factor_cols <- cols()[sapply(data(), function(x) is.factor(x) || is.character(x))]
              
              updateSelectInput(
                session,
                inputId = "xcol",
                choices = c("", numeric_cols),
                selected = x_col()
              )
              updateSelectInput(
                session,
                inputId = "ycol",
                choices = c("", numeric_cols),
                selected = y_col()
              )
              updateSelectInput(
                session,
                inputId = "colorcol",
                choices = c("", cols()),
                selected = color_col()
              )
              updateSelectInput(
                session,
                inputId = "sizecol",
                choices = c("", numeric_cols),
                selected = size_col()
              )
              updateSelectInput(
                session,
                inputId = "shapecol",
                choices = c("", factor_cols),
                selected = shape_col()
              )
            }
          )

          list(
            expr = reactive({
              # Build aesthetics
              aes_list <- list()
              if (isTruthy(x_col())) aes_list$x <- x_col()
              if (isTruthy(y_col())) aes_list$y <- y_col()
              if (isTruthy(color_col())) aes_list$colour <- color_col()
              if (isTruthy(size_col())) aes_list$size <- size_col()
              if (isTruthy(shape_col())) aes_list$shape <- shape_col()
              
              # Build geom arguments
              point_args <- list()
              point_args$alpha <- alpha_val()
              
              # Start with base plot + points
              plot_expr <- bquote(
                ggplot2::ggplot(data, ggplot2::aes(..(aes_mapping))) +
                  ggplot2::geom_point(..(point_arguments)),
                list(
                  aes_mapping = lapply(aes_list, as.name),
                  point_arguments = point_args
                ),
                splice = TRUE
              )
              
              # Add smooth line if requested
              if (add_smooth_val()) {
                plot_expr <- bquote(
                  ..(plot_base) + ggplot2::geom_smooth(method = "lm", se = TRUE),
                  list(plot_base = plot_expr),
                  splice = TRUE
                )
              }
              
              # Add title if specified
              if (isTruthy(plot_title())) {
                plot_expr <- bquote(
                  ..(plot_base) + ggplot2::labs(title = .(title_text)),
                  list(plot_base = plot_expr, title_text = plot_title()),
                  splice = TRUE
                )
              }
              
              plot_expr
            }),
            state = list(
              x = x_col,
              y = y_col,
              color = color_col,
              size = size_col,
              shape = shape_col,
              alpha = alpha_val,
              add_smooth = add_smooth_val,
              title = plot_title
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        h4("Scatter Plot Configuration"),
        div(
          class = "row",
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "xcol"),
              label = "X-axis (Numeric)",
              choices = x,
              selected = x
            ),
            selectInput(
              inputId = NS(id, "ycol"),
              label = "Y-axis (Numeric)",
              choices = y,
              selected = y
            ),
            selectInput(
              inputId = NS(id, "colorcol"),
              label = "Color By",
              choices = color,
              selected = color
            )
          ),
          div(
            class = "col-md-6",
            selectInput(
              inputId = NS(id, "sizecol"),
              label = "Size By (Numeric)",
              choices = size,
              selected = size
            ),
            selectInput(
              inputId = NS(id, "shapecol"),
              label = "Shape By (Categorical)",
              choices = shape,
              selected = shape
            ),
            sliderInput(
              inputId = NS(id, "alpha"),
              label = "Transparency",
              value = alpha,
              min = 0.1,
              max = 1.0,
              step = 0.1
            )
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-8",
            textInput(
              inputId = NS(id, "title"),
              label = "Plot Title",
              value = title,
              placeholder = "Enter plot title..."
            )
          ),
          div(
            class = "col-md-4",
            checkboxInput(
              inputId = NS(id, "add_smooth"),
              label = "Add Trend Line",
              value = add_smooth
            )
          )
        )
      )
    },
    class = "scatter_plot_block",
    ...
  )
}