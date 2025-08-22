# blockr.ggplot

<!-- badges: start -->
[![ci](https://github.com/BristolMyersSquibb/blockr.ggplot/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.ggplot/actions/workflows/ci.yaml)
<!-- badges: end -->

**Data visualization blocks for blockr.core**

`blockr.ggplot` extends [blockr.core](https://github.com/BristolMyersSquibb/blockr.core) with comprehensive plotting blocks for data visualization, providing intuitive visual interfaces for creating professional charts and graphs. Build visualization pipelines by connecting data transformation blocks to powerful plotting blocks in an interactive drag-and-drop interface.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("BristolMyersSquibb/blockr.ggplot")
```

## Quick Start

```r
library(blockr.ggplot)

# Create and serve a simple scatter plot
blockr.core::serve(
  new_scatter_plot_block(x = "wt", y = "mpg", color = "cyl"),
  data = list(data = mtcars)
)
```

This launches an interactive web interface where you can:
- Configure plot aesthetics with visual controls
- See real-time preview as you modify settings
- Customize titles, colors, and styling options

## Individual Blocks

### Bar Chart Block - Column/Bar Charts

Create bar charts with support for grouping, stacking, and horizontal orientation.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_bar_chart_block(x = "cyl", fill = "gear", position = "dodge"),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars", package = "datasets"),
    bar_chart = new_bar_chart_block(x = "cyl", fill = "gear", position = "stack")
  ),
  links = c(
    chart_link = new_link("data_block", "bar_chart", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Multiple positions**: Stacked, grouped (side-by-side), or filled (100%)
- **Horizontal bars**: Optional coordinate flipping
- **Smart column detection**: Automatically filters categorical vs numeric columns
- **Count or value**: Use existing numeric column or count occurrences

### Line Chart Block - Time Series & Trends

Perfect for time series data and trend visualization with multiple line support.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_line_chart_block(x = "Time", y = "demand"),
  data = list(data = BOD)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("ChickWeight", package = "datasets"),
    line_chart = new_line_chart_block(
      x = "Time",
      y = "weight",
      color = "Diet",
      title = "Chick Weight Over Time by Diet",
      show_points = TRUE
    )
  ),
  links = c(
    chart_link = new_link("data_block", "line_chart", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Multiple lines**: Color by categorical variables
- **Points overlay**: Optional point markers on lines
- **Line customization**: Size, linetype aesthetics
- **Date/time support**: Optimized for temporal data

### Scatter Plot Block - XY Relationships

Explore relationships between variables with full aesthetic mapping support.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_scatter_plot_block(
    x = "wt",
    y = "mpg",
    color = "cyl",
    size = "hp",
    add_smooth = TRUE
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris", package = "datasets"),
    scatter_plot = new_scatter_plot_block(
      x = "Sepal.Length",
      y = "Petal.Length",
      color = "Species",
      add_smooth = TRUE
    )
  ),
  links = c(
    chart_link = new_link("data_block", "scatter_plot", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Multi-aesthetic**: x, y, color, size, shape mapping
- **Trend lines**: Optional smooth/linear regression
- **Transparency control**: Alpha channel adjustment
- **Smart filtering**: Numeric columns for x/y, categorical for shape

### Pie Chart Block - Categorical Proportions

Visualize categorical data proportions with standard and donut chart styles.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_pie_chart_block(
    x = "Species",
    y = "Sepal.Length",
    show_labels = TRUE
  ),
  data = list(data = iris)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars", package = "datasets"),
    pie_chart = new_pie_chart_block(x = "gear", donut = TRUE)
  ),
  links = c(
    chart_link = new_link("data_block", "pie_chart", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Donut style**: Optional center hole for modern look
- **Percentage labels**: Automatic calculation and display
- **Count or value**: Use existing values or count occurrences
- **Color mapping**: Optional fill aesthetic

### Boxplot Block - Distribution Analysis

Enhanced boxplot visualization for exploring data distributions and outliers.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_boxplot_block(
    x = "cyl",
    y = "mpg",
    fill = "gear",
    show_outliers = TRUE
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris", package = "datasets"),
    boxplot = new_boxplot_block(
      x = "Species",
      y = "Sepal.Length",
      fill = "Species"
    )
  ),
  links = c(
    chart_link = new_link("data_block", "boxplot", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Outlier control**: Toggle outlier display
- **Multi-aesthetic**: Color and fill by different variables
- **Smart filtering**: Categorical for x-axis, numeric for y-axis
- **Single or grouped**: Works with single variable or grouped by categories

### Histogram Block - Frequency Distributions

Visualize data distributions with customizable binning and styling.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_histogram_block(
    x = "mpg",
    bins = 15,
    fill = "cyl",
    alpha = 0.7
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("faithful", package = "datasets"),
    histogram = new_histogram_block(x = "eruptions", bins = 20)
  ),
  links = c(
    chart_link = new_link("data_block", "histogram", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Flexible binning**: Adjustable number of bins (5-100)
- **Transparency**: Alpha channel control
- **Multiple distributions**: Fill by categorical variables
- **Smart filtering**: Only numeric columns for x-axis

### Area Chart Block - Cumulative Visualization

Show cumulative values over time with stacking support.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_area_chart_block(
    x = "Time",
    y = "demand",
    title = "BOD Demand Over Time"
  ),
  data = list(data = BOD)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("BOD", package = "datasets"),
    area_chart = new_area_chart_block(
      x = "Time",
      y = "demand",
      title = "BOD Demand Over Time"
    )
  ),
  links = c(
    chart_link = new_link("data_block", "area_chart", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Stacking options**: Stack or fill (100%) positioning
- **Transparency**: Alpha adjustment for overlapping areas
- **Date/time optimized**: Works well with temporal data
- **Multi-series**: Fill by categorical variables

### Heatmap Block - 2D Data Visualization

Visualize 2D categorical data or correlation matrices with color intensity.

**Simple usage:**
```r
library(blockr.ggplot)

# Use admissions contingency table from datasets and convert to data.frame
ucb <- as.data.frame(UCBAdmissions)

blockr.core::serve(
  new_heatmap_block(
    x = "Dept",
    y = "Gender",
    fill = "Freq",
    color_palette = "viridis",
    show_values = TRUE
  ),
  data = list(data = ucb)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("warpbreaks", package = "datasets"),
    heatmap = new_heatmap_block(
      x = "wool",
      y = "tension",
      fill = "breaks",
      color_palette = "viridis"
    )
  ),
  links = c(
    chart_link = new_link("data_block", "heatmap", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Color palettes**: Viridis, plasma, inferno, magma, blues
- **Value display**: Optional text labels on tiles
- **Flexible axes**: Any variable types for x/y axes
- **Professional styling**: Clean theme with rotated labels

### Density Plot Block - Smooth Distributions

Create smooth density curves for continuous variable distributions.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_density_plot_block(
    x = "mpg",
    fill = "cyl",
    alpha = 0.6,
    adjust = 1.2
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris", package = "datasets"),
    density_plot = new_density_plot_block(
      x = "Sepal.Length",
      fill = "Species",
      alpha = 0.5
    )
  ),
  links = c(
    chart_link = new_link("data_block", "density_plot", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Bandwidth adjustment**: Control smoothness (0.1-3.0)
- **Multiple densities**: Fill/color by categorical variables
- **Transparency**: Alpha channel for overlapping curves
- **Smart filtering**: Only numeric columns for density calculation

### Violin Plot Block - Distribution Shapes

Show detailed distribution shapes combining density and boxplot concepts.

**Simple usage:**
```r
library(blockr.ggplot)
blockr.core::serve(
  new_violin_plot_block(
    x = "cyl",
    y = "mpg",
    fill = "cyl",
    scale = "area"
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris", package = "datasets"),
    violin_plot = new_violin_plot_block(
      x = "Species",
      y = "Petal.Length",
      fill = "Species",
      trim = TRUE
    )
  ),
  links = c(
    chart_link = new_link("data_block", "violin_plot", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Scaling methods**: Equal area, count-based, or equal width
- **Tail trimming**: Optional trimming of distribution tails
- **Multi-aesthetic**: Color and fill mapping
- **Smart filtering**: Categorical for x-axis, numeric for y-axis

## Comprehensive Example

Here's a realistic data visualization pipeline demonstrating multiple blocks working together:

```r
library(blockr.core)
library(blockr.ggplot)
library(blockr.dplyr)
library(blockr.ui)

# Comprehensive visualization dashboard
viz_board <- blockr.ui::new_dag_board(
  blocks = c(
    # Data source
    cars_data = new_dataset_block("mtcars", package = "datasets"),

    # Data preparation
    cars_enhanced = new_mutate_block(list(
      performance = "hp / wt",
      efficiency = "mpg / cyl",
      car_type = "dplyr::case_when(cyl <= 4 ~ 'Economy', cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
    )),

    # Filter for analysis
    performance_cars = new_filter_block("hp > 100"),

    # Multiple visualizations of the same data
    # 1. Scatter plot - relationship between weight and mpg
    scatter_viz = new_scatter_plot_block(
      x = "wt",
      y = "mpg",
      color = "car_type",
      size = "hp",
      title = "Weight vs MPG by Car Type",
      add_smooth = TRUE
    ),

    # 2. Box plot - mpg distribution by car type
    mpg_distribution = new_boxplot_block(
      x = "car_type",
      y = "mpg",
      fill = "car_type",
      title = "MPG Distribution by Car Type"
    ),

    # 3. Bar chart - count by car type and cylinders
    type_counts = new_bar_chart_block(
      x = "car_type",
      fill = "cyl",
      position = "dodge",
      title = "Car Count by Type and Cylinders"
    ),

    # 4. Histogram - performance distribution
    performance_dist = new_histogram_block(
      x = "performance",
      fill = "car_type",
      bins = 15,
      title = "Performance Distribution (HP/Weight)",
      alpha = 0.7
    ),

    # 5. Line chart - trend analysis (using row numbers as time proxy)
    trend_data = new_mutate_block(list(
      row_id = "dplyr::row_number()"
    )),

    mpg_trend = new_line_chart_block(
      x = "row_id",
      y = "mpg",
      color = "car_type",
      title = "MPG Trend by Car Index",
      show_points = TRUE
    ),

    # 6. Density plot - efficiency distribution
    efficiency_density = new_density_plot_block(
      x = "efficiency",
      fill = "car_type",
      title = "Efficiency Distribution by Car Type",
      alpha = 0.6
    ),

    # 7. Violin plot - detailed mpg distribution
    mpg_violin = new_violin_plot_block(
      x = "car_type",
      y = "mpg",
      fill = "car_type",
      title = "MPG Distribution Shapes",
      scale = "area"
    ),

    # Create correlation data for heatmap
    iris_data = new_dataset_block("iris", package = "datasets"),
    iris_numeric = new_select_block(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),

    # Note: This is a simplified example - in reality you'd need correlation calculation
    correlation_heatmap = new_heatmap_block(
      x = "Sepal.Length",
      y = "Sepal.Width",
      fill = "Petal.Length",
      title = "Iris Feature Relationships",
      color_palette = "viridis"
    ),

    # 9. Area chart - cumulative performance
    area_viz = new_area_chart_block(
      x = "row_id",
      y = "performance",
      fill = "car_type",
      title = "Cumulative Performance by Type",
      position = "stack"
    ),

    # 10. Pie chart - car type distribution
    type_pie = new_pie_chart_block(
      x = "car_type",
      title = "Car Type Distribution",
      show_labels = TRUE,
      donut = TRUE
    )
  ),
  links = c(
    # Data preparation pipeline
    enhance_link = new_link("cars_data", "cars_enhanced", "data"),
    filter_link = new_link("cars_enhanced", "performance_cars", "data"),

    # Connect filtered data to visualizations
    scatter_link = new_link("performance_cars", "scatter_viz", "data"),
    box_link = new_link("performance_cars", "mpg_distribution", "data"),
    bar_link = new_link("performance_cars", "type_counts", "data"),
    hist_link = new_link("performance_cars", "performance_dist", "data"),
    density_link = new_link("performance_cars", "efficiency_density", "data"),
    violin_link = new_link("performance_cars", "mpg_violin", "data"),
    pie_link = new_link("performance_cars", "type_pie", "data"),

    # Trend analysis pipeline
    trend_enhance = new_link("performance_cars", "trend_data", "data"),
    trend_link = new_link("trend_data", "mpg_trend", "data"),
    area_link = new_link("trend_data", "area_viz", "data"),

    # Iris correlation pipeline
    iris_select_link = new_link("iris_data", "iris_numeric", "data"),
    heatmap_link = new_link("iris_numeric", "correlation_heatmap", "data")
  )
)

# Launch the comprehensive visualization dashboard
blockr.core::serve(viz_board)
```

This example demonstrates:
- **10 different chart types** working with the same dataset
- **Data preprocessing** using blockr.dplyr blocks
- **Multiple perspectives** on the same data
- **Professional styling** with titles and customization
- **Complex workflows** with data transformation and visualization

## Learn More

- [blockr.core documentation](https://github.com/BristolMyersSquibb/blockr.core)
- [blockr.ui documentation](https://github.com/BristolMyersSquibb/blockr.ui)
- [blockr.dplyr documentation](https://github.com/BristolMyersSquibb/blockr.dplyr)
- [ggplot2 documentation](https://ggplot2.tidyverse.org/)