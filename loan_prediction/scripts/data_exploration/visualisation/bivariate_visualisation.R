# =====================================
# File: bivariate_analysis.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
#       -
#       -
# =====================================

# =====================================
# Load Libraries
# =====================================

library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(grid)

# =====================================
# Define File Paths
# =====================================
# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "raw", "loan_data.csv")

# =====================================
# Load Dataset
# =====================================

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

# =====================================
# Variables for graph palettes
# =====================================

bar_palette_one <- "Dark2"
density_palette_one <- "Set1"
box_palette_one <- "Dark2"
violin_palette_one <- "Dark2"

# =====================================
# Chart Helper Functions
# =====================================

# Function to plot a bar chart with set aesthetics and params
create_bar_plot <- function(data, x, fill = NULL,
                            plot_title = "Bar Plot",
                            x_title = "X-Axis", y_title = "Frequency",
                            fill_palette = bar_palette_one ) {
  
  # Ensure consistent factor levels for the column
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]], levels = unique(data[[x]]))
  }
  
  # Ensure fill is a factor
  if (!is.null(fill) && !is.factor(data[[fill]])) {
    data[[fill]] <- factor(data[[fill]])
  }
  
  # fill defaults to x
  if (is.null(fill)) {
    fill <- x
  }
  
  # Create bar plot
  p <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill]])) +
    geom_bar(position = "dodge", color = "black", alpha = 1, stat = "count") +
    geom_text(
      aes(label = after_stat(count)), 
      stat = "count", 
      position = position_dodge(width = 0.8),
      vjust = -0.5, 
      color = "black", 
      size = 5
    ) +
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_title,
      y = y_title
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      plot.title = element_text(color = "black", size = 16, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1),
      legend.title = element_text(size = 12, face = "bold", color = "black")
    ) +
    guides(fill = guide_legend(title = fill))
  
  return(p)
}

# Helper function to plot a density graph with set aesthetics and params
create_density_plot <- function(data, x, fill = NULL,
                                plot_title = "Density Plot", x_title = "X-Axis",
                                y_title = "Density", fill_palette = density_palette_one) {
  
  # Ensure the column is numeric
  if (!is.numeric(data[[x]])) {
    stop(paste(x, "must be numeric for density plot"))
  }
  
  # Ensure fill is a factor
  if (!is.null(fill) && !is.factor(data[[fill]])) {
    data[[fill]] <- factor(data[[fill]])
  }
  
  # fill defaults to x_column
  if (is.null(fill)) {
    fill <- x
  }
  
  p <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill]])) +
    geom_density(alpha = 0.5, linewidth = 1, adjust = 1) +
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_title,
      y = y_title
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      plot.title = element_text(color = "black", size = 16, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1),
      legend.title = element_text(size = 12, face = "bold", color = "black")
    ) +
    guides(fill = guide_legend(title = fill))
  
  return(p)
}

# Helper function to box plot with set aesthetics and params
create_box_plot <- function(data, x, y, fill = NULL,
                            x_labels = NULL, plot_title = "Box Plot", 
                            x_title = "X-Axis", y_title = "Y-Axis",
                            fill_palette = box_palette_one) {
  
  # Ensure consistent factor levels for the x column
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]], levels = unique(data[[x]]))
  }
  
  # if no labels use unique values in the column
  if (is.null(x_labels)) {
    x_labels <- unique(data[[x]])
  }
  
  # Default fill to categorical if no grouping
  if (is.null(fill)) {
    fill <- x
  }
  
  # Ensure fill is a factor
  if (!is.null(fill) && !is.factor(data[[fill]])) {
    data[[fill]] <- factor(data[[fill]])
  }
  
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = if (!is.null(fill)) .data[[fill]] else NULL)) +
    geom_boxplot(
      outlier.color = "black",
      outlier.shape = 16,
      outlier.size = 2,
    ) +
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_title,
      y = y_title
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1)
    ) +
    guides(fill = guide_legend(title = fill))
  
  return(p)
}

# =====================================
# Plot loan status by Categorical features
# =====================================

# Create list of categorical columns
categorical_cols <- names(data)[!sapply(data, is.numeric)]

# Function to plot column by loan status distribution 
plot_x_by_category_distribution <- function(data, x, fill) {
  plot_title = paste(gsub("_", " ", fill), "by", gsub("_", " ", x))
  
  # Create bar plot
  bar_plot <- create_bar_plot(
    data = data,
    x = x,
    fill = "loan_status",
    plot_title = plot_title,
    x_title = gsub("_", " ", x),
    y_title = "Frequency"
  )
  return(bar_plot)
}

plot_x_by_category_distribution(data, x = "person_home_ownership", fill = "loan_status")

for (column in categorical_cols) { # Grid view clips data
  print(plot_x_by_category_distribution(data, x = column, fill = "loan_status"))
}


# =====================================
# Plot loan status by numerical features - Density
# =====================================

# Create list of numerical columns
numerical_cols <- names(data)[sapply(data, is.numeric)]

# Function to plot all columns against fill in a grid
plot_density <- function(data, columns, fill) {
  plot_list <- list()
  
  for(col in columns) {
    if(col == fill) {
      next # Skip if same column
    }
    # Create plot
    p <- create_density_plot(
      data = data, 
      x = col,
      fill = fill,
      plot_title = paste(gsub("_", " ", col), "Density by", gsub("_", " ", fill)),
      x_title = col
      )
    # append plot to list
    plot_list[[col]] <- p
  }
  grid.arrange(grobs = plot_list, ncol = 2)
}

plot_density(data, numerical_cols, "loan_status") # Plot all numerical columns against loan status
 
# =====================================
# Plot loan status by numerical features - boxplot
# =====================================

box_plot_loan_status <- function(data, x, columns, fill = NULL) {
  plot_list <- list()
  
  for(y in columns) {
    if(y == x) {
      next # Skip if same column
    }
    # Create plot
    p <- create_box_plot(
      data, 
      x = x,
      y = y,
      fill = fill,
      plot_title = paste(gsub("_", " ", y), "by", gsub("_", " ", x)),
      x_title = gsub("_", " ", x),
      y_title = gsub("_", " ", y)
    )
    # append plot to list
    plot_list[[y]] <- p
  }
  grid.arrange(grobs = plot_list, ncol = 2)
}

box_plot_loan_status(
  data = data,
  x = "loan_status",
  columns = numerical_cols
  )

# =====================================
# Plot categories vs loan status by person gender
# =====================================

# Helper function for dupliction of bar graph
plot_bar_plot_for_category <- function(data, x, fill = NULL, category, max_cols) {
  plot_list <- list()
  unique_categories <- unique(data[[category]])
  all_x_values <- unique(data[[x]])
  
  for (cat in unique_categories) { 
    category_data <- data[data[[category]] == cat, ] # Filter data for current category
    category_data[[x]] <- factor(category_data[[x]], levels = all_x_values) # Ensure consistent factor
    
    p <- create_bar_plot( # Create plot
      data = category_data,
      x = x,
      fill = fill,
      plot_title = paste(gsub("_", " ", category), ":", cat),
      x_title = gsub("_", " ", x),
      y_title = "Count"
    )
    plot_list[[cat]] <- p # append plot to list
  }
  
  # Dynamic column scaling
  n_plots <- length(plot_list)
  n_cols <- min(max_cols, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  grid.arrange(grobs = plot_list, ncol = n_cols, nrow = n_rows)
}

plot_bar_plot_for_category(
  data = data,
  x = "person_education",
  fill = "loan_status",
  category = "person_gender",
  max_cols = 3
)

for (col in categorical_cols) {
  if (col == "person_gender" || col == "loan_status"){
    next
  }
  plot_bar_plot_for_category(
    data = data,
    x = col,
    fill = "loan_status",
    category = "person_gender",
    max_cols = 3
  )
}

# =====================================
# Violin plot for loan_amnt and
# loan_int_rate by loan_status
# =====================================

create_violin_plot <- function(data, x, y, fill = NULL,
                               plot_title = "Violin Plot", 
                               x_title = "X-Axis", y_title = "Y-Axis", 
                               fill_palette = violin_palette_one) {
  
  # Ensure consistent factor levels for the x column
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]], levels = unique(data[[x]]))
  }
  
  # Ensure fill is a factor
  if (!is.null(fill) && !is.factor(data[[fill]])) {
    data[[fill]] <- factor(data[[fill]])
  }
  
  # fill defaults to x
  if (is.null(fill)) {
    fill <- x
  }
  
  # Create plot
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = if (!is.null(fill)) .data[[fill]] else NULL)) +
    geom_violin(trim = FALSE, alpha = 0.7, color = "black") + # violin
    geom_boxplot(width = 0.1, color = "black", alpha = 0.7, outlier.shape = NA) + # boxplot for summary
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_title,
      y = y_title
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1)
    ) +
    guides(fill = guide_legend(title = fill))
  
  return(p)
}

# Create violin plot for loan amount by loan status
violin_plot_loan_amnt <- create_violin_plot(
  data = data,
  x = "loan_status",
  y = "loan_amnt",
  plot_title = "Loan Amount by Loan Status",
  x_title = "Loan Status",
  y_title = "Loan Amount"
)

# Create violin plot for loan interest rate by loan status
violin_plot_loan_int_rate <- create_violin_plot(
  data = data,
  x = "loan_status",
  y = "loan_int_rate",
  plot_title = "Loan Interest Rate by Loan Status",
  x_title = "Loan Status",
  y_title = "Loan Interest Rate"
)

# Show plots in grid
grid.arrange(violin_plot_loan_amnt, violin_plot_loan_int_rate, ncol = 2)

# =====================================
# 
# =====================================

# =====================================
# 
# =====================================

# =====================================
# 
# =====================================


# =====================================
# Save Results (if applicable)
# =====================================
# Save outputs or results to the specified directory.
# Provide console feedback that the operation completed successfully with the appropriate path displayed.
# Example:
# write.csv(output_data, file = output_path, row.names = FALSE)
# cat("Output saved to:", output_path, "\n")

# =====================================
# Notes for Next Steps (Optional)
# =====================================
# Add comments or guidance on what this script enables next.
# Example:
# - Ensure outputs are reviewed before feeding them into the next phase.
# - Check for anomalies in the processed data or results.
# =====================================
