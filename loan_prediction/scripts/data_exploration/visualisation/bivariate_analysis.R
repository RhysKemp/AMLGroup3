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
pie_palette_one <- "Dark2"
box_palette_one <- "Dark2"
histogram_palette_one <- "Dark2"

# =====================================
# Chart Helper Functions
# =====================================

# Function to plot a bar chart with set aesthetics and params
create_bar_plot <- function(data, x_column, fill_column = NULL,
                            x_labels = NULL, plot_title = "Bar Plot",
                            x_axis_title = "X-Axis", y_axis_title = "Frequency",
                            fill_palette = bar_palette_one ) {
  
  # Ensure consistent factor levels for the column
  if (!is.factor(data[[x_column]])) {
    data[[x_column]] <- factor(data[[x_column]], levels = unique(data[[x_column]]))
  }
  
  # Ensure fill_column is a factor
  if (!is.null(fill_column) && !is.factor(data[[fill_column]])) {
    data[[fill_column]] <- factor(data[[fill_column]])
  }
  
  # fill defaults to x_column
  if (is.null(fill_column)) {
    fill_column <- x_column
  }
  
  # if no labels use unique values in the column
  if (is.null(x_labels)) {
    x_labels <- unique(data[[x_column]])
  }
  
  # Create bar plot
  p <- ggplot(data, aes_string(x = x_column, fill = fill_column)) +
    geom_bar(position = "dodge", color = "black", alpha = 1, stat = "count") +
    geom_text(
      aes(label = after_stat(count)), 
      stat = "count", 
      position = position_dodge(width = 0.8),
      vjust = -0.5, 
      color = "black", 
      size = 5
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    scale_fill_brewer(
      palette = fill_palette,
      labels = x_labels
    ) +
    labs(
      title = plot_title,
      x = x_axis_title,
      y = y_axis_title
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      plot.title = element_text(color = "black", size = 16, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1),
      legend.title = element_text(size = 12, face = "bold", color = "black")
    ) +
    guides(fill = guide_legend(title = fill_column))
  
  return(p)
}

# Helper function to plot a density graph with set aesthetics and params
create_density_plot <- function(data, x_column, fill_column = NULL,
                                plot_title = "Density Plot", x_axis_title = "X-Axis",
                                y_axis_title = "Density", fill_palette = density_palette_one) {
  
  # Ensure the column is numeric
  if (!is.numeric(data[[x_column]])) {
    stop(paste(x_column, "must be numeric for density plot"))
  }
  
  # Ensure fill_column is a factor
  if (!is.null(fill_column) && !is.factor(data[[fill_column]])) {
    data[[fill_column]] <- factor(data[[fill_column]])
  }
  
  # fill defaults to x_column
  if (is.null(fill_column)) {
    fill_column <- x_column
  }
  
  p <- ggplot(data, aes_string(x = x_column, fill = fill_column)) +
    geom_density(alpha = 0.5, linewidth = 1, adjust = 1) +
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_axis_title,
      y = y_axis_title
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
    guides(fill = guide_legend(title = fill_column))
  
  return(p)
}

# Helper function to plot a boxplot graph with set aesthetics and params
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
  
  # Ensure fill_column is a factor
  if (!is.null(fill) && !is.factor(data[[fill]])) {
    data[[fill]] <- factor(data[[fill]])
  }
  
  ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = if (!is.null(fill)) .data[[fill]] else NULL)) +
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
}

# =====================================
# Plot loan status by Categorical features
# =====================================

# Create list of categorical columns
categorical_cols <- names(data)[!sapply(data, is.numeric)]

# Function to plot column by loan status distribution 
plot_x_by_category_distribution <- function(data, x_column, fill_column) {
  plot_title = paste(gsub("_", " ", fill_column), "by", gsub("_", " ", x_column))
  
  # Create bar plot
  bar_plot <- create_bar_plot(
    data = data,
    x_column = x_column,
    fill_column = "loan_status",
    plot_title = plot_title,
    x_axis_title = gsub("_", " ", x_column),
    y_axis_title = "Frequency"
  )
  return(bar_plot)
}

plot_x_by_category_distribution(data, x_column = "person_home_ownership", fill_column = "loan_status")

for (column in categorical_cols) { # Grid view clips data
  print(plot_x_by_category_distribution(data, x_column = column, fill_column = "loan_status"))
}


# =====================================
# Plot loan status by numerical features
# =====================================

# Create list of numerical columns
numerical_cols <- names(data)[sapply(data, is.numeric)]

# Function to plot all columns against fill_column in a grid
plot_density <- function(data, columns, fill_column) {
  plot_list <- list()
  
  for(col in columns) {
    if(col == fill_column) {
      next # Skip if same column
    }
    # Create plot
    p <- create_density_plot(
      data, 
      x_column = col,
      fill_column = fill_column,
      plot_title = paste(gsub("_", " ", col), "Density by", gsub("_", " ", fill_column)),
      x_axis_title = col
      )
    # append plot to list
    plot_list[[col]] <- p
  }
  grid.arrange(grobs = plot_list, ncol = 2)
}

plot_density(data, numerical_cols, "loan_status") # Plot all numerical columns against loan status
 
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
