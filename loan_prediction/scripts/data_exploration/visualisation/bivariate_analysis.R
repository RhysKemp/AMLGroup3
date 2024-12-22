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
density_palette_one <- "Dark2"
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
      aes(label = ..count..), 
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
                                x_labels = NULL, plot_title = "Density Plot",
                                x_axis_title = "X-Axis", y_axis_title = "Density",
                                fill_palette = density_palette_one) {
  
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
  
  # if no labels use unique values in the column
  if (is.null(x_labels)) {
    x_labels <- unique(data[[x_column]])
  }
  
  p <- ggplot(data, aes_string(x = x_column, fill = fill_column)) +
    geom_density(alpha = 0.5, size = 1.5, adjust = 0.5) +
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      x = x_axis_title,
      y = y_axis_title
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = x_labels) +
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

# TODO: append to list and add to grid for easier viewing
for (column in categorical_cols) {
  print(plot_x_by_category_distribution(data, x_column = column, fill_column = "loan_status"))
}


# =====================================
# Plot loan status by numerical features
# =====================================

# Create list of numerical columns
numerical_cols <- names(data)[sapply(data, is.numeric)]

plot_x_by_category_density <- function(data, x_column, fill_column) {
  next
}

col <- "loan_amnt"

print(create_density_plot(data, x_column = col, fill_column = "loan_status", plot_title = paste(col, "Density by Loan Status")))

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
