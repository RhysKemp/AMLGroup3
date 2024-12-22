# =====================================
# File: univariate_visualisation.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose:
# - Conduct univariate analysis
# - Create visualisations (bar plots, pie charts, density plots, box plots)
# - Explore distributions of categorical and numerical variables
# =====================================

# =====================================
# Load Libraries
# =====================================

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
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
pie_palette_one <- "Dark2"
box_palette_one <- "Dark2"
histogram_palette_one <- "Dark2"

# =====================================
#  Graph Helper Functions
# =====================================

# Function to plot a bar chart with set aesthetics and params
create_bar_plot <- function(data, x_column, fill_column = NULL,
                            x_labels = NULL, plot_title = "Bar Plot",
                            x_axis_title = "X-Axis", y_axis_title = "Frequency",
                            fill_pallette = bar_palette_one ) {
  
  # Ensure consistent factor levels for the column
  data[[x_column]] <- factor(data[[x_column]], levels = unique(data[[x_column]]))
  
  # fill defaults to x_column
  if (is.null(fill_column)) {
    fill_column <- x_column
  }
  
  # if no labels use unique values in the column
  if (is.null(x_labels)) {
    x_labels <- unique(data[[x_column]])
  }
  
  # Create bar plot
  bar_plot <- ggplot(data, aes_string(x = x_column, fill = fill_column)) +
    geom_bar(color = "black", alpha = 1, stat = "count") +
    geom_text(
      aes(label = ..count..), 
      stat = "count", 
      vjust = -0.5, 
      color = "black", 
      size = 5
    ) +
    scale_x_discrete(labels = x_labels) +
    scale_y_continuous(breaks = seq(0, max(table(data[[x_column]])), by = 5000)) +
    scale_fill_brewer(
      palette = fill_pallette,
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
  
  return(bar_plot)
}

# Function to plot a pie chart with set aesthetics and params, has option to show additional information as count, percent or both
create_pie_plot <- function(data, column, fill_palette = pie_palette_one, 
                            plot_title = "Pie Chart", label_format = "percent") {
  
  # Aggregate data for column
  column_counts <- as.data.frame(table(data[[column]]))
  colnames(column_counts) <- c("Category", "Count")
  
  # Ensure consistent factor levels for the column
  column_counts$Category <- factor(column_counts$Category, levels = unique(data[[column]]))
  
  # Calculate percentage
  column_counts$Percentage <- round(100 * column_counts$Count / sum(column_counts$Count), 1)
  
  
  
  # Generate labels dependent on input; count, percent or both
  if (label_format == "count") {
    column_counts$Label <- paste0(column_counts$Category, "\n(", column_counts$Count, ")")
  } else if (label_format == "percent") {
    column_counts$Label <- paste0(column_counts$Category, "\n(", column_counts$Percentage, "%)")
  } else if (label_format == "both") {
    column_counts$Label <- paste0(column_counts$Category, "\n(", column_counts$Count, ", ", column_counts$Percentage, "%)")
  } else {
    stop("Invalid label_format. Use 'count', 'percent', or 'both'.")
  }
  
  pie_plot <- ggplot(column_counts, aes(x = "", y = Count, fill = Category)) +
    geom_bar(width = 1, stat = "identity", color = "black") +
    coord_polar(theta = "y") + # Turn bar chart into pie chart
    scale_fill_brewer(palette = fill_palette) +
    labs(
      title = plot_title,
      fill = column
    ) +
    geom_text(
      aes(label = Label), 
      position = position_stack(vjust = 0.5), 
      color = "white", 
      size = 5
    ) +
    theme_void() +
    theme(
      plot.title = element_text(color = "black", size = 16, face = "bold"),
      legend.background = element_rect(color = "black", linewidth = 1),
      legend.title = element_text(size = 12, face = "bold", color = "black"),
      legend.text = element_text(size = 10)
    )
  
  return(pie_plot)
}

# =====================================
# Visualise Loan Status
# =====================================

# Function to plot a frequency bar chart and a percentage pie char for categorical columns
plot_categorical_distribution <- function(data, column) {
  
  # Column handling
  column_title <- gsub("_", " ", column)
  
  # Create bar plot
  bar_title <- paste(column_title, "Distribution")
  bar_plot <- create_bar_plot(
    data = data,
    x_column = column,
    x_axis_title = column_title,
    plot_title = bar_title,
  )
  bar_plot <- bar_plot +
    guides(
      fill = guide_legend(
        title = "Loan Status", 
        title.position = "top", 
        title.theme = element_text(size = 12, face = "bold", color = "black"),
        label.theme = element_text(size = 10, face = "bold", color = "black")
      )
    )
  
  # Plot pie chart
  pie_title = paste(column_title, "Percentage Distribution")
  pie_plot <- create_pie_plot(
    data = data,
    column = column,
    plot_title = pie_title
  )
  pie_plot <- pie_plot +
    theme(
      legend.position = "none"
    )
  return(grid.arrange(bar_plot, pie_plot, ncol = 2))
}

plot_categorical_distribution(data, "loan_status")

# =====================================
# Univariate Analysis for Numeric Columns
# Distribution with KDE
# =====================================

# List of numerical columns for analysis
numerical_columns_to_analyse <- c('person_age', 'person_income', 'person_emp_exp', 'loan_amnt', 
                        'loan_int_rate', 'loan_percent_income', 'cb_person_cred_hist_length', 
                        'credit_score')

# Function for univariate analysis on numeric columns
univariate_analysis_dist_kde <- function(data, columns) {
  plot_list <- list()
  
  # Loop through each column specified in provided df
  for (i in seq_along(columns)) {
    column <- columns[i]
    
    # Create histogram with density probability curve 
    p <- ggplot(data, aes_string(x = column)) + 
      geom_histogram(
        aes(y = ..density..),
        bins = 30,
        fill = brewer.pal(3, histogram_palette_one)[1],
        color = "black",
        ) + 
      geom_density( # KDE
        color = brewer.pal(3, histogram_palette_one)[2],
        size = 1
      ) + 
      labs(
        title = paste(gsub("_", " ", column), "Distribution with KDE"),
        x = gsub("_", " ", column),
        y = "Density"
        ) +
      # Aesthetics and theme
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma, breaks = scales::pretty_breaks(n = 10)) +
      theme_classic() +
      theme(
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.2)
      )
    
    # Add plot to list
    plot_list[[i]] <- p
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}


univariate_analysis_dist_kde(data, numerical_columns_to_analyse)

# =====================================
# Univariate Analysis for Numeric Columns
# Boxplot
# =====================================

# Function for univariate analysis on numeric columns
univariate_analysis_boxplot <- function(data, column) {
  # boxplot
  p <- ggplot(data, aes_string(x = column)) +
    stat_boxplot(geom ="errorbar", width = 0.5) + # whisker endcaps
    geom_boxplot(
      fill = brewer.pal(3, "Set3")[2],
      color = "black",
      outlier.color = "black",
      outlier.shape = 16,
      outlier.size = 2,
    ) +
    labs(
      title = paste(gsub("_", " ", column), "Boxplot"),
      x = gsub("_", " ", column)
    ) +
    # Aesthetics and theme
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.title.x = element_text(size = 10),
      axis.text.y = element_blank(), # remove y values
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5), # gridlines only on x axis
      panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.2),
    )
  print(p)
  
  # Summary to console
  cat("\nSummary Statistics for ", column, ":\n")
  print(summary(data[[column]]))
}

for (column in numerical_columns_to_analyse) {
  univariate_analysis_boxplot(data, column)
}

# =====================================
# Univariate Analysis for categorical Columns
# Distributions
# =====================================

# List of categorical columns for analysis
categorical_columns_to_analyse <- c("person_gender", "person_education", "person_home_ownership",
                                   "loan_intent", "previous_loan_defaults_on_file")

for (column in categorical_columns_to_analyse) {
  plot_categorical_distribution(data, column)
  }


# =====================================
# Save Results (if applicable)
# =====================================
# Save outputs or results to the specified directory.
# Provide console feedback that the operation completed successfully with the appropriate path displayed.
# Example:
# write.csv(output_data, file = output_path, row.names = FALSE)
# cat("Output saved to:", output_path, "\n")

# =====================================
# Notes for Next Steps
# =====================================
# - Examine loan status by categorical variables 
# - Analyse numeric variables by loan status using density and box plots 
# - Perform correlation analysis on numeric variables 
# - Feature engineering based on EDA insights 
# - Data preprocessing (handle missing values, scale features, encode categorical variables) 
# - Train and evaluate models iteratively
# =====================================
