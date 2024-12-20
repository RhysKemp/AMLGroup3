# =====================================
# File: univariate_visualisation.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: [Brief description of the script's purpose, e.g., data exploration, 
#           preprocessing, EDA, model training, or evaluation.]
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

create_bar_plot <- function(data, x_column, fill_column = NULL,
                            x_labels = NULL, plot_title = "Bar Plot",
                            x_axis_title = "X-Axis", y_axis_title = "Frequency",
                            fill_pallette = bar_palette_one ) {
  
  # Convert columns to factors if necessary
  data[[x_column]] <- factor(data[[x_column]])
  if (!is.null(fill_column)) {
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

# TODO:
#     - add pie chart helper function


# =====================================
# Visualise Loan Status
# =====================================

# Prepare data for pie chart
loan_status_count <- table(data$loan_status)
loan_status_percent <- loan_status_count / sum(loan_status_count) * 100

loan_status_df <- data.frame(
  loan_status = names(loan_status_count),
  count = as.integer(loan_status_count),
  percentage = round(as.numeric(loan_status_percent), 1)
)

b_plot <- create_bar_plot(
  data = data, 
  x_column = "loan_status", 
  x_labels = c("Rejected (0)", "Approved (1)"), 
  plot_title = "Loan Status Distribution", 
  x_axis_title = "Loan Status", 
  y_axis_title = "Frequency"
)

b_plot <- b_plot +
  guides(fill = guide_legend(
    title = "Loan Status", 
    title.position = "top", 
    title.theme = element_text(size = 12, face = "bold", color = "black"),
    label.theme = element_text(size = 10, face = "bold", color = "black")
  ))

print(b_plot)


# TODO:
#     - Fix y_interval param
#     - add pie chart for percentage distribution
#     - refactor all distribution code to utilise
plot_distribution_column <- function(data, column, y_interval = NULL) {
  
  # Column handling
  column_title <- gsub("_", " ", column)
  
  
  # Create bar plot
  bar_title <- paste(column_title, "Distribution")
  bar_plot <- create_bar_plot(
    data = data,
    column,
    x_axis_title = column_title,
    plot_title = bar_title,
    )
  
  
  bar_plot <- bar_plot +
    guides(fill = guide_legend(
      title = "Loan Status", 
      title.position = "top", 
      title.theme = element_text(size = 12, face = "bold", color = "black"),
      label.theme = element_text(size = 10, face = "bold", color = "black")
    ))
  
  print(bar_plot)
  
}

plot_distribution_column(data, "loan_amount", 5000)

# Plot Bar chart
bar_plot <- ggplot(data, aes(x = factor(loan_status), fill = factor(loan_status))) +
  geom_bar(color = "black", alpha = 1, stat = "count") +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    vjust = -0.5, 
    color = "black", 
    size = 5
  ) +
  # Aesthetics and theme
  scale_x_discrete(labels = c("Rejected", "Approved")) +
  scale_y_continuous(breaks = seq(0, max(table(data$loan_status)), by = 5000)) +
  scale_fill_brewer(
    palette = bar_palette_one,
    labels = c("Rejected (0)", "Approved (1)")
    ) +
  labs(
    title = "Loan Status Distribution",
    x = "Loan Status",
    y = "Frequency"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    plot.title = element_text(color = "black", size = 16, face = "bold"),
    legend.background = element_rect(color = "black", linewidth = 1),
    legend.title = element_text(size = 12, face = "bold", color = "black")
  ) +
  guides(fill = guide_legend(title = "Loan Status"))

# Plot Pie Chart
pie_plot <- ggplot(loan_status_df, aes(x = "", y = percentage, fill = loan_status)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +  # Turn bar chart into pie chart
  geom_text(
    aes(label = paste0(percentage, "%")), 
    position = position_stack(vjust = 0.5), 
    color = "white", 
    size = 5
  ) +
  scale_fill_brewer(
    palette = pie_palette_one,
    labels = c("Rejected (0)", "Approved (1)")
    ) +
  labs(
    title = "Loan Status Percentage Distribution"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold"),
    legend.position = "none",
  )

# Plot both in a grid
grid.arrange(bar_plot, pie_plot, ncol = 2)

# =====================================
# Univariate Analysis for Numeric Columns
# Distribution with KDE
# =====================================

# List of numerical columns for analysis
numerical_columns_to_analyse <- c('person_age', 'person_income', 'person_emp_exp', 'loan_amnt', 
                        'loan_int_rate', 'loan_percent_income', 'cb_person_cred_hist_length', 
                        'credit_score')

# Function for univariate analysis on numeric columns
univariate_analysis <- function(data, columns) {
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


univariate_analysis(data, numerical_columns_to_analyse)

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
      panel.grid.major.x = element_line(color = "gray90", size = 0.5), # gridlines only on x axis
      panel.grid.minor.x = element_line(color = "gray90", size = 0.2),
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

# Function to plot distribution of categorical columns and percentage distribution
plot_categorical_distribution <- function(data, column) {
  # Bar plot
  bar_plot <- ggplot(data, aes(x = factor(.data[[column]]), fill = factor(.data[[column]]))) +
    geom_bar(
      stat = "count",
      color = "black"
    ) +
    geom_text(
      aes(label = ..count..), 
      stat = "count", 
      vjust = -0.5, 
      color = "black", 
      size = 5
    ) +
    labs(
      title = paste(gsub("_", " ", column), "Distribution"),
      x = gsub("_", " ", column),
      y = "Count"
      ) +
    scale_fill_brewer(palette = bar_palette_one) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
      )
  print(bar_plot)
}
 
for (column in categorical_columns_to_analyse) {
  plot_categorical_distribution(data, column)
  }

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
