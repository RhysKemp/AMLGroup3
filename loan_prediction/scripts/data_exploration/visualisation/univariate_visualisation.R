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

# =====================================
# Define File Paths
# =====================================
# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "raw", "loan_data.csv")

# Output files:
output_path <- file.path(project_dir, "[output_folder]", "[output_file_name.csv]")

# =====================================
# Load Dataset
# =====================================

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

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

head(loan_status_df)

# Plot Bar chart
bar_plot <- ggplot(data, aes(x = factor(loan_status), fill = factor(loan_status))) +
  geom_bar(color = "black", alpha = 0.7, stat = "count") +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    vjust = -0.5, 
    color = "black", 
    size = 5
  ) +
  scale_x_discrete(labels = c("Rejected", "Approved")) +
  scale_y_continuous(breaks = seq(0, max(table(data$loan_status)), by = 5000)) +
  scale_fill_brewer(
    palette = "Dark2",
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
    legend.background = element_rect(color = "black", size = 1),
    legend.title = element_text(size = 12, face = "bold", color = "black")  # Set legend title style
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
  scale_fill_brewer(palette = "Dark2", labels = c("Rejected (0)", "Approved (1)")) +
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
# Univarate Analysis for Numeric Columns
# =====================================

univariate_analysis <- function(data, columns) {
  plot_list <- list()
  
  # Loop through each column specified in provided df
  for (i in seq_along(columns)) {
    column <- columns[i]
    
    # Create histogram with density curve 
    # TODO:
    #     - Fix Scaling issues
    #     - Fix aesthetics 
    #     - Understand more thoroughly
    p <- ggplot(data, aes_string(x = column)) + 
      geom_histogram(
        aes(y = ..density..),
        bins = 30,
        fill = brewer.pal(3, "Blues")[2],
        color = "black", alpha = 0.7
        ) + 
      geom_density(
        color = brewer.pal(3, "Blues")[3],
        size = 1
        ) + 
      labs(
        title = paste(gsub("_", " ", column), "Distribution with KDE"),
        x = gsub("_", " ", column),
        y = "Density"
        ) +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1") +
      theme_classic() +
      theme(
        plot.title = element_text(color = "black", size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(color = "black"),
        axis.title = element_text(size = 12),
        plot.margin = unit(c(1, 1, 1, 1), "lines")  # Adjust margins for better spacing
      )
    
    # Add plot to list
    plot_list[[i]] <- p
  }
  
  grid.arrange(grobs = plot_list, ncol = 2)
}

columns_to_analyze <- c('person_age', 'person_income', 'person_emp_exp', 'loan_amnt', 
                        'loan_int_rate', 'loan_percent_income', 'cb_person_cred_hist_length', 
                        'credit_score')

univariate_analysis(data, columns_to_analyze)

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
