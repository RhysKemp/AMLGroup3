# =====================================
# File: correlation_visualisation.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
# - Create correlation matrix
# - Visualise heat map
# - Infer correlations from heat map
# =====================================

# =====================================
# Load Libraries
# =====================================

library(ggplot2)
library(corrplot)
library(reshape2)
library(RColorBrewer)

# =====================================
# Define File Paths
# =====================================
# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "processed", "loan_data_processed.csv")


# =====================================
# Load Dataset
# =====================================

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

cat("\nFirst few rows of the dataset:\n")
print(head(data))

# =====================================
# Create Correlation matrix
# =====================================

corr_matrix <- cor(data, use = "complete.obs")
corr_melted <- melt(corr_matrix) # Reshape for ggplot

# =====================================
# Plot Correlation Heatmap
# =====================================

ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
  scale_y_discrete(limits = rev(levels(corr_melted$Var1))) + # Reverse
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 2, fontface = "bold") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
    axis.text.y = element_text(angle = 0, hjust = 1, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.key.height = unit(1, "npc") * 0.1
    )


# =====================================
# Re plot matrix with respect
# to target variable (loan status)
# =====================================

# Extract correlations with target variable
tar_var <- "loan_status"
tar_corr <- corr_matrix[, tar_var, drop = FALSE]
tar_corr <- as.data.frame(tar_corr)
tar_corr$Feature <- rownames(tar_corr)
colnames(tar_corr) <- c("Correlation", "Feature")

tar_corr <- tar_corr[order(tar_corr$Correlation, decreasing = TRUE), ] # Sort by value

ggplot(tar_corr, aes(x = "", y = reorder(Feature, Correlation), fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.2)) +
  labs(title = paste("Correlation with", tar_var), x = NULL, y = NULL) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 4, fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.key.height = unit(1, "npc") * 0.15
        )

# =====================================