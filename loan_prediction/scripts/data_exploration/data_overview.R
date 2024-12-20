# =====================================
# File: data_overview.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
#         - Loads the raw dataset
#         - Provides basic dataset information
#         - Checks for missing and duplicated values
#         - Displays the summary statistics
# =====================================

# =====================================
# Load Libraries
# =====================================


# =====================================
# Define File Paths
# =====================================

project_dir <- getwd()
# Input files:
input_path <- file.path(project_dir, "data", "raw", "loan_data.csv")

# =====================================
# Load Dataset
# =====================================
# Load Raw dataset
data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

# =====================================
# Basic Dataset Overview
# =====================================
# Display basic information about the dataset.

cat("Dataset Overview:\n")
cat("Number of rows: ", nrow(data), "\n")
cat("Number of columns: ", ncol(data), "\n")
cat("\nColumn names:\n")
print(colnames(data))

cat("\nFirst few rows of the dataset:\n")
print(head(data))

# =====================================
# Check for Missing & duplicated values
# =====================================

cat("\nMissing values:", sum(is.na(data)), "\n")
cat("\nDuplicated values:", sum(duplicated(data)), "\n")

# No missing or duplicated data, no further action required.

# =====================================
# Summary Statistics
# =====================================

cat("\nSummary Statistics:\n")
print(summary(data))

# =====================================
# Data Types of Columns
# =====================================

cat("\nData Types of Columns:\n")
print(sapply(data, class))


# =====================================
# Notes for Next Steps
# =====================================
# - Explore unique values
# - Exploratory Data Analysis
# - Script gives us a basic overview that will be continually referred to
# =====================================
