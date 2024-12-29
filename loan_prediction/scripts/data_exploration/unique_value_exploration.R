# =====================================
# File: unique_value_exploration.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
#         - Loads the raw dataset.
#         - Displays the number of unique values for each column.
#         - Separates and identifies numerical and categorical columns.
#         - Displays unique values for categorical columns.
#         - Summarises unique values for numerical columns.
#         - Provides insights for data quality and preprocessing.
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

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

# =====================================
# Display Unique Values
# =====================================

# Display the number of unique values for each column
unique_values <- sapply(data, function(col) length(unique(col)))
cat("\nNumber of unique values in each column:\n")
print(unique_values)

# =====================================
# Display Categorical and Numerical
# =====================================

# Separate numerical and categorical columns
numerical_cols <- names(data)[sapply(data, is.numeric)]
categorical_cols <- names(data)[!sapply(data, is.numeric)]

# Display Numerical column names
cat("\nNumerical columns:\n")
print(numerical_cols)

# Display Categorical column names
cat("\nCategorical columns:\n")
print(categorical_cols)

# =====================================
# Check for infinite and undefined values
# =====================================

# Check for infinite or NaN values in numerical columns
for (col in numerical_cols) {
  cat("\nInfinite values in", col, ":", sum(is.infinite(data[[col]])))
  cat("\nNaN values in", col, ":", sum(is.nan(data[[col]])), "\n")
}

# No Infinite or NaN values, no further action required.

# =====================================
# Display Unique Categorical
# =====================================

# Display unique values for all non-numerical categories
cat("\nUnique values for categorical columns:\n")
for (col in categorical_cols) {
  cat(paste0(col, ":\n"))
  print(unique(data[[col]]))
  cat("\n")
}

# =====================================
# Notes for Next Steps
# =====================================
# - Exploratory Data Analysis (EDA).
# - Preprocess categorical data.
# - Handle numerical column distributions.
# =====================================
