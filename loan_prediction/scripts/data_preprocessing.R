# =====================================
# File: data_preprocessing.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
# - Load the loan dataset.
# - Apply necessary data transformations and encodings.
# - Prepare the data for analysis and model training.
# =====================================

# =====================================
# Load Libraries
# =====================================


# =====================================
# Define File Paths
# =====================================

# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "raw", "loan_data.csv")

# Output files:
output_path <- file.path(project_dir, "data", "processed", "loan_data_processed.csv")

# =====================================
# Load Dataset
# =====================================

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

# Show head for verification
cat("\nFirst few rows of the dataset:\n")
print(head(data))

# =====================================
# Binary Encoding
# =====================================

# Find unique values
unique(data$person_gender)

# Perform encoding
data$person_gender <- ifelse(data$person_gender == "male", 1, 0)

# Verify
cat("Binary encoding for 'person_gender' completed. Unique values in new column:", unique(data$person_gender_binary), "\n")

# =====================================
# Ordinal Encoding
# =====================================

# Find unique values
unique(data$person_education)

# Define order
education_order <- c("High School", "Associate", "Bachelor", "Master", "Doctorate")

# Perform encoding
data$person_education <- factor(data$person_education, levels = education_order, ordered = TRUE)
data$person_education <- as.numeric(data$person_education)

# Verify
cat("Ordinal encoding for 'person_education' completed. Unique values in the column:", unique(data$person_education), "\n")

# =====================================
# One-hot encoding
# =====================================

# Find unique values
cat("Unique values for 'person_home_ownership':", unique(data$person_home_ownership), "\n")
cat("Unique values for 'loan_intent':", unique(data$loan_intent), "\n")

# Perform encoding
# person_home_ownership
data <- cbind(data, model.matrix(~ person_home_ownership - 1, data = data))

# loan_intent
data <- cbind(data, model.matrix(~ loan_intent - 1, data = data))

# Drop original columns
data$person_home_ownership <- NULL
data$loan_intent <- NULL

# Verify
cat("One-hot encoding completed. Columns after encoding:", colnames(data), "\n")

# =====================================
# Display dataframe
# =====================================

cat("\nFirst few rows of the dataset:\n")
print(head(data))

# =====================================
# Replace age outlier with median value
# =====================================

# Display summary for person age
cat("\nSummary Statistics for person_age")
print(summary(data[["person_age"]]))
# 144 age is way past reasonable lifespan, 100 years seems reasonable

# Calulate median
median_age <- median(data$person_age, na.rm = TRUE)

# Replace any age greater than 100 with median
data$person_age[data$person_age > 100] <- median_age

# Verify
print(summary(data[["person_age"]]))

# =====================================
# Save Results
# =====================================

write.csv(data, file = output_path, row.names = FALSE)
cat("Processed data saved to:", output_path, "\n")