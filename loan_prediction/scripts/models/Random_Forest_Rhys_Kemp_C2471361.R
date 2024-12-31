# =====================================
# File: Random_Forest_Rhys_Kemp_C2471361.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose:
#   - Train Random Forest model
#   - Minor evaluation
#   - Save model to /models as random_forest_model.rds
# =====================================

# =====================================
# Load Libraries
# =====================================

library(randomForest)
library(caret)
library(ggplot2)

# =====================================
# Define File Paths
# =====================================
# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "processed", "loan_data_processed.csv")

# Output files:
output_path <- file.path(project_dir, "models", "random_forest_model.rds")

# =====================================
# Load Dataset
# =====================================

data <- read.csv(input_path)
cat("Dataset loaded from:", input_path, "\n")

# =====================================
# Prepare data
# =====================================

# Ensure target variable is a factor
data$loan_status <- as.factor(data$loan_status)

# Split dataset
set.seed(42)
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# =====================================
# Train model
# =====================================

start_time <- Sys.time()  # Timer

fit <- randomForest(loan_status ~ ., data = train_data, ntree = 100, mtry = 5, importance = TRUE) # Values yielded best results
cat("Model training completed.\n")

end_time <- Sys.time()  # End timer
training_time <- difftime(end_time, start_time, units = "secs")
cat("Model training completed in:", training_time, "seconds.\n")

# =====================================
# Evaluate model
# =====================================

start_time <- Sys.time()

# Predict on test data
predictions <- predict(fit, test_data)

end_time <- Sys.time()  # End timer
training_time <- difftime(end_time, start_time, units = "secs")
cat("Predictions completed in:", training_time, "seconds.\n")

conf_matrix <- confusionMatrix(predictions, test_data$loan_status)
cat("Confusion Matrix:\n")
print(conf_matrix)

# =====================================
# Feature Importance
# =====================================

importance <- importance(fit)
cat("Feature importance:\n")
print(importance)

# Plot importance
importance_df <- data.frame(Feature = rownames(importance), Importance = importance[,1])

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")

# =====================================
# Save Results
# =====================================

saveRDS(fit, file = output_path)
cat("Random Forest model saved to:", output_path, "\n")

