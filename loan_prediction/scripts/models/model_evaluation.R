# =====================================
# File: model_evaluation.R
# Author: Rhys Kemp - C2471361
# Institution: Teesside University
# Purpose: 
#   - Load and run all models
#   - Evaluate performance of models
# =====================================

# =====================================
# Load Libraries
# =====================================

library(caret)

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

# =====================================
# Load Models
# =====================================

model_path <- file.path(project_dir, "models")

# dt_model <- readRDS(file.path(model_path, "decision_tree_model.rds"))
knn_model <- readRDS(file.path(model_path, "knn_model.rds"))
logr_model <- readRDS(file.path(model_path, "logistics_regression_model.rds"))
rf_model <- readRDS(file.path(model_path, "random_forest_model.rds"))

# =====================================
# Evaluate models
# =====================================

eval_model <- function(model, data, tar_var) {
  predictions <- predict(model, newdata = data)
  conf_matrix <- confusionMatrix(predictions, data[[tar_var]])
  return(conf_matrix)
}

tar_var <- "loan_status"

# dt_results <- eval_model(dt_model, data, tar_var)
# print(dt_results)

knn_results <- eval_model(knn_model, data, tar_var)
print(dt_results)

logr_results <- eval_model(logr_model, data, tar_var)
print(dt_results)

rf_results <- eval_model(rf_model, data, tar_var)
print(dt_results)

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

