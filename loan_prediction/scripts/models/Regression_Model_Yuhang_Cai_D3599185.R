# Install necessary packages
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("pscl")
# install.packages("ROCR")

# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(pscl)
library(ROCR)

# =====================================
# Define File Paths
# =====================================
# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "processed", "loan_data_processed.csv")

# Output files:
output_path <- file.path(project_dir, "models", "logistics_regression_model.rds")

# =====================================
# Load data
# =====================================

final_data <- read.csv(input_path)

# View the first few rows of the dataset to quickly understand its general appearance
head(final_data)

# View the structure of the dataset to know the data types and dimensions of each variable
str(final_data)

# =====================================
# Processing data
# =====================================

# Check for missing values in the dataset and print the number of missing values for each variable
missing_values <- colSums(is.na(final_data))
print(missing_values)
# No missing values

# Normalize the data to make variables with different numerical ranges comparable, which helps with model convergence and performance improvement
# Define a normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Get the indices of numeric columns (excluding categorical variable columns)
numeric_cols <- sapply(final_data, is.numeric)
# Apply normalization transformation to numeric columns
final_data[, numeric_cols] <- as.data.frame(lapply(final_data[, numeric_cols], normalize))

# View the structure of the dataset after normalization to confirm the state of the data after processing
str(final_data)

# Extract the names of numeric variables (excluding the target variable loan_status) for subsequent analysis
numeric_vars <- names(final_data)[sapply(final_data, is.numeric)][names(final_data)[sapply(final_data, is.numeric)]!= "loan_status"]

# Correlation analysis: Calculate the correlations among all numeric variables, with a focus on the correlations related to the target variable loan_status
cor_matrix <- cor(final_data[, numeric_cols])
print(cor_matrix)

# =====================================
# Split data
# =====================================

# Split the data into training and test sets 
set.seed(42)
train_index <- sample(1:nrow(final_data), 0.75 * nrow(final_data))
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

# =====================================
# Build Model
# =====================================

# Build a logistic regression model 
formula_str <- paste("loan_status", "~", paste(names(final_data)[names(final_data)!= "loan_status"], collapse = " + "))
logisticMod <- glm(formula(formula_str), data = train_data, family = binomial())

# Diagnose the logistic regression model 
summary(logisticMod)

# Check some key metrics of the model
print(paste("AIC:", AIC(logisticMod)))
print(paste("BIC:", BIC(logisticMod)))

# Use the McFadden R2 index to evaluate the model fit
model_fit_measures <- pR2(logisticMod)
print(paste("McFadden R2:", model_fit_measures["McFadden"]))

# Make predictions on the test set and evaluate the prediction ability
# Predict to get the results in probability form for later conversion to class predictions based on a threshold
predictions <- predict(logisticMod, newdata = test_data, type = "response")

# Convert the probabilities to class predictions based on a set threshold 
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# =====================================
# Evaluation
# =====================================

# Build a confusion matrix to comprehensively evaluate the model performance 
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$loan_status))
print(confusion_matrix)

# Calculate the accuracy 
accuracy <- confusion_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

# Plot the ROC curve and calculate the AUC (using the ROCR package) to visually display the classification performance of the model
prediction_obj <- prediction(predictions, test_data$loan_status)
performance_obj <- performance(prediction_obj, measure = "tpr", x.measure = "fpr")

# Use ggplot to draw a more visually appealing ROC curve
roc_df <- data.frame(
  fpr = unlist(performance_obj@x.values),
  tpr = unlist(performance_obj@y.values)
)
ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "royalblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

auc_value <- performance(prediction_obj, measure = "auc")
auc_value <- auc_value@y.values[[1]]
print(paste("AUC:", auc_value))

# Visualize the relationship between predicted values and actual values 
actuals_preds <- data.frame(actuals = test_data$loan_status, predicteds = predicted_classes)
ggplot(actuals_preds, aes(x = actuals, y = predicteds)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  labs(title = "Actual vs Predicted Values for loan_status",
       x = "Actual loan_status",
       y = "Predicted loan_status") +
  theme_minimal()

# =====================================
# Cross Validation
# =====================================

# k - fold cross-validation 
k_folds <- 5
cv_results <- vector("list", k_folds)
for (i in 1:k_folds) {
  # Split the training and test sets
  test_indices <- ((i - 1) * (nrow(final_data) / k_folds) + 1):(i * (nrow(final_data) / k_folds))
  train_indices <- setdiff(1:nrow(final_data), test_indices)
  train_data_fold <- final_data[train_indices, ]
  test_data_fold <- final_data[test_indices, ]
  
  # Train a logistic regression model on the training set
  model_fold <- glm(formula(formula_str), data = train_data_fold, family = binomial())
  
  # Make predictions on the test set and get the prediction probabilities
  predictions_fold <- predict(model_fold, newdata = test_data_fold, type = "response")
  
  # Convert the prediction probabilities to class predictions
  predicted_classes_fold <- ifelse(predictions_fold > 0.5, 1, 0)
  
  # Build a confusion matrix to evaluate the model performance in each cross-validation iteration
  confusion_matrix_fold <- confusionMatrix(as.factor(predicted_classes_fold), as.factor(test_data_fold$loan_status))
  
  # Calculate the accuracy and save it to the result list 
  accuracy_fold <- confusion_matrix_fold$overall["Accuracy"]
  cv_results[[i]] <- accuracy_fold
}

# View the average accuracy in k - fold cross-validation
mean_accuracy_cv <- mean(unlist(cv_results))
print(paste("Mean Accuracy in k - fold Cross - Validation:", mean_accuracy_cv))

# View the accuracy in each fold
print(paste("Accuracy in each fold:", paste(cv_results, collapse = ", ")))


# =====================================
# Save Results
# =====================================

saveRDS(logisticMod, file = output_path)
cat("Logistic Regression model saved to:", output_path, "\n")

