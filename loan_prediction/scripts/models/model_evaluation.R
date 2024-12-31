# model_evaluation.R

# Load libs
library(rpart)
library(caret)









# MODEL NAME
model_name <- "decision_tree" # Change for relevant model
# file name structure should be "model_name_model.rds"











# Load Test data
project_dir <- getwd()
test_data_path <- file.path(project_dir, "data", "processed", "test_data_processed.csv")
test_data <- read.csv(test_data_path)

# Load Model
model_file <- paste0(model_name, "_model.rds")  # Assuming models are saved with this naming convention
fit <- readRDS(file.path(project_dir, "models", model_file))
Prediction <- predict(fit, test_data, type = "class")

# Generate confusion matrix and accuracy
conf_matrix <- table(test_data$loan_status, Prediction)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print matrix and accuracy
cat("Accuracy: ", accuracy * 100, "%\n")
print(conf_matrix)

# prepare results
submit <- data.frame(loan_status = Prediction, accuracy = rep(accuracy, length(Prediction)))

# Define the results folder and file name
results_folder <- file.path(project_dir, "results")
if (!dir.exists(results_folder)) {
  dir.create(results_folder)
}

# Dynamic file naming
accuracy_str <- sprintf("%.2f", accuracy * 100)
accuracy_str <- gsub("\\.", "_", accuracy_str) 
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_name <- paste0(model_name, "_predictions_", accuracy_str, "_accuracy_", timestamp, ".csv")

# Write to csv
write.csv(submit, file = file.path(results_folder, file_name), row.names = FALSE)

cat("Prediction results saved to '", file.path(results_folder, file_name), "'.\n")