# =====================================
# Load Libraries
# =====================================

#install.packages("class")
#install.packages("ggplot2")

library(class)
library(ggplot2)

# =====================================
# Define File Paths
# =====================================

# Set up project directory and paths for inputs and outputs.
project_dir <- getwd()

# Input files:
input_path <- file.path(project_dir, "data", "processed", "loan_data_processed.csv")

# Output files:
output_path <- file.path(project_dir, "models", "knn_model.rds")

# =====================================
# Load Dataset
# =====================================

# Loading the processed dataset
la <- read.csv(input_path)

# Checking the structure and summary after pre-processing
str(la)
summary(la)

# =====================================
# K-NN Preparation 
# =====================================

# Sub-setting the columns from the processed dataset
la.subset <- la[c(
  'loan_status', 'person_age', 'person_gender', 'person_education',
  'person_income', 'loan_amnt', 'loan_int_rate', 'loan_percent_income',
  'cb_person_cred_hist_length', 'credit_score', 'previous_loan_defaults_on_file',
  'person_home_ownershipMORTGAGE', 'person_home_ownershipOTHER',
  'person_home_ownershipOWN', 'person_home_ownershipRENT',
  'loan_intentDEBTCONSOLIDATION', 'loan_intentEDUCATION',
  'loan_intentHOMEIMPROVEMENT', 'loan_intentMEDICAL',
  'loan_intentPERSONAL', 'loan_intentVENTURE'
)]

# Creating a normalize function - this is to help with improving the performance & accuracy of the k-NN model
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Excluding the first column which is our target variable, 'loan_status'
la.subset.n <- as.data.frame(lapply(la.subset[, 2:ncol(la.subset)], normalize))

# Adding back the target variable, 'loan_status'
la.subset.n$loan_status <- la.subset$loan_status

# To get the same random sample 
set.seed(123)

# Random selection of 70% data for training
dat.d <- sample(1:nrow(la.subset.n), size = nrow(la.subset.n) * 0.75, replace = FALSE)

# Splitting data into training and testing sets
train.la <- la.subset.n[dat.d,]
test.la <- la.subset.n[-dat.d,]

# Separating labels (target variable) for loan_status
train.la_labels <- train.la$loan_status
test.la_labels <- test.la$loan_status

# Removing loan_status from the training and testing sets
train.la <- train.la[, -ncol(train.la)]
test.la <- test.la[, -ncol(test.la)]

# Checking the rows in the training data & testing data
cat("Training Data Rows:\n")
print(table(train.la_labels))

cat("Testing Data Rows:\n")
print(table(test.la_labels))

# =====================================
# K-NN Training Model
# =====================================

# Choosing 177 & 178 for K number as the total number of rows are 31,499 - the square root value is 177.479
knn.177 <- knn(train = train.la, test = test.la, cl = train.la_labels, k = 177)
knn.178 <- knn(train = train.la, test = test.la, cl = train.la_labels, k = 178)

# Gaining the accuracy of the model with the 2 different K numbers 
ACC.177 <- 100 * sum(test.la_labels == knn.177) / NROW(test.la_labels)
ACC.178 <- 100 * sum(test.la_labels == knn.178) / NROW(test.la_labels)

# Outputting the accuracy
cat("KNN 177 Accuracy:", ACC.177, "%\n")
cat("KNN 178 Accuracy:", ACC.178, "%\n")

# Checking prediction against actual value in tabular form
table(knn.177, test.la_labels) 
table(knn.178, test.la_labels)




# =====================================
# Evaluate Performance Metrics
# =====================================

# install.packages("caret")
library(caret)

knn.177 <- factor(knn.177, levels = unique(c(as.character(knn.177), as.character(test.la_labels))))
test.la_labels <- factor(test.la_labels, levels = unique(c(as.character(knn.177), as.character(test.la_labels))))

# Confusion Matrix for KNN with k = 177
conf_matrix_177 <- confusionMatrix(knn.177, test.la_labels)

# Extracting Metrics for k = 177
sensitivity_177 <- conf_matrix_177$byClass["Sensitivity"]
accuracy_177 <- conf_matrix_177$overall["Accuracy"]
kappa_177 <- conf_matrix_177$overall["Kappa"]

# Printing Metrics for k = 177
cat("For k = 177:\n")
cat("Sensitivity:", sensitivity_177, "\n")
cat("Accuracy:", accuracy_177, "\n")
cat("Kappa:", kappa_177, "\n\n")

# Confusion Matrix for KNN with k = 178
conf_matrix_178 <- confusionMatrix(knn.178, test.la_labels)

# Extracting Metrics for k = 178
sensitivity_178 <- conf_matrix_178$byClass["Sensitivity"]
accuracy_178 <- conf_matrix_178$overall["Accuracy"]
kappa_178 <- conf_matrix_178$overall["Kappa"]

# Printing Metrics for k = 178
cat("For k = 178:\n")
cat("Sensitivity:", sensitivity_178, "\n")
cat("Accuracy:", accuracy_178, "\n")
cat("Kappa:", kappa_178, "\n")


























# ========================================================
# K-NN Model Performance Trend with Different K Numbers
# ========================================================

# Declarations to initiate for loop 
i = 1
k.optm = 1

# Creating for loop 
for (i in 1:30) { 
  knn.mod <- knn(train = train.la, test = test.la, cl = train.la_labels, k = i)
  k.optm[i] <- 100 * sum(test.la_labels == knn.mod) / NROW(test.la_labels)
  k = i
  cat(k, '=', k.optm[i],'\n')
  }

# Plotting % accuracy to K-Value
plot (k.optm, type = "b", xlab = "K- Value", ylab = "Accuracy Level")

# =====================================
# Save Results
# =====================================

saveRDS(knn.mod, file = output_path)
