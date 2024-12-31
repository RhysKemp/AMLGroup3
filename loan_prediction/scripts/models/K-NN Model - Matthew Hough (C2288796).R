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
data$previous_loan_defaults_on_file <- ifelse(data$previous_loan_defaults_on_file == "Yes", 1, 0)

# Verify
cat("Binary encoding for 'person_gender' completed. Unique values in new column:", unique(data$person_gender_binary), "\n")
cat("Binary encoding for 'previous_loan_defaults_on_file' completed. Unique values in new column:", unique(data$person_gender_binary), "\n")

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

# Loading the processed dataset
la <- read.csv(output_path)

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

saveRDS(knn.mod, file = "models/knn_model.rds")
