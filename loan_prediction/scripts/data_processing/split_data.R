# UNUSED
# PREVIOUSLY USED TO SPLIT DATA INTO TWO SETS FOR ROBUST TRAINING



# Load libs
library(fs)

# set up paths
project_dir <- getwd()
raw_data_path <- file.path(project_dir, "data", "raw", "Final_Dataset.csv")
processed_data_dir <- file.path(project_dir, "data", "processed")

# validation
if (!dir.exists(processed_data_dir)) dir.create(processed_data_dir)

# load data
df <- read.csv(raw_data_path)

# split data 75/25
set.seed(42) # for reproducibility
split <- sample(1:nrow(df), size = 0.75 * nrow(df))

train_data <- df[split, ] # training
test_data <- df[-split, ] # testing

train_data_path <- file.path(processed_data_dir, "train_data.csv")
test_data_path <- file.path(processed_data_dir, "test_data.csv")

# write data
write.csv(train_data, train_data_path, row.names = FALSE)
write.csv(test_data, test_data_path, row.names = FALSE)

cat("Datasets split and saved:\n")
cat("- Training Data: ", train_data_path, "\n")
cat("- Testing Data: ", test_data_path, "\n")