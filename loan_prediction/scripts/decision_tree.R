# decision_tree.R

# Load Libs
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load Dataset
project_dir <- getwd()
train_data_path <- file.path(project_dir, "data", "processed", "train_data_processed.csv")
train_data = read.csv(train_data_path)

names(train_data)

# fit model
fit <- rpart(
  loan_status ~ 
    income + 
    loan_amount + 
    loan_interest_rate + 
    loan_percent_income + 
    credit_history_length + 
    credit_score + 
    education_Associate + 
    education_Bachelor + 
    education_Doctorate + 
    education_High_School + 
    education_Master + 
    home_ownership_Mortgage + 
    home_ownership_Other + 
    home_ownership_Own + 
    home_ownership_Rent + 
    loan_intent_Debt_Consolidation + 
    loan_intent_Education + 
    loan_intent_Home_Improvement + 
    loan_intent_Medical + 
    loan_intent_Personal + 
    loan_intent_Venture + 
    previous_loan_defaults_on_file,
  
  data = train_data, 
  method = "class"
)

# Save model
saveRDS(fit, file = file.path(project_dir, "models", "decision_tree_model.rds"))


# plot
fancyRpartPlot(fit)

cat("Decision tree model training complete and model saved.")