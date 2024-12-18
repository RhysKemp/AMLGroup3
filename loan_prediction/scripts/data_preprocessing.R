# data_preprocessing.R

# load libs
library(dplyr)
library(caret)

# load datasets
project_dir <- getwd()
train_data_path <- file.path(project_dir, "data", "processed", "train_data.csv")
train_data <- read.csv(train_data_path)

test_data_path <- file.path(project_dir, "data", "processed", "test_data.csv")
test_data <- read.csv(test_data_path)

# function to preprocess data
preprocess_data <- function(data){
  # Omit rows with missing values
  data <- na.omit(data)
  
  # Convert categorical variables into factors
  data <- data %>%
    mutate(
      person_education = factor(person_education),
      person_home_ownership = factor(person_home_ownership),
      loan_intent = factor(loan_intent),
      loan_status = factor(loan_status),
      previous_loan_defaults_on_file = factor(previous_loan_defaults_on_file)
    )
  
  # One-hot encoding
  data_encoded <- data %>%
    bind_cols(
      model.matrix(~person_education - 1, data = .),
      model.matrix(~person_home_ownership - 1, data = .),
      model.matrix(~loan_intent - 1, data = .)
    ) %>%
    # Drop original tables
    select(-c(person_education, person_home_ownership, loan_intent))
  
  # rename columns after encoding
  data_encoded <- data_encoded %>%
    rename(
      income = person_income,
      loan_amount = loan_amnt,
      loan_interest_rate = loan_int_rate,
      loan_percent_income = loan_percent_income,
      credit_history_length = cb_person_cred_hist_length,
      credit_score = credit_score,
      
      education_Associate = person_educationAssociate,
      education_Bachelor = person_educationBachelor,
      education_Doctorate = person_educationDoctorate,
      education_High_School = `person_educationHigh School`,
      education_Master = person_educationMaster,
      
      home_ownership_Mortgage = person_home_ownershipMORTGAGE,
      home_ownership_Other = person_home_ownershipOTHER,
      home_ownership_Own = person_home_ownershipOWN,
      home_ownership_Rent = person_home_ownershipRENT,
      
      loan_intent_Debt_Consolidation = loan_intentDEBTCONSOLIDATION,
      loan_intent_Education = loan_intentEDUCATION,
      loan_intent_Home_Improvement = loan_intentHOMEIMPROVEMENT,
      loan_intent_Medical = loan_intentMEDICAL,
      loan_intent_Personal = loan_intentPERSONAL,
      loan_intent_Venture = loan_intentVENTURE,
    )
  return(data_encoded)
}

# Preprocess both training and testing data
train_data_encoded <- preprocess_data(train_data)
test_data_encoded <- preprocess_data(test_data)


# save
write.csv(train_data_encoded, file = file.path(project_dir, "data", "processed", "train_data_processed.csv"), row.names = FALSE)
write.csv(test_data_encoded, file = file.path(project_dir, "data", "processed", "test_data_processed.csv"), row.names = FALSE)

