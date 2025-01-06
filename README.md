# Loan Prediction Analysis

This project implements various machine learning models to predict loan defaults based on customer data.

## Project Structure

```
AMLGroup3/
├── data/                                           # Dataset storage
│   ├── processed/
│   └── raw/
├── models/                                         # Serialised models
├── scripts/
│   ├── data_exploration/                           # General data exploration scripts
│   │   └── visualisation/                          # Visualisation scrips
│   ├── data_processing/                            # Scripts for processing data
│   └── models/                                     # Model creation scripts
├── results/                                        # Old repo for results of models
└── README.md
```

## Workflow

1. **Data Exploration**

   - Initial data overview and summary statistics
   - Unique value analysis for each feature
   - Univariate visualisation of feature distributions
   - Bivariate analysis of relationships between features

2. **Data Preprocessing**

   - Missing value handling
   - Feature encoding
   - Data normalisation
   - Feature selection

3. **Correlation Analysis**

   - Feature correlation matrix
   - Target variable correlation analysis

4. **Model Implementation**

   - Random Forest Model
   - K-Nearest Neighbors Model
   - Regression Model

## Running the Analysis

1. Start with the data exploration scripts in order:

   ```R
   source("scripts/data_exploration/data_overview.R")
   source("scripts/data_exploration/unique_value_exploration.R")
   source("scripts/data_exploration/visualisation/univariate_visualisation.R")
   source("scripts/data_exploration/visualisation/bivariate_visualisation.R")
   ```

2. Run the preprocessing script:

   ```R
   source("scripts/data_processing/data_preprocessing.R")
   ```

3. Generate correlation visualisations:

   ```R
   source("scripts/data_exploration/visualisation/correlation.R")
   ```

4. Train and evaluate models:

   ```R
   source("scripts/models/Random_Forest_Rhys_Kemp_C2471361.R")
   source("scripts/models/K-NN Model - Matthew Hough (C2288796).R")
   source("scripts/models/Regression_Model_Yuhang_Cai_D3599185.R")
   ```
