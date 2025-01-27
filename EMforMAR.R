# Load necessary libraries
library(dplyr)   # For data manipulation
library(readxl)  # For reading Excel files
library(mice)    # For multiple imputation of missing data
library(seminr)  # For structural equation modeling

# Read the data from the Excel file
data <- read_excel("path_to_your_file\\mar_corporatereputation.xlsx")

# Check the dataset for missing values
head(data)

# Apply multiple imputation using the Expectation Maximization (EM) method
imputed_data <- mice(data, method = "norm", m = 1, maxit = 5, seed = 123)

# Retrieve the imputed dataset and check it
completed_data <- complete(imputed_data)
print("Completed dataset after imputation:")
print(completed_data)

# Define the measurement model for Partial Least Squares Structural Equation Modeling (PLS-SEM)
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Define the structural model for PLS-SEM
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = "CUSA", to = "CUSL")
)

# Estimate the PLS-SEM model using the imputed data
model_results <- estimate_pls(data = completed_data, 
                              measurement_model = corp_rep_mm, 
                              structural_model = corp_rep_sm)

# Print the model loadings summary
print("Model loadings:")
print(summary(model_results)$loadings)

# Set parameters for iterative model estimation and convergence checking
max_iter <- 100  # Maximum number of iterations
tolerance <- 0.0001  # Tolerance threshold for convergence
previous_loadings <- NULL  # Variable to store loadings from the previous iteration
converged <- FALSE  # Flag to check if the model has converged
converged_data <- NULL  # Variable to store the data at convergence

# Start the iterative process for model estimation
for (i in 1:max_iter) {
  imputed_data <- mice(data, m=1, method='pmm', maxit=50, seed=500)
  new_data <- complete(imputed_data, 1)
  
  model_results <- estimate_pls(data = new_data, 
                                measurement_model = corp_rep_mm, 
                                structural_model = corp_rep_sm)
  
  current_loadings <- summary(model_results)$loadings
  
  # Check if the model has converged
  if (!is.null(previous_loadings)) {
    loadings_diff <- max(abs(current_loadings - previous_loadings))
    if (loadings_diff < tolerance) {
      converged <- TRUE
      converged_data <- new_data  # Save the converged dataset
      break
    }
  }
  
  previous_loadings <- current_loadings
}

# Output results of the model estimation
if (converged) {
  print(paste("Model converged at iteration", i))
  print("Summary of the dataset at convergence:")
  summary(converged_data)
  write.csv(converged_data, "converged_data.csv", row.names = FALSE)
  print("Converged dataset saved as 'converged_data.csv'")
} else {
  print("Model did not converge within the maximum number of iterations.")
}
