# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files
library(mice)       # For multiple imputation of missing data
library(seminr)     # For structural equation modeling

# Read the data from an Excel file
data <- read_excel("C:\\Users\\liuyi\\OneDrive - must.edu.mo\\Ph.D stu\\Yide\\lushan\\multiple\\nmar_corporatereputation.xlsx")

# Apply multiple imputation using the EM method via the 'mice' package
imputed_data <- mice(data, method = "norm", m = 1, maxit = 5, seed = 123)

# Obtain and view the imputed dataset
completed_data <- complete(imputed_data)
print("Imputed dataset:")
print(completed_data)

# Define measurement and structural models for PLS-SEM analysis
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = "CUSA", to = "CUSL")
)

# Estimate the PLS-SEM model using the seminr package on the completed data
model_results <- estimate_pls(data = completed_data, 
                              measurement_model = corp_rep_mm, 
                              structural_model = corp_rep_sm)
print(summary_model$loadings)

# Set parameters for iterative estimation and model convergence checking
max_iter <- 100  # Maximum number of iterations to perform
tolerance <- 0.0001  # Tolerance level for convergence

# Begin iterative process to estimate and refine the model
for (i in 1:max_iter) {
  imputed_data <- mice(data, m=1, method='pmm', maxit=50, seed=500)
  new_data <- complete(imputed_data, 1)
  
  model_results <- estimate_pls(data = new_data, 
                                measurement_model = corp_rep_mm, 
                                structural_model = corp_rep_sm)
  current_loadings <- summary(model_results)$loadings
  
  # Check for convergence by comparing loadings change
  if (!is.null(previous_loadings) && max(abs(current_loadings - previous_loadings)) < tolerance) {
    converged <- TRUE
    converged_data <- new_data  # Save the converged dataset
    break
  }
  previous_loadings <- current_loadings
}

# Output results and check dataset summary at convergence
if (converged) {
  print(paste("Model converged at iteration", i))
  print("Summary of the dataset at convergence:")
  summary(converged_data)
  write.csv(converged_data, "converged_data.csv", row.names = FALSE)
  print("Converged dataset saved as 'converged_data.csv'")
} else {
  print("Model did not converge within the set maximum number of iterations.")
}

# Calculate and view weights based on the minimum values of competence indicators
# This ensures that observations with lower scores have higher influence in the analysis
converged_data$weights <- 1 / (pmin(data$comp_1, data$comp_2, data$comp_3) + 0.1)
head(converged_data$weights)
View(converged_data)

# Normalize and standardize the weights for further analysis
converged_data$normalized_weights <- (converged_data$weights - min(converged_data$weights)) / (max(converged_data$weights) - min(converged_data$weights))
converged_data$standardized_weights <- (converged_data$weights - mean(converged_data$weights)) / sd(converged_data$weights)

# View statistical summaries of normalized and standardized weights
summary(converged_data$normalized_weights)
summary(converged_data$standardized_weights)

# Save the processed data with metric weights for future analysis
write.csv(converged_data, "converged_data_with_metric_weights.csv", row.names = FALSE)
print("Processed dataset with metric weights saved as 'converged_data_with_metric_weights.csv'")
