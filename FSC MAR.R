# Load necessary libraries
library(dplyr)
library(readxl)
library(mice)
library(seminr)

# Read the data from the Excel file
data <- read_excel("path_to_your_file\\corporatereputation.xlsx")

# Define base missing probabilities and adjustment factors for each serviceprovider category
a <- c(0.1, 0.15, 0.2, 0.25)  # Base missing probabilities for each category
b <- c(0.05, 0.05, 0.05, 0.05)  # Adjustment factors for each category

# Generate a random customer satisfaction score for demonstration
set.seed(123)  # For reproducibility
customer_satisfaction <- runif(n = nrow(data), min = 0, max = 10)

# Define the function to calculate missing probability
missing_probability <- function(satisfaction, serviceprovider) {
  sp_index <- as.numeric(serviceprovider)  # Convert serviceprovider to a numeric index
  prob <- min(a[sp_index] + b[sp_index] * (-satisfaction), 1)  # Calculate the missing probability
  return(prob)
}

# Columns to introduce missing data
columns_to_miss <- c("comp_1", "comp_2", "comp_3")

# Apply missing data function to the dataset
set.seed(123)  # For reproducibility
for (i in 1:nrow(data)) {
  for (col in columns_to_miss) {
    prob <- missing_probability(customer_satisfaction[i], data$serviceprovider[i])
    if (runif(1) < prob) {
      data[[col]][i] <- NA  # Set to NA based on calculated probability
    }
  }
}

# Check the dataset for missing values
head(data)

# data$comp_1 <- as.factor(data$comp_1)
# data$comp_2 <- as.factor(data$comp_2)
# data$comp_3 <- as.factor(data$comp_3)

# Apply multiple imputation using MCMC method
#imputed_data <- mice(data, method = "polr", m = 5, seed = 123)
#imputed_data <- mice(data, method = "norm", m = 1, maxit = 5, seed = 123)
imputed_data <- mice(data, method = "pmm", m = 5, seed = 123)



# Define measurement model for PLS-SEM analysis
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Define structural model for PLS-SEM analysis
corp_rep_sm <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = "CUSA", to = "CUSL")
)

# Initialize lists to store results for each analysis
model_results <- list()
path_coefficients_list <- list()
reliability_metrics_list <- list()
loadings_metrics_list <- list()
htmt_metrics_list <- list()
r_squared_list <- list()
vif_list <- list()
IT_list <- list()


# Analyze each imputed dataset
for (i in 1:5) {
  # Retrieve the ith imputed dataset
  imputed_dataset <- complete(imputed_data, action = i)
  imputed_dataset$comp_1 <- as.numeric(as.character(imputed_dataset$comp_1))
  
  # Perform PLS-SEM analysis
  corp_rep_pls_model <- estimate_pls(
    data = imputed_dataset,
    measurement_model = corp_rep_mm, # Ensure these models are correctly defined
    structural_model = corp_rep_sm
  )
  
  # Store the results of the model
  model_results[[i]] <- corp_rep_pls_model
  
  # Store key statistical indicators
  summary_model <- summary(corp_rep_pls_model)
  path_coefficients_list[[i]] <- summary_model$paths
  r_squared_list[[i]] <- summary_model$R2
  reliability_metrics_list[[i]] <- summary_model$reliability
  loadings_metrics_list[[i]] <- summary_model$loadings
  htmt_metrics_list [[i]] <- summary_model$validity$htmt
  vif_list [[i]] <- summary_model$validity$vif_items
  IT_list [[i]] <- summary_model$it_criteria
}


# Iterate and print detailed results for each model
for (i in 1:length(model_results)) {
  cat("Model", i, "Results:\n")
  summary_model <- summary(model_results[[i]])
  
  # Print the model's path coefficients, R² values, and reliability metrics
  print(summary_model$paths)
  print(summary_model$R2)
  print(summary_model$reliability)
  print(summary_model$loadings)
  print(summary_model$validity$htmt)
  print(summary_model$validity$vif_items)
  print(summary_model$it_criteria)
  
  cat("\n") # Add a blank line to separate results of different models
}


# Initialize a list to store the results of the bootstrap
bootstrap_results_list <- list()

# Perform bootstrap for each PLS model
for (i in 1:length(model_results)) {
  # Execute the bootstrap
  boot_result <- bootstrap_model(
    seminr_model = model_results[[i]],
    nboot = 1000,
    cores = parallel::detectCores(),
    seed = 123
  )
  
  # Store the results
  bootstrap_results_list[[i]] <- boot_result
}

# Iterate through each bootstrap result and print the summary
for (i in 1:length(bootstrap_results_list)) {
  cat("Bootstrap Summary for Model", i, ":\n")
  
  # Summarize the results
  summary_boot <- summary(bootstrap_results_list[[i]], alpha = 0.05)
  
  # Print the summary
  print(summary_boot)
  cat("\n") # Separate results of different models for clarity
}


# Initialize a list to store prediction results
prediction_results_list <- list()

# Predict for each model from the imputed datasets
for (i in 1:length(model_results)) {
  # Perform prediction using predict_pls
  predict_results <- predict_pls(
    model = model_results[[i]],
    technique = predict_DA,
    noFolds = 10,
    reps = 10
  )
  
  # Store the prediction results
  prediction_results_list[[i]] <- predict_results
}

# Iterate and print prediction results for each model
for (i in 1:length(prediction_results_list)) {
  cat("Model", i, "Prediction Results:\n")
  sum_predict_results <- summary(prediction_results_list[[i]])
  
  # Print prediction statistics
  print(sum_predict_results)
  
  cat("\n") # Add a blank line to separate results of different models
}














