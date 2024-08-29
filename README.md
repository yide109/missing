# missing
This folder contains three R scripts, each implementing different methods for handling missing data in statistical models, particularly focusing on Partial Least Squares Structural Equation Modeling (PLS-SEM):

FSC MAR.R

This script demonstrates the Fixed Scenario Completion (FSC) method specifically tailored for data missing at random (MAR). It includes steps for data preparation, missing data simulation based on MAR assumptions, and model estimation.
EM MAR0707.R

Implements the Expectation Maximization (EM) algorithm for data assumed to be missing at random (MAR). The script guides through the process of imputation, model estimation, and results interpretation within the context of MAR.
EM with a weighting method0707.R

Combines the EM algorithm with a weighting method to address non-random missing data (NMAR). This script provides a detailed implementation of the EM algorithm, supplemented by a weighting mechanism to enhance the analysis robustness against NMAR conditions.
General Instructions:

Ensure that all data paths in the scripts are correctly set to match the location of your datasets.
Required R packages include readxl, mice, seminr, and others as specified within each script. Install these packages using install.packages("packageName") if not already installed.
Run each script in an R environment, either through RStudio or a similar interface, to execute the analysis described.
For detailed explanation of each method and step-by-step guidance, refer to the comments and documentation within each script file.
