# Missing Data Handling in PLS-SEM

This folder contains three R scripts, each demonstrating a different approach to handling missing data in Partial Least Squares Structural Equation Modeling (PLS-SEM). All data paths, assumptions, and usage instructions are documented within each script.

## Data Sources

The datasets used in this analysis are based on the Corporate Reputation Model from Hair et al. (2021, 2023, 2024). They are categorized according to the type of missingness introduced:

- **mcar_corporatereputation**
  Data with Missing Completely at Random (MCAR) missingness.
- **mar_corporatereputation**
  Data with Missing at Random (MAR) missingness.
- **nmar_corporatereputation**
  Data with Not Missing at Random (NMAR) missingness.

## R Scripts

### 1. `EMforMAR.R`

- **Description**: Implements the Expectation-Maximization (EM) algorithm for datasets assumed to have Missing at Random (MAR).

- Usage

  :

  - Set your working directory to the folder containing the script and data.
  - Install and load any required packages (e.g., `mice`, `seminr`).
  - Run the script to perform EM-based imputation and subsequent PLS-SEM analysis.

### 2. `FCSforMAR.R`

- **Description**: Demonstrates the Fully Conditional Specification (FCS) method for data assumed to be Missing at Random (MAR).

- Usage

  :

  - Update file paths and parameters to match your local setup.
  - Execute the script to generate imputations under the FCS framework and conduct PLS-SEM.

### 3. `EM_with_weighting_methodforNMAR.R`

- **Description**: Combines EM imputation with a weighting method to address Not Missing at Random (NMAR) conditions.

- Usage

  :

  - Adjust any necessary file paths and parameters.
  - Run the script to apply the EM-weighting approach and perform PLS-SEM on the completed dataset.

## General Instructions

### Data Preparation

- Verify that your data are in the correct format (e.g., CSV, Excel) and that variable names match those referenced in the R scripts.

### Software Requirements

- Open each script in R or RStudio.
- Modify file paths, data inputs, and analysis parameters as needed.
- Run the script. Outputs (e.g., imputed datasets, model summaries) will be generated as specified within the code.

### Further Analysis

- Refer to in-script comments and documentation for guidance on adjusting the imputation settings.
- Compare results across methods to evaluate performance under MCAR, MAR, or NMAR assumptions.

## References

- Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2023).
  *A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM) (3rd ed.).* CA: Sage.
- Hair, J.F., Hult, G.T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., & Ray, S. (2021).
  *The SEMinR Package.* In *Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R.* Classroom Companion: Business. Springer, Cham.
- Hair, J. F., Sarstedt, M., Ringle, C. M., & Gudergan, S. P. (2024).
  *Advanced Issues in Partial Least Squares Structural Equation Modelling (PLS-SEM) (2nd ed.).* Sage.