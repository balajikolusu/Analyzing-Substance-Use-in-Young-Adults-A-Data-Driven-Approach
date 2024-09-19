
# Substance Use Patterns in Young Adults: A Data-Driven Investigation

## Overview

This project analyzes substance use patterns among young adults, focusing on identifying key predictors using L1 regularization techniques (LASSO) and tree-based methods (Random Forest). The findings aim to provide insights into the factors influencing substance use behaviors, which can inform targeted interventions and policy decisions.

## Project Structure

- **R Script**: Contains the code for data preprocessing, model training, and visualization (`substance use pattern code.R`).
- **Data**: CSV file (`data_math678-3.csv`) containing the dataset used for analysis.
- **Visualizations**: Includes various plots generated during the analysis, such as:
  - **Density Plots**: Visualizing the distribution of mental health variables like depression, anxiety, and loneliness.
  - **Boxplots**: Comparing the distribution of total drug use by employment status and school attendance.
  - **Model Plots**: Cross-validation plots for LASSO and Ridge regression models, and variable importance from the Random Forest model.

## Installation

To run the analysis, you will need R and the following packages:

```r
install.packages(c("dplyr", "ggplot2", "glmnet", "randomForest", "vip"))
```

## Usage

1. **Run the R Script**: Use the `substance use pattern code.R` file to load and preprocess the data, fit the models, and generate visualizations.
2. **Analyze the Results**: Review the plots and model outputs to understand the influence of various factors on substance use among young adults.

## Key Insights

- **Mental Health Correlations**: Density plots reveal patterns in how depression, anxiety, and loneliness scores relate to substance use.
- **Sociodemographic Factors**: Boxplots show the impact of employment and school attendance on total drug use.
- **Model Performance**: The LASSO and Ridge regression models identify the most significant predictors, while the Random Forest model highlights variable importance.

## Challenges

- **Data Preprocessing**: Handling missing data and encoding categorical variables were critical steps to ensure model accuracy.
- **Model Tuning**: Selecting the optimal lambda for LASSO and Ridge required careful cross-validation to minimize errors.

## Contributors

- Balaji Kolusu

## References

- [glmnet Documentation](https://glmnet.stanford.edu/)
- [Random Forest Overview](https://www.stat.berkeley.edu/~breiman/RandomForests/)


