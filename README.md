# Boston Housing Price Prediction (Elastic Net Regression)

## 📌 Project Overview
This project analyzes the Boston Housing dataset to predict median home values. It uses **Elastic Net Regression**, a technique that combines the benefits of Ridge and Lasso regression to handle correlated predictors and perform feature selection.

## 🛠️ Technologies Used
* **Language:** R
* **Libraries:** `tidyverse`, `caret`, `glmnet`, `corrplot`
* **Model:** Elastic Net (Grid Search for optimal $\alpha$ and $\lambda$)

## 📊 Key Steps
1.  **Exploratory Data Analysis (EDA):** Analyzed distributions of variables like Crime Rate (`CRIM`) and Room Count (`RM`).
2.  **Data Preprocessing:** * Imputed missing values (median for numeric, mode for categorical).
    * Engineered features (e.g., `log_CRIM` to handle skewness, `RM_sq` for non-linearity).
3.  **Modeling:**
    * Implemented a 10-fold cross-validation grid search.
    * Optimized for Minimum Mean Squared Error (MSE).
4.  **Evaluation:** Achieved a high $R^2$ on the test set, identifying `LSTAT` (neighborhood status) and `RM` (room count) as the primary drivers of price.

## 🚀 How to Run
1.  Clone this repository.
2.  Ensure `HousingData.csv` is in your working directory.
3.  Open `final_code.R` in RStudio.
4.  Run the script to see the EDA plots and model output.
