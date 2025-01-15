# 🏡 House Price Prediction Using Linear Regression

![House Price Prediction](https://github.com/SYEDFAIZAN1987/House-Price-Prediction/blob/main/pic%203.jpg)

## 📘 About the Project

This project leverages **Linear Regression** to predict house prices based on the **Ames Housing Dataset**, which provides detailed information on housing sales in Ames, Iowa. The analysis focuses on identifying significant predictors of house prices and refining the regression model to improve predictive accuracy.

---

## 🔑 Key Features

1. **Data Preprocessing**:
   - Addressed missing values using advanced imputation techniques like **MICE**.
   - Focused on key continuous variables such as **Gr.Liv.Area**, **Total.Bsmt.SF**, **Garage.Area**, etc.
   - Handled outliers through visual diagnostics and statistical tests.

2. **Regression Modeling**:
   - Built and refined multiple linear regression models.
   - Included **interaction terms** and performed **all subsets regression** for model selection.
   - Addressed multicollinearity using **Variance Inflation Factors (VIF)**.

3. **Model Evaluation**:
   - Evaluated models using **Adjusted R-Squared**, **AIC**, **BIC**, and **F-statistics**.
   - Diagnostic plots such as **Residuals vs. Fitted**, **Scale-Location**, and **Q-Q Plots**.

4. **Business Impact**:
   - Insights into factors influencing house prices, aiding real estate professionals in pricing strategies.

---

## 📊 Key Insights

- **Significant Predictors**:
  - **Gr.Liv.Area** (Above Ground Living Area): The strongest positive predictor of house price.
  - **Total.Bsmt.SF** (Basement Area): A significant contributor to house value.
  - **Garage.Area**: Highly valued by buyers, as reflected in its positive coefficient.

- **Best Model**:
  - A three-variable interaction model with the highest **Adjusted R-Squared (76.73%)**.
  - **Equation**:
    \[
    SalePrice = 69,940 + 0.0628 \times (Gr.Liv.Area \times Garage.Area) + 0.1515 \times (Garage.Area \times Total.Bsmt.SF) + 0.3489 \times (Gr.Liv.Area \times Total.Bsmt.SF)
    \]

- **Model Comparison**:
  - The interaction model outperformed simpler models with lower AIC/BIC scores and better predictive power.

---

## 📜 Full Report

For a detailed analysis, including methodology, visualizations, and results, refer to the complete project report:  
[📄 House Price Prediction Report](https://github.com/SYEDFAIZAN1987/House-Price-Prediction/blob/main/House%20Price%20Prediction.pdf)

---

## 📂 Project Structure

```
.
├── Data/
│   ├── AmesHousing.csv
├── Scripts/
│   ├── House_Price_Prediction.R
├── Reports/
│   ├── House_Price_Prediction.pdf
├── README.md
```
## 🤝 Connect with Me

Feel free to reach out for feedback, questions, or collaboration opportunities:  
**LinkedIn**: [Dr. Syed Faizan](https://www.linkedin.com/in/drsyedfaizanmd/)

---

**Author**: Syed Faizan  
**Master’s Student in Data Analytics and Machine Learning**
