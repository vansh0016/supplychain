# Supply Chain Analytics Using Statistical Methods

## Project Overview

This project applies statistical methods and machine learning techniques to analyze supply chain operations, specifically focusing on insurance cost prediction, shipment classification, and delivery outcome prediction. The study employs various statistical techniques, including linear regression, random forest, and decision trees, to extract insights from supply chain data and improve operational efficiency.

## Table of Contents

- [Introduction](#introduction)
- [Dataset](#dataset)
- [Data Cleaning](#data-cleaning)
- [Research Questions](#research-questions)
  - [Prediction of Insurance Costs](#prediction-of-insurance-costs)
  - [Shipment Classification](#shipment-classification)
  - [Delivery Outcome Prediction](#delivery-outcome-prediction)
- [Methodology](#methodology)
- [Results](#results)
- [Future Scope](#future-scope)
- [References](#references)

## Introduction

Efficient supply chain management is critical for reducing costs and improving operational effectiveness. This study explores factors influencing insurance costs in shipments, classifies shipments based on insurance cost categories, and predicts delivery outcomes using statistical models. The findings can assist businesses in making data-driven decisions for risk management and logistics optimization.

## Dataset

The dataset contains various attributes related to supply chain shipments, including:

- **Shipment Information:** Shipment mode (air, sea, or road), country, and delivery dates.
- **Cost Information:** Freight cost (USD), insurance cost (USD), and pack price.
- **Product Characteristics:** Product group, sub-classification, and dosage.
- **Operational Data:** Line item quantity, line item value, and weight.

## Data Cleaning

The dataset underwent extensive preprocessing, including:

- Conversion of non-numeric values in **freight cost** and **weight** columns to numeric format.
- Imputation of missing values using median-based methods to handle skewed distributions.
- Encoding categorical variables such as **shipment mode**, **country**, and **product group**.
- Removal of outliers using percentile-based filtering.
- Creation of new features, such as **delivery_time**, by calculating differences between scheduled and actual delivery dates.
- Computation of a **correlation matrix** to identify relationships between key variables.

## Research Questions

### Prediction of Insurance Costs

- **Objective:** Identify factors that contribute to insurance cost estimation.
- **Methods:**
  - **Linear Regression:** Used to analyze the impact of key predictors such as **line item value**, **freight cost**, and **shipment mode**.
  - **Random Forest:** Applied for enhanced accuracy in capturing non-linear relationships.
- **Findings:**
  - **Line item value** had the strongest correlation with insurance costs.
  - **Random Forest (R² = 94.47%)** outperformed Linear Regression (R² = 92.64%) in predictive accuracy.

### Shipment Classification

- **Objective:** Categorize shipments into **low, medium, and high** insurance cost categories.
- **Methods:**
  - **Decision Tree:** Generated classification rules based on shipment attributes.
  - **Random Forest:** Improved classification performance using ensemble learning.
- **Findings:**
  - **Decision Tree Accuracy:** 93.87%
  - **Random Forest Accuracy:** 94.65%
  - Both models effectively classified shipments with **high precision and reliability**.

### Delivery Outcome Prediction

- **Objective:** Predict whether shipments are delivered **early, on-time, or late**.
- **Methods:**
  - **Logistic Regression:** Used to determine key factors influencing delivery performance.
  - **Decision Tree & Random Forest:** Employed for improved classification accuracy.
- **Findings:**
  - **Key predictors:** Pack price, delivery time, and line item value significantly impacted delivery outcomes.
  - **Model accuracy:** 74.10%, highlighting potential areas for improvement.

## Methodology

1. **Feature Engineering:** Created new features such as **delivery time**, categorized insurance levels, and encoded categorical variables.
2. **Exploratory Data Analysis (EDA):** Used **correlation matrices** and **visualizations** to identify key trends.
3. **Model Selection:** Compared multiple models (Linear Regression, Random Forest, Decision Tree, Logistic Regression) to evaluate predictive power.
4. **Evaluation Metrics:** Used **R², RMSE, Accuracy, and Confusion Matrices** to assess model performance.

## Results

- **Insurance Cost Prediction:** Random Forest model provided the best accuracy for estimating insurance costs.
- **Shipment Classification:** Decision Tree and Random Forest models successfully categorized shipments into cost-based groups.
- **Delivery Outcome Prediction:** Identified key factors affecting shipment delivery times, achieving 74.10% accuracy.

## Future Scope

- **Improve Prediction Models:** Incorporate deep learning techniques (e.g., LSTM, Transformer models) to enhance prediction accuracy.
- **Expand Feature Engineering:** Include external variables such as **weather conditions and economic factors** affecting delivery timelines.
- **Real-Time Analytics:** Develop a dashboard to provide live insights into shipment trends and cost variations.
- **Optimization Strategies:** Use machine learning for real-time decision-making in supply chain logistics.

## References

A comprehensive list of research papers and data sources supporting the study.
