# Data Preparation

## Overview
This repository contains a data preparation script for the Home Credit default risk dataset.  
The goal of this assignment is to translate exploratory data analysis (EDA) findings into reusable and consistent data cleaning and feature engineering functions that can be applied to both training and test data.

## What the script does
The data preparation script performs the following steps:

- Fixes known data issues identified during EDA (e.g., the `DAYS_EMPLOYED = 365243` placeholder value)
- Handles missing values in important variables such as the EXT_SOURCE features
- Creates basic engineered features including financial ratios and demographic variables
- Adds missing-value indicator variables
- Aggregates supplementary datasets (e.g., bureau data) to the application level
- Ensures that identical transformations are applied to both training and test datasets to avoid data leakage

## How to run the script
1. Place the required CSV files (e.g., `application_train.csv`, `application_test.csv`, and supplementary datasets) in your working directory.
2. Open the script in R.
3. Source the script or run it section by section.
4. Use the provided functions to prepare training and test data consistently.

## Inputs
- application_train.csv  
- application_test.csv  
- bureau.csv (optional supplementary data)

## Outputs
- Cleaned and feature-engineered training dataset
- Cleaned and feature-engineered test dataset with identical structure (except TARGET)

## AI usage
AI tools were used to help outline the data preparation workflow and generate draft code structures.  
All code was reviewed, modified, and tested in R, and final decisions on data cleaning, feature engineering, and interpretation were made by the student based on EDA results and course concepts.
