# Predictive Modeling in Sports Betting

## Introduction
In this research, we concentrate on predicting soccer match outcomes and evaluating relationships between performance metrics and game results by utilizing a comprehensive dataset from Football-Data (https://www.football-data.co.uk/data.php). It contains basic information about the match and pre-match betting odds. The dataset consists of 20,901 games with 21 variables.

Our objectives include developing a predictive model that integrates a variety of data points to forecast match outcomes, identifying the most significant factors determining match results, and comparing the effectiveness of different statistical and machine learning methods in sports outcome predictions.

## Contents
Predictive-Modeling-in-Sports-Betting/  
1. src/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Source code
- eda_soccerdata.R&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Exploratory data analysis
- final_code.R&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Main code
- get_Football_Data.ipynb&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Get data from Football Data (main)
- get_Soccer_API_data.ipynb&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Get data from Soccer API
- soccer_models.R&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#Code for first report
2. data/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Static files
- soccerdata.csv&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Final CSV file for the models
- Soccer Data/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Folder of CSV files for each league season (input for get_Football_Data.ipynb)
3. docs/
- Presentation.pdf
- Report.pdf
4. README.md

## Basic Usage
1. Run the get_Football_Data.ipynb file to get the data
2. Run the eda_soccerdata.R to perform exploratory data analysis on the dataset
3. Run final_code.R to get the output of the models

## Output
The code produces these statistics in its output:
1. Logistic Regression for the likelihood of a home win (0 = Win, 1 = Lose)
2. Random Forest for the likelihood of a home win
3. Simplified Logistic Regression for the likelihood of a home win
4. Accuracy, AUC-ROC, precision, recall, F1 score, and CV accuracy for each model

## Contributor
Yousef Lari 
