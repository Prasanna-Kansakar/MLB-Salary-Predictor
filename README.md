# README

## Overview
This R program performs data cleaning, merging, analysis, and machine learning tasks on baseball-related datasets. The program integrates player, batting, salary, fielding, and team data to analyze player performance, salary trends, and team dynamics. It also includes predictive modeling and clustering.

## Features
1. **Data Cleaning and Merging**
   - Combines multiple datasets such as player, batting, salary, fielding, and team data.
   - Handles missing values and removes unnecessary records (e.g., pitchers).

2. **Data Analysis**
   - **Trends in Player Performance:** Analyzes metrics like runs, RBIs, and home runs over time.
   - **Salary Trends:** Examines player salary changes over years.
   - **Batting Hand Impact:** Studies the influence of batting hand on performance and salary.
   - **Fielding and Age Distribution:** Visualizes player distribution based on fielding positions and age.
   - **Batting Average by Position and Age:** Calculates and visualizes batting averages across positions and ages.
   - **Player Size Evolution:** Tracks changes in player height and weight over time.
   - **Performance vs Size:** Explores how player size impacts performance metrics like home runs and stolen bases.
   - **Salary vs Size:** Analyzes the relationship between player size and salary.
   - **Salary and Winning Correlation:** Investigates the impact of team spending on winning.
   - **Best Hitters’ Impact:** Assesses if teams with better hitters achieve higher win rates.

3. **Machine Learning**
   - **Salary Prediction:** Predicts player salary using hitting statistics and XGBoost regression.
   - **Playoff Prediction:** Predicts whether a team will make the playoffs using logistic regression.
   - **Player Clustering:** Groups players based on career statistics using k-means clustering.

## Prerequisites
- **Required R Packages:**
  - `dplyr`
  - `ggplot2`
  - `gridExtra`
  - `corrplot`
  - `caret`
  - `xgboost`
  - `factoextra`
  - `tibble`
  - `cluster`

- **Data Files:**
  - `Data/player.csv`
  - `Data/batting.csv`
  - `Data/fielding.csv`
  - `Data/salary.csv`
  - `Data/team.csv`

## Instructions
1. **Install Required Packages:**
   ```R
   install.packages(c("dplyr", "ggplot2", "gridExtra", "corrplot", "caret", "xgboost", "factoextra", "tibble", "cluster"))
   ```

2. **Set Up File Structure:**
   Place all required CSV files in a folder named `Data` relative to the program directory.

3. **Run the Script:**
   Execute the script to perform data cleaning, analysis, and machine learning tasks. Outputs include visualizations, metrics, and predictions.

## Outputs
- **Plots:** Various visualizations, including line graphs, scatter plots, bar graphs, pie charts, and heatmaps.
- **Summary Tables:** Data summaries, correlation matrices, and feature importances.
- **Machine Learning Results:** Predictions for player salary and playoff qualification, along with clustering results.

## Key Analyses
1. **Performance Metrics Over Time:**
   - Visualizes trends in player metrics like runs and RBIs.
2. **Impact of Batting Hand:**
   - Explores performance differences based on batting hand.
3. **Cost-Effective Players:**
   - Identifies players with high performance and low salary.
4. **Winning Correlation:**
   - Examines if spending or best hitters correlate with winning.
5. **Clustering:**
   - Groups players into clusters based on performance.

## Machine Learning Models
1. **Salary Prediction:**
   - Uses XGBoost regression with feature importance ranking.
2. **Playoff Prediction:**
   - Logistic regression to predict playoff qualification.
3. **Player Clustering:**
   - K-means clustering to group players based on performance metrics.

## Performance Metrics
1. **XGBoost Regression:**
   - R-squared, RMSE, and MAE are calculated to evaluate model accuracy.
2. **Logistic Regression:**
   - Confusion matrix used to assess model performance.
3. **Clustering:**
   - Elbow method determines optimal clusters; visualization highlights clusters.

