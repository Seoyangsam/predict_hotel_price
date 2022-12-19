# ML22-TEAM12

This is the repository for our group assignment regarding the Machine Learning course. The goal of this assignment is to train different Machine Learning regression models to predict the average daily hotel room rate in function of several indicators and to obtain the optimal model. 

# Structure 

We stored our data in the data folder as structure below: 
- bronze:
original dataset 

- silver:
dataet after cleaning 

- gold:
dataset after cleaning and feature engineering 

- results:
results of diffirent models<br> 

In src, we have our R code, as below:

1. data preprocessing:

- data_exploration

- data_cleaning

- feature_engineering

2. models:

- support_vector_regression

- regularized_polynomial_model

- reg_linear_model_ridge

- reg_linear_model_lasso

- random forest

- polynomial_model

- light_GBM

- generalized_additive_models

- deep_learning

- XGBoost

- GBM

- LGBM

# What we did:

1. Data preprocessing 

2. Create the linear baseline 

3. Build other models with hyperparameter tuning 

4. Select best model and retrain the model on train+validation set  

5. Submissions on [Kaggle](https://www.kaggle.com/competitions/hotel-price-prediction-ugent-ml-2022)

Besides our R code, we also provided a technical report. 

# What we fund:
Below is the perfermance comparision of different models on validation set. We used RMSE(root of mean squared error) and MAE(mean abusolute error) as metrics. 

| Model                         | RMSE     | MAE      |
|-------------------------------|----------|----------|
| Linear model (Ridge)          | 46.9450739620167| 24.0875589083806|
| Linear model (Lasso)          | 46.9452004237641| 24.0878229405589|
| Polynomial model              | 47.8232444073489| 24.2590743626267|
| Polynomial model(Ridge)       | 47.8232444073485| 24.2590743626255|
| Polynomial model(Lasso)       | 47.8232444073485| 24.2590743626255|
| Smoothing Spline model (Lasso)| 46.9450187663735| 24.0883525771904|
| Local Regression model (Lasso)| 46.9450187663735| 24.0883525771905|
| Random Forest                 | 41.7082616838757| 16.9499372629923|
| Support Vector Regression     | 41.3513970085533| 15.5208363929206|
| Gradient Boosting Machine     | 40.0568441506894| 14.7029830342072|
| Light GBM                     | 39.7322809704038| 13.4160055318152|
| XGBoost.                      | 39.7322809704038| 13.4160055318152|
| Deep Learning                 | 40.3360880601974| 14.3416000000000|

Based on this table, we decided that light GBM and XGBoost model were the best fit for our data since these model’s errors were the lowest.  

# Authors 
Hanne Van Impe<br>
Sien Van Herreweghe<br> 
Ruben Mertens<br>
Robert Xu 
