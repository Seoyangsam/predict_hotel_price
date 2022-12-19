# ML22-TEAM12

This is the repository for our group assignment of the Machine Learning course provided by [Gent univeristy](https://www.ugent.be/en). The task is to finish hotel room price prediction on [Kaggle competition](https://www.kaggle.com/competitions/hotel-price-prediction-ugent-ml-2022). The goal of this assignment is to benchmark different Machine Learning regression models that predict average daily hotel room rates based on a range of indicators. 

# Structure 

We stored our data in the data folder as structure below: 
- bronze:
original dataset 

- silver:
dataet after cleaning 

- gold:
dataset after cleaning and feature engineering 

- results:
results of diffirent models 

In src, we have our R code, as below:

- data preprocessing:

data_exploration

data_cleaning

feature_engineering

- models:

support_vector_regression

regularized_polynomial_model

reg_linear_model_ridge

reg_linear_model_lasso

random forest

polynomial_model

light_GBM

generalized_additive_models

deep_learning

XGBoost

GBM

LGBM

# What we did:

1.Data preprocessing 

2.Create the linear baseline 

3.Build other models with hyperparameter tuning 

4.Select best model and retrain the model 

5.Submission on Kaggle

Except the R code, we also have the technical report. 


# What we fund:

We used RMSE(root of mean squared error) and MAE(mean abusolute error) as metrics. 

| Model                         | RMSE     | MAE      |
|-------------------------------|----------|----------|
| Linear model (Ridge)          | 45.9838087301083| 23.0158474776116|
| Linear model (Lasso)          | 45.984102829997 | 23.0158781113198|
| Polynomial model              | 46.0840812969587| 23.0672143396751|
| Polynomial model(Ridge)       | 46.0840812969586| 23.0672143396752|
| Polynomial model(Lasso)       | Cell 8   | Cell 9   |
| Smoothing Spline model (Lasso)| 45.9833172300957    | 23.0179371917265   |
| Local Regression model (Lasso)| 45.9833172300958    | 23.0179371917263    |
| Random Forest                 | 41.70826            | 16.94994    |
| Support Vector Regression     | 41.3513970085533    | 15.5208363929206    |
| Gradient Boosting Machine     | 40.0568441506894    | 14.7029830342072   |
| Light GBM                     | 39.7645296838941    | 13.3646612615321    |
| Deep Learning                 | 40.3360880601974    | 14.3416    |

Based on this table, we decided that the SVR, GBM and light GBM model were the best fit for our data since these model’s errors were the lowest.  This however does not mean these models were the easiest to work with if we take a look at the computing time. Like already said, both the SVR and GBM model took up a great amount of time when they were ran. So although the performances of the 3 last mentioned models are quite similar, we would still opt for light GBM since this model could be ran very easily and quickly.  
