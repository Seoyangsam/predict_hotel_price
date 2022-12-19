# ML22-TEAM12

This is the repository for our group assignment of the Machine Learning course. The task is to finish hotel room price prediction on Kaggle competition. The goal of this assignment is to benchmark different Machine Learning regression models that predict average daily hotel room rates based on a range of indicators. 

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

Except the R code, we also have the technical report. 


# What we fund:
Looking at our results(as stored in results folder), we decided that the SVR, GBM and light GBM model were the best fit for our data since these model’s errors were the lowest.  This however does not mean these models were the easiest to work with if we take a look at the computing time. Like already said, both the SVR and GBM model took up a great amount of time when they were ran. So although the performances of the 3 last mentioned models are quite similar, we would still opt for light GBM since this model could be ran very easily and quickly.  
