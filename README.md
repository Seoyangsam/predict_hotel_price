# ML22-TEAM12

<<<<<<< HEAD
This is the repository for our group assignment regarding the Machine Learning course. The goal of this assignment is to train different Machine Learning regression models to predict the average daily hotel room rate in function of several indicators and to obtain the optimal model. 
=======
This is the repository for our group assignment of the Machine Learning course provided by [Gent univeristy](https://www.ugent.be/en). The task is to finish hotel room price prediction on [Kaggle competition](https://www.kaggle.com/competitions/hotel-price-prediction-ugent-ml-2022). The goal of this assignment is to benchmark different Machine Learning regression models that predict average daily hotel room rates based on a range of indicators. 
>>>>>>> 89a8cac1968ec303fb5bd9db7155c42bbe8d2b59

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

Besides our R code, we also provided a technical report. 
