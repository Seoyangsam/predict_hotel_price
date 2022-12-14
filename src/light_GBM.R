# light GBM

# packages
install.packages('gbm')
install.packages('caret')
install.packages('lightgbm')
install.packages('Metrics')

library(gbm)
library(caret)
library(lightgbm)
library(Metrics)

# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# Convert all columns to factor
train_X <- as.data.frame(unclass(train_X), stringsAsFactors = TRUE)
train_y <- as.data.frame(unclass(train_y), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE,levels = levels(train_X_data))

# hyperparameter tuning
params <- list(
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2"
)

grid <- list(
    learning_rate = seq(0.01, 0.1, 0.3),
    num_leaves = seq(50, 110, 150),
    max_depth = seq(6, 8, 10)
)

results <- lgb.cv(params= params, data= train_X, label= train_y, nrounds = 100, nfold= 5, grid= grid, verboseIter = TRUE)

# optimal values for parameters
optimal_learning_rate <- results$learning_rate
optimal_num_leaves <- results$num_leaves
optimal_max_dept <- results$max_depth 

# Train the LightGBM model using the training data and optimal parameters
params_optimal <- list(
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2",
  learning_rate = optimal_learning_rate,
  num_leaves = optimal_num_leaves,
  max_depth = optimal_max_depth
)

lgb <- lgb.train(params = params_optimal, data = train_x, label = train_y, verboseIter = TRUE)

# Use the trained model to make predictions on the test data
predictions <- lgb.predict(lgb, validation_X)

# Evaluate the predictions using an appropriate metric
LGBM_RMSE <- sqrt(mean((predictions - validation_y$average_daily_rate)^2))
LGBM_MAE <- mae(validation_y$average_daily_rate, predictions)
summary(predictions)