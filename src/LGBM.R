# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# packages
library(gbm)
library(caret)
library(lightgbm)
library(Metrics)

train_X_data <- data.frame(train_X,train_y)

validation_X_data <- data.frame(validation_X,validation_y)

train_X_data <- as.matrix(train_X_data)
validation_X_data <- as.matrix(validation_X_data) 
train_X <- as.matrix(train_X) 
validation_X <- as.matrix(validation_X) 
train_y <- as.matrix(train_y)
validation_y <- as.matrix(validation_y)

dtrain <- lgb.Dataset(data = train_X_data)
dtrain <- lgb.Dataset.construct(data = dtrain)

dvalid <- lgb.Dataset(data = validation_X_data)
dvalid <- lgb.Dataset.construct(data = dvalid)

dtrain_X <- lgb.Dataset(data = train_X)
dtrain_X <- lgb.Dataset.construct(data = dtrain_X)

dvalid_X <- lgb.Dataset(data = validation_X)
dvalid_X <- lgb.Dataset.construct(data = dvalid_X)

dtrain_y <- lgb.Dataset(data = train_y)
dtrain_y <- lgb.Dataset.construct(data = dtrain_y)

dvalid_y <- lgb.Dataset(data = validation_y)
dvalid_y <- lgb.Dataset.construct(data = dvalid_y)


# hyperparameter tuning
params <- list(
  boosting_type = "gbdt",
  objective = "regression",
  metric ="RMSE",
  learning_rate = c(0.01,0.1,0.3),
  num_leaves = c(50,100,150),
  max_depth = c(6,8,10),
  min_data_in_leaf = c(10,50,100),
  early_stopping_rounds = 10,
  force_col_wise = TRUE,
  verboseIter = TRUE
)

results <- lgb.cv(
  params= params, 
  data = dtrain_X,
  label = dtrain_y,
  stratified = TRUE,
  nrounds = 1000,
  nfold = 5
  )

# optimal values for parameters
optimal_learning_rate <- results$params$learning_rate

optimal_num_leaves <- results$params$num_leaves
optimal_max_depth <- results$params$max_depth 

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

lgb <- lgbm.train(params = params_optimal, data = dtrain, label = train_y, nrounds = 100, task = "regression")

# Use the trained model to make predictions on the test data
predictions <- lgbm.predict(lgb, validation_X)

# Evaluate the predictions using an appropriate metric
LGBM_RMSE <- sqrt(mean((predictions - validation_y$average_daily_rate)^2))
LGBM_MAE <- mae(validation_y$average_daily_rate, predictions)
summary(predictions)


# manier 2
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

params <- list(
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2",
  learning_rate = c(0.01, 0.1, 0.3),
  num_leaves = c(50, 100, 150),
  max_depth = c(6, 8, 10)
)

tuned_model <- train(
  average_daily_rate ~ ., 
  data = train_X_data,
  method = "LGBMModel",
  metric = "RMSE",
  trControl = ctrl,
  tuneGrid = params,
  verboseIter = TRUE,
  nrounds = 100
)

print(tuned_model$bestTune)

final_model <- LGBModel(
  average_daily_rate ~ ., 
  data = train_X_data,
  learning_rate = tuned_model$bestTune$learning_rate,
  num_leaves = tuned_model$bestTune$num_leaves,
  max_depth = tuned_model$bestTune$max_depth,
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2",
  verboseIter = TRUE,
  nrounds = 100
)

predictions <- lgb.predict(final_model, validation_X)

# Evaluate the predictions using an appropriate metric
LGBM_RMSE <- sqrt(mean((predictions - validation_y$average_daily_rate)^2))
LGBM_MAE <- mae(validation_y$average_daily_rate, predictions)
summary(predictions)

X <- as.matrix(train_X_data[,-1])
y <- as.matrix(train_X_data[,1])
X <- lgb.Dataset(data = X)
X <- lgb.Dataset.construct(data = X)
y <- lgb.Dataset(data = y)
y <- lgb.Dataset.construct(data = y)
