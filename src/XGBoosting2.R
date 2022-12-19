# read data: we will start from the data cleaning and do the feature engineering here
train_X_cleaned <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X_cleaned <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_X_cleaned <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# packages
library(gbm)
library(caret)
library(lightgbm)
library(Metrics)


# create 1 dataframe
train_X_data <- data.frame(train_X_scale,train_y)
validation_X_data <- data.frame(validation_X_scale,validation_y)

#train_X_data <- data.frame(train_X_scale,train_y)
#validation_X_data <- data.frame(validation_X_scale,validation_y)

#train_X_data <- as.matrix(train_X_data)
#validation_X_data <- as.matrix(validation_X_data) 
#train_X_scale <- as.matrix(train_X_scale)
#validation_X_scale <- as.matrix(validation_X_scale) 
#train_y <- as.matrix(train_y)
#validation_y <- as.matrix(validation_y)

train_X_scale <- as.numeric(train_X_scale)
validation_X_scale <- as.numeric(validation_X_scale)
train_y <- as.numeric(train_y)

train_X_scale <- as.matrix(train_X_scale)
validation_X_scale <- as.matrix(validation_X_scale) 
train_y <- as.matrix(train_y)

# Convert the data to an xgb.DMatrix object
dtrain <- xgb.DMatrix(data = train_X_scale, label = train_y)
dvalid <- xgb.DMatrix(data = validation_X_scale, label = validation_y)

# Set the xgboost parameters
param <- list(booster = "gbtree", objective = "reg:linear", eta = 0.2, max_depth = 12)

# Fit the model
xgboost_model <- xgboost(params = param, data = dtrain, nrounds = 120)

# Predict on the test set
predictions <- predict(xgboost_model, validation_X_scale)

rf_error <- sqrt(mean((predictions - validation_y)^2))
mae <- mean(abs(predictions - validation_y))

#data_frame <- data.frame(rf_error = rf_error,
  #mae = mae
#)
write.table(rf_error, file = "data/results/XGBoost_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(mae, file = "data/results/XGBoost_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# retrain 
train_and_validation_X <- rbind(train_X_scale, validation_X_scale)
dependent_y <- rbind(train_y, validation_y)

dtrain_retrain <- xgb.DMatrix(data = as.matrix(train_and_validation_X), label = dependant_y)


# Set the xgboost parameters
param <- list(booster = "gbtree", objective = "reg:linear", eta = 0.2, max_depth = 12)

# Fit the model
xgboost_model <- xgboost(params = param, data = dtrain_retrain, nrounds = 120)

# Predict on the test set
predictions <- predict(xgboost_model, test_X_scale)

# make file with id and corresponding average daily rate
xgboost_submission <- data.frame(col1 = test_id$x, col2 = predictions)

colnames(xgboost_submission) <- c("id", "average_daily_rate")
write.table(xgboost_submission, file = "data/results/XGBoost_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)





