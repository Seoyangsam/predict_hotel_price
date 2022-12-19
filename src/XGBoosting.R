install.packages("xgboost")
library(xgboost)

# read data: we will start from the data cleaning and do the feature engineering here
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X_cleaned <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# packages
library(gbm)
library(caret)
library(lightgbm)
library(Metrics)

# integer encoding for meal booked

train_X_ft_engineering <- train_X_cleaned
test_X_ft_engineering <- test_X_cleaned
validation_X_ft_engineering <- validation_X_cleaned

union(unique(train_X_ft_engineering$meal_booked), unique(test_X_ft_engineering$meal_booked))
meal_booked_levels <- c("meal package NOT booked", "bed & breakfast (BB)", "breakfast + one other meal // usually dinner (half board)", "full board [BREAKF -- lunch -- Dinner]") # in correct order!
train_X_ft_engineering$meal_booked <- as.numeric(factor(train_X_ft_engineering$meal_booked, levels = meal_booked_levels))
validation_X_ft_engineering$meal_booked <- as.numeric(factor(validation_X_ft_engineering$meal_booked, levels = meal_booked_levels))
test_X_ft_engineering$meal_booked <- as.numeric(factor(test_X_ft_engineering$meal_booked, levels = meal_booked_levels))

# dummy encoding
library(dummy)
# get categories and dummies
cats <- categories(train_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")], p=15)
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_train <- subset(dummies_train, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, day_arrival_date_monday, month_arrival_date_January))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X_ft_engineering[, c("assigned_room_type", "customer_type","last_status", "market_segment", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, month_arrival_date_January, day_arrival_date_monday))
# apply on validation set (exclude reference categories)
dummies_validation <- dummy(validation_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_validation <- subset(dummies_validation, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, month_arrival_date_January, day_arrival_date_monday))

## merge with overall training set
train_X_ft_engineering <- subset(train_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, month_arrival_date, day_arrival_date))
train_X_ft_engineering <- cbind(train_X_ft_engineering, dummies_train)
## merge with overall test set
test_X_ft_engineering <- subset(test_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, month_arrival_date, day_arrival_date))
test_X_ft_engineering <- cbind(test_X_ft_engineering, dummies_test)
## merge with overall validation set
validation_X_ft_engineering <- subset(validation_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, month_arrival_date, day_arrival_date))
validation_X_ft_engineering <- cbind(validation_X_ft_engineering, dummies_validation)


# create new dataframes to avoid overwriting the existing dataframes
train_X_scale <- train_X_ft_engineering
test_X_scale <- test_X_ft_engineering
validation_X_scale <- validation_X_ft_engineering

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

# apply on training set
mean_train <- colMeans(train_X_scale[, scale_cols])
sd_train <- sapply(train_X_scale[, scale_cols], sd)
train_X_scale[, scale_cols] <- scale(train_X_scale[, scale_cols], center = TRUE, scale = TRUE)

# apply on test set
test_X_scale[, scale_cols] <- scale(test_X_scale[, scale_cols], center = mean_train, scale = sd_train)

# apply on validation set
validation_X_scale[, scale_cols] <- scale(validation_X_scale[, scale_cols], center = mean_train, scale = sd_train)

# now, we check the distributions
colMeans(train_X_scale[, scale_cols])
sapply(train_X_scale[, scale_cols], sd)

colMeans(test_X_scale[, scale_cols])
sapply(test_X_scale[, scale_cols], sd)

colMeans(validation_X_scale[, scale_cols])
sapply(validation_X_scale[, scale_cols], sd)


#train_X_data <- data.frame(train_X_scale,train_y)
#validation_X_data <- data.frame(validation_X_scale,validation_y)

#train_X_scale <- as.numeric(train_X_scale)
train_X_scale[] <− lapply(train_X_scale,as.numeric)
#validation_X_scale <- as.numeric(validation_X_scale)
validation_X_scale[] <− lapply(validation_X_scale,as.numeric)
#train_y <- as.numeric(train_y)
train_y[] <− lapply(train_y,as.numeric)
validation_y[] <− lapply(validation_y,as.numeric)

#train_X_data <- as.matrix(train_X_data)
#validation_X_data <- as.matrix(validation_X_data) 
train_X_scale <- data.matrix(train_X_scale)
validation_X_scale <- data.matrix(validation_X_scale) 
train_y <- data.matrix(train_y)
validation_y <- data.matrix(validation_y)


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










































# Load the caret package
library(caret)

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(validation_X), stringsAsFactors = TRUE,levels = levels(train_X))

# Create the grid of values to search over
tuning_grid <- expand.grid(nrounds = 100,
                           max_depth = 3,
                           eta = 0.1,
                           gamma = 0.2,
                           colsample_bytree = 0.5,
                           min_child_weight = 1,
                           subsample = 0.5)

# Fit the model using 5-fold cross-validation
model <- train(average_daily_rate ~ ., data = train_X_data,
               method = "xgbTree",
               nrounds = 100,
               max_depth = 3,
               eta = 0.1,
               gamma = 0.2,
               colsample_bytree = 0.5,
               min_child_weight = 1,
               )

# Print the best hyperparameters found by the grid search
print(model)

# Use the trained model to make predictions on the test set
predictions <- predict(model, validation_X)

rf_error <- sqrt(mean((predictions - validation_y)^2))
rf_error

mae <- mean(abs(predictions - validation_y))
mae

#data_frame <- data.frame(rf_error = rf_error,
  #mae = mae
#)
write.table(rf_error, file = "data/results/XGBoost_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(mae, file = "data/results/XGBoost_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
