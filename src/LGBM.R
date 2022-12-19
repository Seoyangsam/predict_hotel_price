# read data: we will start from the data cleaning and do the feature engineering here
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X_cleaned <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# power transform dependent variable to make it more left skewed (<1)
train_y <- sqrt(train_y)
colMeans(is.na(train_y))

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


train_X_data <- data.frame(train_X_scale,train_y)

validation_X_data <- data.frame(validation_X_scale,validation_y)

train_X_data <- as.matrix(train_X_data)
#validation_X_data <- as.matrix(validation_X_data) 
train_X_scale <- as.matrix(train_X_scale) 
validation_X_scale <- as.matrix(validation_X_scale) 
train_y <- as.matrix(train_y)
#validation_y <- as.matrix(validation_y)

dtrain <- lgb.Dataset(data = train_X_scale, label = train_y)
dtrain <- lgb.Dataset.construct(data = dtrain)

# Hyperparameter tuning for learning rate, num_leaves, max_depth, lambda_l1 and nrounds
# Train the LightGBM model using the training data and optimal parameters
params_optimal <- list(
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2",
  learning_rate = 0.15,
  num_leaves = 80,
  max_depth = 10,
  lambda_l1 = 0.01)

lgb <- lgb.train(params = params_optimal, data = dtrain, nrounds = 800, task = "regression")

# Use the trained model to make predictions on the test data
predictions <- predict(lgb, validation_X_scale)
predictions <- predictions^2

# Evaluate the predictions using an appropriate metric
LGBM_RMSE <- sqrt(mean((predictions - validation_y$average_daily_rate)^2))
LGBM_MAE <- mae(validation_y$average_daily_rate, predictions)
summary(predictions)

write.table(LGBM_RMSE, file = "data/results/LGBM_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(LGBM_MAE, file = "data/results/LGBM_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# step 2: retrain on val+train set and predict on test set

#First we read our datas
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned_retrain.csv', header = TRUE)
str(train_X_cleaned)

test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned_retrain.csv', header = TRUE)
str(test_X_cleaned)

train_y_retrain <- read.csv(file = 'data/gold/train_y_retrain.csv', header = TRUE)


train_X_ft_engineering <- train_X_cleaned
test_X_ft_engineering <- test_X_cleaned

union(unique(train_X_ft_engineering$meal_booked), unique(test_X_ft_engineering$meal_booked))
meal_booked_levels <- c("meal package NOT booked", "bed & breakfast (BB)", "breakfast + one other meal // usually dinner (half board)", "full board [BREAKF -- lunch -- Dinner]") # in correct order!
train_X_ft_engineering$meal_booked <- as.numeric(factor(train_X_ft_engineering$meal_booked, levels = meal_booked_levels))
test_X_ft_engineering$meal_booked <- as.numeric(factor(test_X_ft_engineering$meal_booked, levels = meal_booked_levels))


library(dummy)
# get categories and dummies
cats <- categories(train_X_ft_engineering[, c("assigned_room_type","customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")], p=15)
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_train <- subset(dummies_train, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, month_arrival_date_January, day_arrival_date_monday))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, month_arrival_date_January, day_arrival_date_monday))

## merge with overall training set
train_X_ft_engineering <- subset(train_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, month_arrival_date, day_arrival_date))
train_X_ft_engineering <- cbind(train_X_ft_engineering, dummies_train)
## merge with overall test set
test_X_ft_engineering <- subset(test_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, month_arrival_date, day_arrival_date))
test_X_ft_engineering <- cbind(test_X_ft_engineering, dummies_test)


#convert the predictors to factors
#train_X_ft_engineering[sapply(train_X_ft_engineering, is.character)] <- lapply(train_X_ft_engineering[sapply(train_X_ft_engineering, is.character)], as.factor)
#str(train_X_ft_engineering)
#test_X_ft_engineering[sapply(test_X_ft_engineering, is.character)] <- lapply(test_X_ft_engineering[sapply(test_X_ft_engineering, is.character)], as.factor)
#str(test_X_ft_engineering)
#validation_X_ft_engineering[sapply(validation_X_ft_engineering, is.character)] <- lapply(validation_X_ft_engineering[sapply(validation_X_ft_engineering, is.character)], as.factor)
#str(validation_X_ft_engineering)

# create new dataframes to avoid overwriting the existing dataframes
train_X_scale <- train_X_ft_engineering
test_X_scale <- test_X_ft_engineering

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

# apply on training set
mean_train <- colMeans(train_X_scale[, scale_cols])
sd_train <- sapply(train_X_scale[, scale_cols], sd)
train_X_scale[, scale_cols] <- scale(train_X_scale[, scale_cols], center = TRUE, scale = TRUE)

# apply on test set
test_X_scale[, scale_cols] <- scale(test_X_scale[, scale_cols], center = mean_train, scale = sd_train)

# now, we check the distributions
colMeans(train_X_scale[, scale_cols])
sapply(train_X_scale[, scale_cols], sd)

colMeans(test_X_scale[, scale_cols])
sapply(test_X_scale[, scale_cols], sd)

train_X_scale <- as.matrix(train_X_scale)
train_y_retrain <- as.matrix(train_y_retrain)

dtrain_retrain <- lgb.Dataset(data = train_X_scale, label = train_y_retrain)
dtrain_retrain <- lgb.Dataset.construct(data = dtrain_retrain)

test_X_scale <- as.matrix(test_X_scale)

# lgbm model
params_optimal <- list(
  tree_learner = "serial",
  task = "train",
  boosting_type = "gbdt",
  objective = "regression",
  metrics = "l2",
  learning_rate = 0.15,
  num_leaves = 80,
  max_depth = 12,
  lambda_l1 = 0.01
)

lgb_final_model <- lgb.train(params = params_optimal, data = dtrain_retrain, nrounds = 800, task = "regression")


pred_test_set <- predict(lgb_final_model, test_X_scale)

# make file with id and corresponding average daily rate
lgbm_submission <- data.frame(col1 = test_id$x, col2 = pred_test_set)

colnames(lgbm_submission) <- c("id", "average_daily_rate")
write.table(lgbm_submission, file = "data/results/LGBM_submission_2.csv", sep = ",", row.names = FALSE, col.names=TRUE)
