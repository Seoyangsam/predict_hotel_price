# GBM
#install.packages('gbm')
library(gbm)
library(caret)
library(Metrics)
install.packages("Metrics")
# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE)
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

# create 1 dataframe
train_X_data <- data.frame(train_X_scale,train_y)
validation_X_data <- data.frame(validation_X_scale,validation_y)

# 1: tune parameters using cv k=5
cv_5 = trainControl(method = "cv", number = 5)

gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)
                    
set.seed(40)
gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)

print(gbm_tune)
# optimal parameters are: n.trees = 3000, interaction.depth = 5, shrinkage = 0.1
# since these are all the maximum values, we tune again to see if we can further improve

# 2: tune parameters using cv k=5
cv_5 = trainControl(method = "cv", number = 5, verboseIter = TRUE)

gbm_grid =  expand.grid(interaction.depth = 5:10,
                        n.trees = c(3000,4000,5000,6000),
                        shrinkage = c(0.1,0.5,1),
                        n.minobsinnode = 10)


set.seed(40)
gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)  

print(gbm_tune)
# optimal parameters are: n.trees = 4000, interaction.depth = 10, shrinkage = 0.1

# 3: tune n.minobsinnode with optimal parameters
cv_5 = trainControl(method = "cv", number = 5, verboseIter = TRUE)

gbm_grid =  expand.grid(interaction.depth = 10,
                        n.trees = 4000,
                        shrinkage = 0.1,
                        n.minobsinnode = c(5,10,15,20))

set.seed(40)
gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)

print(gbm_tune)

# 4: tune n.minobsinnode + interaction depth
cv_5 = trainControl(method = "cv", number = 5, verboseIter = TRUE)

gbm_grid =  expand.grid(interaction.depth = 10,
                        n.trees = 4000,
                        shrinkage = 0.1,
                        n.minobsinnode = c(4,5,6))

set.seed(40)
gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)

print(gbm_tune)

# gbm model with optimal parameters
model_gbm <- gbm(average_daily_rate ~.,
                data = train_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost
                shrinkage = 0.1,
                n.minobsinnode = 5,
                interaction.depth = 10,
                n.trees = 2250)


val_prediction <- predict(object = model_gbm, newdata = validation_X, type = "response")

# MSE & MAE 
gbm_RMSE <- sqrt(mean((val_prediction - validation_y$average_daily_rate)^2))
print(gbm_RMSE) # 43.50409
gbm_MAE <- mae(validation_y$average_daily_rate, val_prediction)
print(gbm_MAE) # 19.9034

summary(val_prediction)

write.table(gbm_RMSE, file = "data/results/GBM_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(gbm_MAE, file = "data/results/GBM_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)


# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)
# gbm model
final_model_gbm = gbm(average_daily_rate ~.,
                data = train_and_validation_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost           
                shrinkage = .1,
                n.minobsinnode = 5,
                interaction.depth = 10,
                n.trees = 2250)


test_prediction <- predict(object = final_model_gbm, newdata = test_set, type = "response")

# make file with id and corresponding average daily rate
final_gbm_submission <- data.frame(col1 = test_id$x, col2 = test_prediction)

colnames(final_gbm_submission) <- c("id", "average_daily_rate")
write.table(final_gbm_submission, file = "data/results/gbm_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)
