# Load the caret package
library(caret)
set.seed(1)

# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

# apply on training set
mean_train <- colMeans(train_X[, scale_cols])
sd_train <- sapply(train_X[, scale_cols], sd)
train_X[, scale_cols] <- scale(train_X[, scale_cols], center = TRUE, scale = TRUE)

# apply on validation set
validation_X[, scale_cols] <- scale(validation_X[, scale_cols], center = mean_train, scale = sd_train)

# Convert all columns to factor
train_X <- as.data.frame(unclass(train_X), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(validation_X), stringsAsFactors = TRUE,levels = levels(train_X))

train_X_data <- data.frame(train_X,train_y)

str(train_X_data)

# Create the grid of values to search over
tuning_grid <- expand.grid(nrounds = 120,
                           max_depth = 1,
                           eta = 0.1,
                           gamma = 0.5,
                           colsample_bytree = 0.5,
                           min_child_weight = 1,
                           subsample = 0.5)

# Fit the model using 10-fold cross-validation
model <- train(average_daily_rate ~ ., data = train_X_data,
               method = "xgbTree",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = tuning_grid,
               verbose = FALSE,
               lambda = 0.5,
               alpha = 0.5
               )

# Print the best hyperparameters found by the grid search
print(model)

# Use the trained model to make predictions on the test set
predictions <- predict(model, validation_X)
df <-(predictions - validation_y)^2
str(df)

#calculate the rmse
rf_error <- sqrt(mean(df$average_daily_rate))
rf_error

mae <- mean(abs(predictions - validation_y))
mae

data_frame <- data.frame(rf_error = rf_error,
  mae = mae
)
write.table(data_frame, file = "data/results/xgboost_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)