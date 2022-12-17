# Load the caret package
library(caret)
install.packages("xgboost")
library(xgboost)

# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_babies","nr_children","nr_nights","nr_previous_bookings","previous_cancellations","special_requests")

# apply on training set
mean_train <- colMeans(train_X[, scale_cols])
sd_train <- sapply(train_X[, scale_cols], sd)
train_X[, scale_cols] <- scale(train_X[, scale_cols], center = TRUE, scale = TRUE)

# apply on validation set
validation_X[, scale_cols] <- scale(validation_X[, scale_cols], center = mean_train, scale = sd_train)

# Make the dataframe to train the model
train_X_data <- data.frame(train_X,train_y)

# Convert all columns to factor
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE,levels = levels(train_X_data))

# Define the range of values to explore for the max_depth and eta parameters
max_depth_values <- c(3, 5, 7)
eta_values <- c(0.1, 0.3, 0.5)

# Create the grid of values to search over
param_grid <- expand.grid(max_depth = max_depth_values, eta = eta_values)

# Fit the model using 10-fold cross-validation
model <- train(target ~ ., data = train_X_data,
               method = "xgbTree",
               trControl = trainControl(method = "cv", number = 3),
               tuneGrid = params)

# Print the best hyperparameters found by the grid search
print(xgb_model$bestTune)

# Use the best hyperparameters to train a new XGBoost model on the entire training set
xgb_model <- xgboost(
  data = xgb.DMatrix(train_X,train_y),
  nrounds = xgb_model$bestTune$nrounds,
  max_depth = xgb_model$bestTune$max_depth,
  eta = xgb_model$bestTune$eta
)

# Use the trained model to make predictions on the test set
predictions <- predict(xgb_model, test)