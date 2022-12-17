# Load the caret package
library(caret)
library(xgboost)

# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

# Make the dataframe to train the model
train_X<- data.frame(train_X,train_y)
validation_X<- data.frame(validation_X,validation_y)

# Combine the training and validation sets
combined <- rbind(train_X, validation_X)

# Create a vector of labels for stratified sampling
labels <- as.factor(combined$booking_distribution_channel)
set.seed(1)
# Split the combined dataset into a new training and validation set using stratified sampling
validation <- createDataPartition(labels, p = 0.3, list = FALSE)
validation_X1<- combined[validation, ]
train_X1 <- combined[-validation, ]

train_X <- subset(train_X1, select = -c(average_daily_rate))
validation_X <- subset(validation_X1, select = -c(average_daily_rate))

validation_y <- validation_X1$average_daily_rate
validation_y
train_y <- train_X1$average_daily_rate
train_y
train_X_data <- data.frame(train_X,train_y)

str(train_X_data)
# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_babies","nr_children","nr_nights","nr_previous_bookings","previous_cancellations","special_requests")

# apply on training set
mean_train <- colMeans(train_X[, scale_cols])
sd_train <- sapply(train_X[, scale_cols], sd)
train_X[, scale_cols] <- scale(train_X[, scale_cols], center = TRUE, scale = TRUE)

# apply on validation set
validation_X[, scale_cols] <- scale(validation_X[, scale_cols], center = mean_train, scale = sd_train)

# Convert all columns to factor
train_X <- as.data.frame(unclass(train_X), stringsAsFactors = TRUE)
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
model <- train(train_y ~ ., data = train_X_data,
               method = "xgbTree",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid = tuning_grid,
               )

# Print the best hyperparameters found by the grid search
print(model)

# Use the trained model to make predictions on the test set
predictions <- predict(model, validation_X)

rf_error <- sqrt(mean((predictions - validation_y)^2))
rf_error