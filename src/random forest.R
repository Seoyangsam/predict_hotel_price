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

# Hyperparemeter tuning with mtry
library(randomForest)
library(caret)
set.seed(123)
mtry_values <- c(sqrt(ncol(train_X)), ncol(train_X), 0.5*ncol(train_X))
tunegrid <- expand.grid(mtry = mtry_values)
rf_default <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=tunegrid,
                      trControl=trainControl(method="cv", number=10),
                      min.node.size = 500,
                      ntree=100)
print(rf_default)

# retrain Random forest
set.seed(123)
updated_mtry <- expand.grid(mtry=5)
rf.train <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=updated_mtry,
                      trControl=trainControl(method="cv", number=5),
                      ntree=500)
print(rf.train)

rf_model <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 5, ntree = 500)
yhat.rf <- predict(rf_model, newdata = validation_X)

#calculate the rmse
rf_error <- sqrt(mean((yhat.rf - validation_y$average_daily_rate)^2))
rf_error

mae <- mean(abs(yhat.rf - validation_y$average_daily_rate))
mae

r_squared <- 1 - (sum((yhat.rf - validation_y$average_daily_rate)^2) / sum((validation_y$average_daily_rate - mean(validation_y$average_daily_rate))^2))
n <- nrow(validation_X)
p <- ncol(validation_X)
adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)
adj_r_squared

data_frame <- data.frame(
  rf_error = rf_error,
  mae = mae,
  adj_r_squared = adj_r_squared
)
write.table(data_frame, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)

importance(rf.train)

varImpPlot(rf.train,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")

