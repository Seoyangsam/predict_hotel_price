set.seed(1)

# import libraris
library(randomForest)
library(caret)

# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

# Make the dataframe to train the model
#train_X<- data.frame(train_X,train_y)
#validation_X<- data.frame(validation_X,validation_y)

# Combine the training and validation sets
#combined <- rbind(train_X, validation_X)

# Create a vector of labels for stratified sampling
labels <- as.factor(combined$assigned_room_type)

# Split the combined dataset into a new training and validation set using stratified sampling
#validation <- createDataPartition(labels, p = 0.3, list = FALSE)
#validation_X1<- combined[validation, ]
#train_X1 <- combined[-validation, ]

#train_X <- subset(train_X1, select = -c(average_daily_rate))
#validation_X <- subset(validation_X1, select = -c(average_daily_rate))

#validation_y <- validation_X1$average_daily_rate
#train_y <- train_X1$average_daily_rate
#train_X_data <- data.frame(train_X,train_y)
#str(train_X_data)
# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

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
validation_X <-as.data.frame(unclass(validation_X), stringsAsFactors = TRUE,levels = levels(train_X_data))

# Hyperparemeter tuning with mtry
mtry_values <- c(sqrt(ncol(train_X)), ncol(train_X), 0.5*ncol(train_X))
tunegrid <- expand.grid(mtry = mtry_values)
rf_default <- train(train_y~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=tunegrid,
                      trControl=trainControl(method="cv", number=10),
                      min.node.size = 500,
                      ntree=100)
print(rf_default)

# retrain Random forest
updated_mtry <- expand.grid(mtry=4)
rf.train <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=updated_mtry,
                      trControl=trainControl(method="cv", number=10, verboseIter = TRUE),
                      min.node.size = 500,
                      ntree=100)
print(rf.train)

yhat.rf <- predict(rf.train, newdata = validation_X)

#calculate the rmse
rf_error <- sqrt(mean((yhat.rf - validation_y)^2))
rf_error

mae <- mean(abs(yhat.rf - validation_y))
mae

#data_frame <- data.frame(rf_error = rf_error,
  #mae = mae
#)
#write.table(data_frame, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)

write.table(rf_error, file = "data/results/rf_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(mae, file = "data/results/rf_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)


# retrain Random forest
set.seed(123)
updated_mtry <- expand.grid(mtry=5)
rf.train <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=updated_mtry,
                      min.node.size = 20,
                      ntree=100)
print(rf.train)

yhat.rf <- predict(rf.train, newdata = validation_X)

#calculate the rmse
rf_error <- sqrt(mean((yhat.rf - validation_y)^2))
rf_error

mae <- mean(abs(yhat.rf - validation_y))
mae

#data_frame <- data.frame(rf_error = rf_error,
  #mae = mae
#)
#write.table(data_frame, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)

write.table(rf_error, file = "data/results/rf_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(mae, file = "data/results/rf_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
