set.seed(1)

# import libraris
library(randomForest)
library(caret)

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

# Make the dataframe to train the model
train_X_data <- data.frame(train_X,train_y)

# Hyperparemeter tuning with mtry
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
updated_mtry <- expand.grid(mtry=sqrt(ncol(train_X)))
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
df<-yhat.rf - validation_y
#calculate the rmse
rf_error <- sqrt(mean((df$average_daily_rate)^2))
rf_error

mae <- mean(abs(df$average_daily_rate))
mae

data_frame <- data.frame(rf_error = rf_error, mae = mae)
write.table(data_frame, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)



