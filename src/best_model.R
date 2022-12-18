# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
test_X <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

test_X <- subset(test_X, select = -c(assigned_room_type))
str(test_X)
# Combine the training and validation sets
train_X <- rbind(train_X, validation_X)
train_y <- rbind(train_y, validation_y)

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

# apply on training set
mean_train <- colMeans(train_X[, scale_cols])
sd_train <- sapply(train_X[, scale_cols], sd)
train_X[, scale_cols] <- scale(train_X[, scale_cols], center = TRUE, scale = TRUE)

# Convert all columns to factor
train_X <- as.data.frame(unclass(train_X), stringsAsFactors = TRUE)

# Make the dataframe to train the model
train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

# retrain Random forest on trainset
library(caret)
set.seed(123)
updated_mtry <- expand.grid(mtry=4)
rf.train <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=updated_mtry,
                      trControl=trainControl(method="cv", number=10),
                      min.node.size = 500,
                      ntree=100)
print(rf.train)

# Prediction on test set
yhat.rf <- predict(rf.train, newdata = test_X)

# Make the dataframe for submiiting on Kaggle
data_frame <- data.frame(ID = id,
  average_daily_rate = yhat.rf
)
write.table(data_frame, file = "data/results/best_model.csv", sep = ",", row.names = FALSE, col.names=TRUE)
