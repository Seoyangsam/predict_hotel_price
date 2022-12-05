library(randomForest)
set.seed(1)
#read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

#bagging
#bag.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 24, importance = TRUE)
#yhat.bag <- predict(bag.train, newdata = validation_X)
#mean((yhat.bag - validation_y)^2)

#random forest
rf.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 6, importance = TRUE)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)
mean((yhat.rf - validation_y)^2)