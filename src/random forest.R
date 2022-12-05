library(randomForest)
set.seed(1)
#read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

cat <- categories(train_X_data[("country")], p = 15)
str(cat)
train_X_data$country[!(train_X_data$country %in% cat)] <- "others"
str(train_X_data$country)

# Convert all columns to factor
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)

#bagging
#bag.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 24, importance = TRUE)
#yhat.bag <- predict(bag.train, newdata = validation_X)
#mean((yhat.bag - validation_y)^2)

#random forest
rf.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 6, importance = TRUE, ntree = 25)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)
yhat.rf
validation_y
mean((yhat.rf - validation_y$average_daily_rate)^2)