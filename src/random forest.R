library(randomForest)
library(dummy)
set.seed(1)
# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

# Kepp top 15 values for country
cat <- categories(train_X_data[("country")], p = 15)
train_X_data$country[!(train_X_data$country %in% cat)] <- "others"
str(train_X_data$country)

cat <- categories(validation_X[("country")], p = 15)
validation_X$country[!(validation_X$country %in% cat)] <- "others"
str(validation_X$country)

# Convert all columns to factor
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE,levels = levels(train_X_data))

# Random forest
rf.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 7, importance = TRUE, ntree = 15)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)
mean((yhat.rf - validation_y$average_daily_rate)^2)

