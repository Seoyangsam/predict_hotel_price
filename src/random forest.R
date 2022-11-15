library(randomForest)
set.seed(1)
#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

bag.train <- randomForest(average_daily_rate ~ ., data = train_X_data,  importance = TRUE)
bag.train