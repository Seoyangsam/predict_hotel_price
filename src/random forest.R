library(randomForest)
set.seed(1)
# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
unique(train_X_data$country)

# Convert all columns to factor
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE,levels = levels(train_X_data))

# Hyperparemeter tuning with mtry
library(caret)
library(e1071)
control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3)

metric <- "RMSE"
set.seed(123)
mtry <- sqrt(ncol(train_X))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=tunegrid,
                      trControl=control,
                      ntree = 15)

print(rf_default)


# Random forest
rf.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 10, importance = TRUE, ntree = 15)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)

#calculate the rmse
rf_error <- sqrt(mean((yhat.rf - validation_y$average_daily_rate)^2))
write.table(rf_error, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)

importance(rf.train)

varImpPlot(rf.train,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")

