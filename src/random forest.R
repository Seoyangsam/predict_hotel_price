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
mtry_values <- sqrt(ncol(train_X))
tunegrid <- expand.grid(mtry = mtry_values)
rf_default <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric="RMSE",
                      tuneGrid=tunegrid,
                      trControl=control,
                      ntree=10)
print(rf_default)


# retrain Random forest

updated_hyperparams <- list(mtry=5)
rf.train <- train(average_daily_rate~.,
                  data=train_X_data,
                  method='rf',
                  metric="RMSE",
                  trControl=trainControl(method = "cv"),
                  ntree=10,
                  tuneGrid=updated_hyperparams)

hyperparams <- list(
  ntree = 500,  # number of trees in the forest
  mtry = 5      # number of variables to consider at each split
)
model <- train(
  average_daily_rate~.,
  data=train_X_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyperparams
)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)
str(yhat.rf)
sqrt(mean((yhat.rf - validation_y$average_daily_rate)^2))
#calculate the rmse
rf_error <- sqrt(mean((yhat.rf - validation_y$average_daily_rate)^2))
write.table(rf_error, file = "data/results/rf_error.csv", sep = ",", row.names = FALSE, col.names=TRUE)

mae <- mean(abs(predictions - test_data$target))

importance(rf.train)

varImpPlot(rf.train,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")

