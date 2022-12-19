install.packages("e1071")
install.packages("caret")
install.packages("Metrics")
library(e1071)
library(caret)
library(Metrics)

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)

# average daily rate as vector and dataframe into matrix
average_daily_rate <- train_y[['average_daily_rate']]
train_X_matrix <- data.matrix(train_X)

validation_X_data <- data.frame(validation_X,validation_y)

# hyperparameter tuning for sigma and cost using cross-validation
ctrl <- trainControl(method = "cv", number=5, verboseIter = TRUE)

SVRGrid <- expand.grid(.sigma=c(0.01,0.1,1) , .C=c(1,10,100))

set.seed(50)
SVRFit <- train(x = train_X, y = average_daily_rate, method="svmRadial", metric = "RMSE", tuneGrid = SVRGrid, trControl = ctrl, type="eps-svr", verbose = TRUE)

SVRFit$bestTune
SVRFit$bestTune$sigma
SVRFit$bestTune$C

# training with optimal parameters 
SVR_final_model <- svm(average_daily_rate~ ., data = train_X_data, kernel = "radial", cost = 10 , sigma = 0.01)

# prediction on validation set
prediction <- predict(object = SVR_final_model, newdata = validation_X)

# MSE & MAE 
svr_RMSE <- sqrt(mean((prediction - validation_y$average_daily_rate)^2))
svr_MAE <- mae(validation_y$average_daily_rate, prediction)

write.table(svr_RMSE, file = "data/results/SVR_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(svr_MAE, file = "data/results/SVR_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

SVRFitCoarse$finalModel

# retrain on validation + training set and predict on test set
train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)

# svr model
svr_test_set <- svm(average_daily_rate~ ., data = train_and_validation_X_data, kernel = "radial", cost = 10 , sigma = 0.01)

prediction <- predict(object = svr_test_set, newdata = test_set)

# make file with id and corresponding average daily rate
svr_submission <- data.frame(col1 = test_id$x, col2=prediction)

colnames(svr_submission) <- c("id","average_daily_rate")
write.table(svr_submission, file = "data/results/svr_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)

