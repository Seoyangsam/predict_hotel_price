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
#write.table(train_X_data, file = "data/results/train_X_data.csv", sep = ",", row.names = FALSE, col.names=TRUE)
#write.table(train_X_data, file = "data/results/probeersel.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# average daily rate as vector and dataframe into matrix
average_daily_rate <- train_y[['average_daily_rate']]
train_X_matrix <- data.matrix(train_X)

validation_X_data <- data.frame(validation_X,validation_y)

ctrl <- trainControl(method = "cv", number=2, verboseIter = TRUE)

SVRGrid <- expand.grid(.sigma=c(0.01) , .C=c(10))

# first way: gives error
#VRFitCoarse <- train(x = train_X, y = train_y, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")
# second way
set.seed(50)
SVRFit <- train(x = train_X, y = average_daily_rate, method="svmRadial", metric = "RMSE", tuneGrid = SVRGrid, trControl = ctrl, type="eps-svr", verbose = TRUE)

SVRFit$bestTune
SVRFit$bestTune$sigma
SVRFit$bestTune$C

SVR_final_model <- svm(average_daily_rate~ ., data = train_X_data, kernel = "radial", cost = 10 , sigma = 0.01)
prediction <- predict(object = SVR_final_model, newdata = validation_X)

# MSE & MAE 
svr_RMSE <- sqrt(mean((prediction - validation_y$average_daily_rate)^2))
print(svr_RMSE)
svr_MAE <- mae(validation_y$average_daily_rate, prediction)
print(svr_MAE)

summary(prediction)

write.table(svr_RMSE, file = "data/results/SVR_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(svr_MAE, file = "data/results/SVR_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# third way
#SVRFitCoarse <- train(average_daily_rate ~ . , data = train_X_data, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")

SVRFitCoarse$finalModel


train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)

# svr model
svr_test_set <- svm(average_daily_rate~ ., data = train_and_validation_X_data, kernel = "radial", cost = 10 , sigma = 0.01)

prediction <- predict(object = svr_test_set, newdata = test_set)

# make file with id and corresponding average daily rate
svr_submission <- data.frame(col1 = test_id$x)

colnames(svr_submission) <- c("id")
write.table(svr_submission, file = "data/results/svr_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)

