install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE)

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
#write.table(train_X_data, file = "data/results/train_X_data.csv", sep = ",", row.names = FALSE, col.names=TRUE)
#write.table(train_X_data, file = "data/results/probeersel.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# average daily rate as vector and dataframe into matrix
average_daily_rate <- train_y[['average_daily_rate']]
train_X_matrix <- data.matrix(train_X)

validation_X_data <- data.frame(validation_X,validation_y)

ctrl <- trainControl(method = "cv", number=2) 

SVRGridCoarse <- expand.grid(.sigma=c(0.01,0.1,1) , .C=c(1,10,100))

# first way: gives error
#VRFitCoarse <- train(x = train_X, y = train_y, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")
# second way
SVRFitCoarse <- train(x = train_X, y = average_daily_rate, method="svmLinear", trControl=ctrl, type="eps-svr")
# third way
#SVRFitCoarse <- train(average_daily_rate ~ . , data = train_X_data, method="svmRadial", tuneGrid=SVRGridCoarse, trControl=ctrl, type="eps-svr")

