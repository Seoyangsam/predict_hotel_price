#here we do RIDGE regression
library(glmnet)

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')

test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# linear regression
lm.fit <- lm(average_daily_rate ~ ., data = train_X_data)
lm.fit

# prepare the data to be used with a ridge regression model (alpha=0)
library(Matrix)
require(Matrix)

train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(lm.fit, train_X_data)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(validation_X_matrix)

# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.ridge <- cv.glmnet(train_X_matrix, train_y_data$average_daily_rate ,alpha = 0, lambda = grid, nfolds = 5)
bestlam.ridge <- cv.ridge$lambda.min

# make predictions on validation set
pred.ridge.valset <- predict(cv.ridge, s = bestlam.ridge, newx = validation_X_matrix )
str(pred.ridge.valset)

# MSE
pred_valset_mse <- sqrt(mean((pred.ridge.valset - validation_y$average_daily_rate)^2))
write.table(pred_valset_mse, file = "data/results/lin_model_ridge_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# MAE 
pred_valset_mae <- mae(validation_y$average_daily_rate, pred.ridge.valset)
write.table(pred_valset_mae, file = "data/results/lin_model_ridge_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# slightly better than regularized linear model with lasso

