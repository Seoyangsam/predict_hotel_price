
#here we do lasso regression
library(glmnet)
#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# linear regression
lm.fit <- lm(average_daily_rate ~ ., data = train_X_data)
lm.fit

# prepare the data to be used with a lasso regression model
library(Matrix)
require(Matrix)

train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(lm.fit, train_X_data)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(validation_X_matrix)
# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix, train_y_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

# make predictions on validation set

pred.lasso.valset <- predict(cv.lasso, s = bestlam.lasso, newx = validation_X_matrix )
str(pred.lasso.valset)
pred_valset_error <- sqrt(mean((pred.lasso.valset - validation_y$average_daily_rate)^2))

write.table(pred_valset_error, file = "data/results/linear_model_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

# new dataframe with train + val set and add average daily rate
train_and_validation <- rbind(train_X, validation_X)
dependant_y <- rbind(train_y, validation_y)
write.table(train_and_validation, file = "data/gold/train_and_validation.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(dependant_y, file = "data/gold/dependant_y.csv", sep = ",", row.names = FALSE, col.names=TRUE)

train_val_data <- data.frame(train_and_validation, dependant_y)

# linear regression
lm.fit2 <- lm(average_daily_rate ~ ., data = train_val_data)
lm.fit2

# prepare the data to be used with a lasso regression model
library(Matrix)
require(Matrix)

train_val_y_data <- subset(train_val_data, select= c(average_daily_rate))

train_X_matrix2 <- model.matrix(lm.fit2, train_val_data)
test_set_matrix <- model.matrix(~., data = test_set)

# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix2, train_val_y_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

# make predictions on test set
pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = test_set_matrix)


# make file with id and corresponding average daily rate
lin_submission <- data.frame(col1 = test_id$x, col2 = pred.lasso.testset)

colnames(lin_submission) <- c("id", "average_daily_rate")
write.table(lin_submission, file = "data/results/lin_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)