
#here we do lasso regression
library(glmnet)
#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
data_id <- read.csv(file = 'data/bronze/data_id.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
train_X_data$average_daily_rate <- train_X_data$x
train_X_data <- subset(train_X_data, select = -c(x,arrival_date,last_status_date))

validation_X_data <- data.frame(validation_X,validation_y)
validation_X_data$average_daily_rate <- validation_X_data$x
validation_X_data <- subset(validation_X_data, select = -c(x,arrival_date,last_status_date))
str(validation_X_data)

lm.fit <- lm(average_daily_rate ~ ., data = train_X_data)
lm.fit
update.packages("Matrix")
library(Matrix)
require(Matrix)

train_X_matrix <- model.matrix(lm.fit, train_X)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)

#set.seed(42) # for cross-validation
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix, train_X_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

pred.lasso <- predict(cv.lasso, s = bestlam.lasso, newx = validation_X_matrix)
lasso.error <- mean((pred.lasso - validation_X_data$average_daily_rate)^2)
coef.lasso <- predict(cv.lasso, type = "coefficients", s = bestlam.lasso)
str(pred.lasso)
pred_lasso <- data.frame(col1 = 1:16595, col2 = pred.lasso)
str(pred_lasso)
colnames(pred_lasso) <- c("id", "average_daily_rate")
write.table(pred_lasso, file = "data/gold/pred_lasso.csv", sep = ",", row.names = FALSE, col.names=TRUE)

data_submission$id <- data_id$x
data_submission <- subset(data_submission, select = c(id))

data_submission$average_daily_rate <- pred.lasso

write.table(data_submission, file = "data/results/submission.csv", sep = ",", row.names = F)

pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = )

pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = )

