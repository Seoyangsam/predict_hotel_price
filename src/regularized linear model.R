
#here we do lasso regression
library(glmnet)
#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')


# dependent and independent variables in 1 dataframe and delete arrival date and last status date
train_X_data <- data.frame(train_X,train_y)
train_X_data$average_daily_rate <- train_X_data$x
train_X_data <- subset(train_X_data, select = -c(x,arrival_date,last_status_date))


validation_X_data <- data.frame(validation_X,validation_y)
validation_X_data$average_daily_rate <- validation_X_data$x
validation_X_data <- subset(validation_X_data, select = -c(x,arrival_date,last_status_date))

# delete arrival date and last status date from test set
test_set <- subset(test_set, select = -c(arrival_date,last_status_date))

# linear regression
lm.fit <- lm(average_daily_rate ~ ., data = train_X_data)
lm.fit

# prepare the data to be used with a lasso regression model
library(Matrix)
require(Matrix)

train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(lm.fit, train_X_data)
test_set_matrix <- model.matrix(~., data = test_set)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(validation_X_matrix)
# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix, train_y_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

# make predictions on test set

pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = test_set_matrix )

colnames(test_set)

# make file with id and corresponding average daily rate
lin_submission <- data.frame(col1 = test_id$x, col2 = pred.lasso.testset)

colnames(lin_submission) <- c("id", "average_daily_rate")
write.table(lin_submission, file = "data/results/lin_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)



