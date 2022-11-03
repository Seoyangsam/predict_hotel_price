
#here we do lasso regression
library(glmnet)
#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
test_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
test_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')

train_X <- model.matrix(~., data =train_X)
test_X <- model.matrix(~., data =test_X)

grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(x=train_X, y=train_y, nfolds = 5, lambda = grid, alpha=1)
bestlam.lasso <- cv.lasso$lambda.min
pred.lasso <- predict(cv.lasso, newx=test_X, s=bestlam.lasso)
lasso.error <- sum(((test_y - pred.lasso)^2)/length(pred.lasso))
coef.lasso <- predict(cv.lasso, s = bestlam.lasso, type = "coefficients")