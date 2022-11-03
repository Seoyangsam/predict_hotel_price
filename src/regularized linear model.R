

#here we do lasso regression
library(glmnet)
cv.lasso <- cv.glmnet(x=train.x, y=train.y, nfolds = 5, lambda = grid, alpha=1)
bestlam.lasso <- cv.lasso$lambda.min
pred.lasso <- predict(cv.lasso, newx=test.x, s=bestlam.lasso)
lasso.error <- sum(((college.test$Apps - pred.lasso)^2)/length(pred.lasso))
coef.lasso <- predict(cv.lasso, s = bestlam.lasso, type = "coefficients")