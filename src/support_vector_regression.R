install.packages("e1071")
library(e1071)

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE)

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# LINEAR KERNEL
# make model and determine optimal value for cost
svmfit_linear <- svm(average_daily_rate ~ ., data = train_X_data, kernel = "linear", cost = 10, scale = FALSE)

tune_out_linear <- tune(svm, average_daily_rate ~ ., data = train_X_data, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out_linear)

bestmod_linear <- tune.out_linear$best.model
summary(bestmod_linear)

# make predictions
testdat <- data.frame(x = validation_X, y = as.factor(validation_y))
ypred_linear <- predict(bestmod_linear, testdat)

# RADIAL KERNEL
# make model and determine optimal value for cost
tune.out_radial = tune(svm, average_daily_rate ~ ., data = train_X_data, kernel = "radial", ranges = list(cost = c(0.001,0.01,0.1, 1, 10, 100), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

bestmod_radial <- tune.out_radial$best.model
summary(bestmod_radial)

# make predictions
testdat <- data.frame(x = validation_X, y = as.factor(validation_y))
ypred_radial <- predict(bestmod_radial, testdat)

# POLYNOMIAL KERNEL
tune.out_poly = tune(svm, average_daily_rate ~ ., data = train_X_data, kernel= "polynomial", ranges = list(cost = c(0.001,0.01,0.1, 1, 10, 100), degree= c(2, 3, 4, 5)))
summary(tune.out)

bestmod_poly <- tune.out_poly$best.model
summary(bestmod_poly)

# make predictions
testdat <- data.frame(x = validation_X, y = as.factor(validation_y))
ypred_poly <- predict(bestmod_poly, testdat)


plot(svm(average_daily_rate ~ ., data = train_X_data, kernel= "linear", cost= lin.tune.out$best.parameters), test)
plot(svm(average_daily_rate ~ ., data = train_X_data, kernel= "polynomial", degree= poly.tune.out$best.parameters), test)
plot(svm(average_daily_rate ~ ., data = train_X_data, kernel = "radial", gamme= rad.tune.out$best.parameters), test)
