# here we will perform a regression spline 

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')

test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)
str(validation_X)

# regression spline 
library (splines)
smooth.spline (average_daily_rate)
 
fit <- lm(average_daily_rate ~ bs(train_X_data, knots = c()) , data = validation_X_data)
pred <- predict(fit, newdata = list(train_X_data = train_X_data.grid), se = T)

fit2 <- lm(average_daily_rate ~ ns(train_X_data, df = 4), data = validation_X_data)
pred2 <- predict(fit2, newdata = list(train_X_data = train_X_data.grid), se = T)

#smoothing spline
fit <- smooth.spline(train_X_data, average_daily_rate, df = ?)
fit2 <- smooth.spline(train_X_data, average_daily_rate, cv = TRUE) 
fit2$df 

#local regression 
fit <- loess(average_daily_rate ~ train_X_data, span = ? , data = validation_X_data)
fit2 <- loess(average_daily_rate ~ train_X_data , span = ? , data = validation_X_data)
