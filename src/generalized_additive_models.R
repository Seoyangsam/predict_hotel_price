
# here we will perform GAM 
install.packages("gam")
library(gam)
library(glmnet)

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE)
train_and_validation <- read.csv(file = 'data/gold/train_and_validation.csv', header = TRUE, fileEncoding = 'latin1')
dependant_y <- read.csv(file = 'data/gold/dependant_y.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# determine optimal degree of freedom for all numerical variables except car parking spaces, nr babies, nr of children because less than 4 unique values
fit_lead_time <- smooth.spline(train_X_data$lead_time, train_X_data$average_daily_rate, cv = FALSE)
fit_nr_adults <- smooth.spline(train_X_data$nr_adults, train_X_data$average_daily_rate, cv = FALSE, tol=0.1)
fit_nr_nights <- smooth.spline(train_X_data$nr_nights, train_X_data$average_daily_rate, cv = FALSE, tol=0.1)
fit_nr_previous_bookings <- smooth.spline(train_X_data$nr_previous_bookings, train_X_data$average_daily_rate, cv = FALSE, tol=0.1)
fit_previous_cancellations <- smooth.spline(train_X_data$previous_cancellations, train_X_data$average_daily_rate, cv = FALSE, tol=0.1)
fit_special_requests <- smooth.spline(train_X_data$special_requests, train_X_data$average_daily_rate, cv = FALSE)

# SMOOTHING SPLINES 
# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# GAM
gam <- lm(average_daily_rate ~ . - lead_time - nr_adults - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + s(lead_time, fit_lead_time$df) + s(nr_adults, fit_nr_adults$df) + s(nr_nights, fit_nr_nights$df) + s(nr_previous_bookings, fit_nr_previous_bookings$df) + s(previous_cancellations, fit_previous_cancellations$df) + s(special_requests, fit_special_requests$df), data = train_X_data)

# prepare the data to be used with a lasso regression model
library(Matrix)
require(Matrix)

train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(gam, train_X_data)
test_set_matrix <- model.matrix(~., data = test_set)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(validation_X_matrix)

# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix, train_y_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

# make predictions on test set
pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = test_set_matrix) #error

# make predictions on validation set
pred.valset <- predict(gam, newdata = validation_X)
str(pred.valset)

# MSE 
pred_valset_error <- sqrt(mean((pred.valset - validation_y$average_daily_rate)^2))
write.table(pred_valset_error, file = "data/results/smooothingspline_model_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# MAE 
pred_valset_mae <- mae(train_y$average_daily_rate, predict(gam))
write.table(pred_valset_mae, file = "data/results/smoothingspline_model_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)


# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

# new dataframe with train + val set and add average daily rate
train_and_validation <- rbind(train_X, validation_X)
dependant_y <- rbind(train_y, validation_y)

train_val_data <- data.frame(train_and_validation, dependant_y)

# GAM with new training data
gam2 <- lm(average_daily_rate ~ . - lead_time - nr_adults - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + s(lead_time, fit_lead_time$df) + s(nr_adults, fit_nr_adults$df) + s(nr_nights, fit_nr_nights$df) + s(nr_previous_bookings, fit_nr_previous_bookings$df) + s(previous_cancellations, fit_previous_cancellations$df) + s(special_requests, fit_special_requests$df), data = train_val_data)

# predict on test set
preds <- predict(gam, newdata = test_set)

# make file with id and corresponding average daily rate
gam_submission <- data.frame(col1 = test_id$x, col2 = preds)

colnames(gam_submission) <- c("id", "average_daily_rate")
write.table(gam_submission, file = "data/results/gam_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)



## LOCAL REGRESSION
# FIRST STEP

# GAM
gam2 <- lm(average_daily_rate ~ . - lead_time - nr_adults - nr_babies - nr_children - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + lo(lead_time, span =0.5) + lo(nr_adults, span=0.5) + lo(nr_babies, span=0.5) + lo(nr_children, span=0.5) + lo(nr_nights, span=0.5) + lo(nr_previous_bookings, span=0.5) + lo(previous_cancellations, span=0.5) + lo(special_requests, span=0.5), data = train_X_data)

# prepare the data to be used with a lasso regression model
library(Matrix)
require(Matrix)

train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(gam2, train_X_data)
test_set_matrix <- model.matrix(~., data = test_set)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(validation_X_matrix)

# fit a lasso regression model with CV
grid <- 10 ^ seq(4, -2, length = 100)
cv.lasso <- cv.glmnet(train_X_matrix, train_y_data$average_daily_rate ,alpha = 1, lambda = grid, nfolds = 5)
bestlam.lasso <- cv.lasso$lambda.min

# make predictions on test set
pred.lasso.testset <- predict(cv.lasso, s = bestlam.lasso, newx = test_set_matrix) #error

# make predictions on validation set
pred.valset <- predict(gam2, newdata = validation_X)
str(pred.valset)

# MSE 
pred_valset_error <- sqrt(mean((pred.valset - validation_y$average_daily_rate)^2))
write.table(pred_valset_error, file = "data/results/localreg_model_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# MAE 
pred_valset_mae <- mae(train_y$average_daily_rate, predict(gam2))
write.table(pred_valset_mae, file = "data/results/localreg_model_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# SECOND STEP

train_val_data <- data.frame(train_and_validation, dependant_y)

# GAM
gam2 <- lm(average_daily_rate ~ . - lead_time - nr_adults - nr_babies - nr_children - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + lo(lead_time, span =0.5) + lo(nr_adults, span=0.5) + lo(nr_babies, span=0.5) + lo(nr_children, span=0.5) + lo(nr_nights, span=0.5) + lo(nr_previous_bookings, span=0.5) + lo(previous_cancellations, span=0.5) + lo(special_requests, span=0.5), data = train_val_data)

# predict on test set
preds <- predict(gam2, newdata = test_set)

# make file with id and corresponding average daily rate
local_reg_submission <- data.frame(col1 = test_id$x, col2 = preds)

colnames(local_reg_submission) <- c("id", "average_daily_rate")
write.table(local_reg_submission, file = "data/results/local_reg_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)
