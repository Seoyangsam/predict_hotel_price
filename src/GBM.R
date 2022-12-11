# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# packages
install.packages('gbm')                    # for fitting the gradient boosting model
install.packages('caret')       # for general data preparation and model fitting

library(gbm)
library(caret)

# create 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# gbm model
model_gbm = gbm(average_daily_rate ~.,
                data = train_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost
                cv.folds = 3,
                shrinkage = 0.1,
                n.minobsinnode = 10,
                interaction.depth = 10,
                n.trees = 3000)

ntree_opt_cv <- gbm.perf(model_gbm, method = "cv")

val_prediction <- predict(object = model_gbm, newdata = validation_X_data, n.trees = ntree_opt_cv, type = "response")

gbm.error <- sqrt(mean((val_prediction - validation_y$average_daily_rate)^2))
gbm.error

# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)
# gbm model
final_model_gbm = gbm(average_daily_rate ~.,
                data = train_and_validation_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost
                cv.folds = 3,
                shrinkage = .01,
                n.minobsinnode = 10,
                interaction.depth = 6,
                n.trees = 5000)


ntree_opt_cv <- gbm.perf(final_model_gbm, method = "cv")

test_prediction <- predict(object = final_model_gbm, newdata = test_set, n.trees = ntree_opt_cv, type = "response")

# make file with id and corresponding average daily rate
final_gbm_submission <- data.frame(col1 = test_id$x, col2 = test_prediction)

colnames(final_gbm_submission) <- c("id", "average_daily_rate")
write.table(final_gbm_submission, file = "data/results/gbm_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# packages
install.packages('gbm')                    # for fitting the gradient boosting model
install.packages('caret')       # for general data preparation and model fitting

library(gbm)
library(caret)

# create 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# tune parameters using cv k=5
cv_5 = trainControl(method = "cv", number = 5)

gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)

summary(gbm_grid)                    

gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      trControl = cv_5,
                      verbose = FALSE,
                      tuneGrid = gbm_grid)    
                                          

gbm_pred = predict(gbm_tune, newdata = validation_X, n.trees = )

gbm.error <- sqrt(mean((gbm_pred - validation_y$average_daily_rate)^2))
gbm.error