# GBM

install.packages('gbm')
library(gbm)
library(caret)

# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE)
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')


# create 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# 1: tune parameters using cv k=5
cv_5 = trainControl(method = "cv", number = 5)

gbm_grid =  expand.grid(interaction.depth = 1:5,
                        n.trees = (1:6) * 500,
                        shrinkage = c(0.001, 0.01, 0.1),
                        n.minobsinnode = 10)
                    

gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)

print(gbm_tune)
# optimal parameters are: n.trees = 3000, interaction.depth = 5, shrinkage = 0.1
# since these are all the maximum values, we tune again to see if we can further improve

# 2: tune parameters using cv k=5
cv_5 = trainControl(method = "cv", number = 5, verboseIter = TRUE)

gbm_grid =  expand.grid(interaction.depth = 5:10,
                        n.trees = c(3000,4000,5000,6000),
                        shrinkage = c(0.1,0.5,1),
                        n.minobsinnode = 10)



gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)  

print(gbm_tune)
# optimal parameters are: n.trees = 4000, interaction.depth = 10, shrinkage = 0.1

# 3: tune n.minobsinnode with optimal parameters
cv_5 = trainControl(method = "cv", number = 5, verboseIter = TRUE)

gbm_grid =  expand.grid(interaction.depth = 10,
                        n.trees = 4000,
                        shrinkage = 0.1,
                        n.minobsinnode = c(5,10,15,20))


gbm_tune = train(average_daily_rate ~ ., data = train_X_data,
                      method = "gbm",
                      distribution = "gaussian",
                      train.fraction = 0.5,
                      trControl = cv_5,
                      verbose = TRUE,
                      tuneGrid = gbm_grid)

print(gbm_tune)

# gbm model with optimal parameters
model_gbm <- gbm(average_daily_rate ~.,
                data = train_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost
                shrinkage = 0.1,
                n.minobsinnode = 4,
                interaction.depth = 10,
                n.trees = 4000)


val_prediction <- predict(object = model_gbm, newdata = validation_X, type = "response")

# MSE & MAE 
gbm_RMSE <- sqrt(mean((val_prediction - validation_y$average_daily_rate)^2))
print(gbm_RMSE) # 43.50409
gbm_MAE <- mae(validation_y$average_daily_rate, val_prediction)
print(gbm_MAE) # 19.9034

summary(val_prediction)

write.table(gbm_RMSE, file = "data/results/GBM_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
write.table(gbm_MAE, file = "data/results/GBM_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)


# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)
# gbm model
final_model_gbm = gbm(average_daily_rate ~.,
                data = train_and_validation_X_data,
                distribution = "gaussian",             #possibilities: gaussian,laplace,bernouilli,adaboost           
                shrinkage = .1,
                n.minobsinnode = 4,
                interaction.depth = 10,
                n.trees = 4000)



test_prediction <- predict(object = final_model_gbm, newdata = test_set, type = "response")

# make file with id and corresponding average daily rate
final_gbm_submission <- data.frame(col1 = test_id$x, col2 = test_prediction)

colnames(final_gbm_submission) <- c("id", "average_daily_rate")
write.table(final_gbm_submission, file = "data/results/gbm_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)
