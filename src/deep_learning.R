# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')


# installation
library(reticulate)
library(keras)
library(tensorflow)
library(ggplot2)
library(caret)
library(ISLR2)

tryCatch(
  remove.packages(c("keras", "tensorflow", "reticulate")),
  error = function(e) "Some or all packages not previously installed, that's ok!"
)

install.packages("keras", repos = 'https://cloud.r-project.org')

write('RETICULATE_AUTOCONFIGURE=FALSE', file = "~/.Renviron", append = TRUE)
write(sprintf('RETICULATE_MINICONDA_PATH=%s',
              normalizePath("~/islr-miniconda", winslash = "/", mustWork = FALSE)),
      file = "~/.Renviron", append = TRUE)

Sys.setenv(RETICULATE_AUTOCONFIGURE='FALSE',
           RETICULATE_MINICONDA_PATH=normalizePath("~/islr-miniconda", winslash = "/", mustWork = FALSE))

source(system.file("helpers", "install.R", package = "ISLR2"))

install_miniconda()
install_tensorflow()
print_py_config()

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

train_and_validation_X <- rbind(train_X, validation_X)
dependent_y <- rbind(train_y, validation_y)

train_and_validation_X_data <- data.frame(train_and_validation_X,dependent_y)

 # matrix
library(Matrix)
require(Matrix)

train_X_matrix <- model.matrix(average_daily_rate ~. -1 , data = train_X_data)
train_Y <- train_X_data$average_daily_rate 
validation_X_matrix <- model.matrix(average_daily_rate ~. -1, data = validation_X_data)
validation_Y <- validation_X_data$average_daily_rate

train_and_validation_X_matrix <- model.matrix(average_daily_rate~. -1, data = train_and_validation_X_data)
train_and_validation_Y <- dependent_y$average_daily_rate

test_set_matrix <- model.matrix(~.-1 , data = test_set)

# 1) single layer model structure
# step 1 make architecture powerful enough
singlemodnn <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 1, activation = "linear")
summary(modnn)
singlemodnn %>% compile(loss = "mse",
                   optimizer = optimizer_adam(),
                   metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
# fit the model
singlehistory <- singlemodnn %>% fit(
  train_X_matrix, train_Y, epochs =150 , batch_size = 32,
  validation_data = list(validation_X_matrix,validation_Y))

# plot mean absolute error of training and test data
plot(singlehistory)

# step 3 regularize architecture
regsinglemodnn <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = "linear")

regsinglemodnn %>% compile(loss = "mse",
                   optimizer = optimizer_adam(),
                   metrics = list("mean_absolute_error"))

# step 4 learning convergence
# fit the model
regsinglehistory <- regsinglemodnn %>% fit(
  train_X_matrix, train_Y, epochs = 100, batch_size = 64,
  validation_data = list(validation_X_matrix,validation_Y))

# plot mean absolute error of training and test data
plot(regsinglehistory)


# 2) multilayer model (wide)
# step 1 make model powerful enough
widemodelnn <- keras_model_sequential()
widemodelnn %>%
  layer_dense(units = 800, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

widemodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
widehistory <- widemodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 32,
  validation_data = list(validation_X_matrix,validation_Y))

# plot
plot(widehistory)


# step 3 reguralize architectrure
regwidemodelnn <- keras_model_sequential()
regwidemodelnn %>%
  layer_dense(units = 800, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = "linear")

regwidemodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),
                    metrics = list("mean_absolute_error"))

# step 4 learning convergence 
regwidehistory <- regwidemodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 64,
  validation_data = list(validation_X_matrix,validation_Y))

# plot
plot(regwidehistory)


# 3) multilayer model (deep)
# step 1 make model powerful enough
deepmodelnn <- keras_model_sequential()
deepmodelnn %>%
  layer_dense(units = 300, activation = "relu",
              input_shape =ncol(train_X_matrix)) %>%
  layer_dense(units = 275, activation = "relu") %>%
  layer_dense(units = 250, activation = "relu") %>%
  layer_dense(units = 200, activation = "relu") %>%
  layer_dense(units = 175, activation = "relu") %>%
  layer_dense(units = 150, activation = "relu") %>%
  layer_dense(units = 125, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 75, activation = "relu") %>%
  layer_dense(units = 60, activation = "relu") %>%
  layer_dense(units = 40, activation = "relu") %>%
  layer_dense(units = 30, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

deepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),           # can change learning rate: learning_rate = ...
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
deephistory <- deepmodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 32,
  validation_data = list(validation_X_matrix,validation_Y))
 
# plot
plot(deephistory)

# step 3 reguralize architectrure
regdeepmodelnn <- keras_model_sequential()
regdeepmodelnn %>%
  layer_dense(units = 300, activation = "relu",
              input_shape = ncol(train_X_matrix), constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 275, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 250, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 200, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 175, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 150, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 125, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 100, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 75, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 60, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 40, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 30, activation = "relu", constraint_maxnorm(max_value = 6)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "linear")

regdeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(learning_rate = 0.001),
                    metrics = list("mean_absolute_error")
                    )

# step 4 learning convergence 
regdeephistory <- regdeepmodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 64,
  validation_data = list(validation_X_matrix,validation_Y))

# plot
plot(regdeephistory)


# 4) multilayer model (wide + deep)

regfinaldeepmodelnn <- keras_model_sequential()
regfinaldeepmodelnn %>%
  layer_dense(units = 1500, activation = "relu",
              input_shape = ncol(train_and_validation_X_matrix)) %>%
  layer_dropout(rate = 0.3) %>%
  #kernel_constraint = maxnormConstraint(max_value = 2, axis = 0) %>%
  #max_norm(3) %>%
  layer_dense(units = 1200, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  #max_norm(3) %>%
  layer_dense(units = 900, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  #max_norm(3) %>%
  layer_dense(units = 600, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = "linear")

regfinaldeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),
                    metrics = list("mean_absolute_error"))

regfinaldeepmodelnn %>%
    fit(train_and_validation_X_matrix, train_and_validation_Y, epochs = 150, batch_size = 300)

y_test_pred <- regfinaldeepmodelnn %>% predict(test_set_matrix)
str(test)
# make file with id and corresponding average daily rate
final_deep_learning_submission <- data.frame(col1 = test_id$x, col2 = y_test_pred)

colnames(final_deep_learning_submission) <- c("id", "average_daily_rate")
write.table(final_deep_learning_submission, file = "data/results/deep_learning_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)



# model that has almost no gap

regdeepmodelnn <- keras_model_sequential()
regdeepmodelnn %>%
  layer_dense(units = 100, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 800, activation = "relu") %>%
  layer_dense(units = 600, activation = "relu") %>%
  layer_dense(units = 400, activation = "relu") %>%
  layer_dense(units = 200, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 80, activation = "relu") %>%
  layer_dense(units = 40, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

widedeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),                     # can change learning rate: learning_rate = ...
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
widedeephistory <- widedeepmodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 32,
  validation_data = list(validation_X_matrix,validation_Y))
 
# plot
plot(widedeephistory)

# step 3 reguralize architectrure
regwidedeepmodelnn <- keras_model_sequential()
regwidedeepmodelnn %>%
  layer_dense(units = 1000, activation = "relu",
              input_shape = ncol(train_X_matrix),constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 800, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 600, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 400, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 200, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 100, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 80, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 40, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "linear")

regwidedeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(learning_rate = 0.001),
                    metrics = list("mean_absolute_error")
                    )

# step 4 learning convergence 
regwidedeephistory <- regwidedeepmodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 150, batch_size = 64,
  validation_data = list(validation_X_matrix,validation_Y))

# plot
plot(regwidedeephistory)



# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

regfinaldeepmodelnn <- keras_model_sequential()
regwidedeepmodelnn %>%
  layer_dense(units = 1000, activation = "relu",
              input_shape = ncol(train_and_validation_X_matrix),constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 800, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 600, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 400, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 200, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 100, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 80, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 40, activation = "relu", constraint_maxnorm(max_value = 5)) %>%
  layer_dropout(rate = 0.2) %>%
 
  layer_dense(units = 1, activation = "linear")

regfinaldeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_adam(),
                    metrics = list("mean_absolute_error"))

regfinaldeepmodelnn %>%
    fit(train_and_validation_X_matrix, train_and_validation_Y, epochs = 150, batch_size = 64)

y_test_pred <- regfinaldeepmodelnn %>% predict(test_set_matrix)

# make file with id and corresponding average daily rate
final_deep_learning_submission <- data.frame(col1 = test_id$x, col2 = y_test_pred)

colnames(final_deep_learning_submission) <- c("id", "average_daily_rate")
write.table(final_deep_learning_submission, file = "data/results/deep_learning_submission.csv", sep = ",", row.names = FALSE, col.names=TRUE)
