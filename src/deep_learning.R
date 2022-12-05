# read data
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# installation
install.packages("reticulate")
install.packages("keras")
install.packages("tensorflow")

library(reticulate)
library(keras)
library(tensorflow)

install.packages("ISLR2")
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

# matrix
library(Matrix)
require(Matrix)
train_X_matrix <- model.matrix(average_daily_rate ~. -1, data = train_X_data)
validation_X_matrix <- model.matrix(average_daily_rate ~. -1, data = validation_X_data)

# 1) single layer model structure

# number of neurons in hidden layer -> mean of #input neurons + #output neurons
# step 1 make architecture powerful enough
modnn <- keras_model_sequential() %>%
  layer_dense(units = 29062, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 1)

modnn %>% compile(loss = "mse",
                   optimizer = optimizer_rmsprop(),
                   metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
# fit the model
history <- modnn %>% fit(
  train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))
?fit.keras.engine.training.Model

# plot mean absolute error of training and test data
plot(history)

# step 3 regularize architecture
modnn <- keras_model_sequential() %>%
  layer_dense(units = 29062, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn %>% compile(loss = "mse",
                   optimizer = optimizer_rmsprop(),
                   metrics = list("mean_absolute_error"))

# step 4 learning convergence
# fit the model
history <- modnn %>% fit(
  train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))
?fit.keras.engine.training.Model

# plot mean absolute error of training and test data
plot(history)

# 2) multilayer model (wide)
# step 1 make model powerful enough
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 60000, activation = "relu",
              input_shape =nccol(train_X_matrix)) %>%
  layer_dense(units = 30000, activation = "relu") %>%
  layer_dense(units = 10)

modelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
history <- modelnn %>%
    fit(train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))

# plot
plot(history, smooth = FALSE)

# step 3 reguralize architectrure
regmodelnn <- keras_model_sequential()
regmodelnn %>%
  layer_dense(units = 60000, activation = "relu",
              input_shape =nccol(train_X_matrix)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 30000, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10)

regmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 4 learning convergence 
reghistory <- regmodelnn %>%
    fit(train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))

# plot
plot(reghistory)

# 3) multilayer model (deep)
