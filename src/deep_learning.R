# read data
train_X <- read.csv(file = 'data/gold/train_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')


# installation
install.packages("reticulate")
install.packages("keras")
install.packages("tensorflow")
install.packages("ggplot2")

library(reticulate)
library(keras)
library(tensorflow)
library(ggplot2)

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
train_X_matrix <- model.matrix(average_daily_rate ~. -1 , data = train_X_data)
#train_X_array <- array_reshape(train_X, dim = dim(train_X))
train_Y <- train_X_data$average_daily_rate 
#train_Y_array <- array_reshape(train_y, dim = dim(train_y))
validation_X_matrix <- model.matrix(average_daily_rate ~. -1, data = validation_X_data)
#validation_X_array <- array_reshape(validation_X,dim = dim(validation_X))
validation_Y <- validation_X_data$average_daily_rate
#validation_Y_array <- array_reshape(validation_y,dim = dim(validation_y))
str(train_X)
# 1) single layer model structure
# number of neurons in hidden layer -> mean of #input neurons + #output neurons
# step 1 make architecture powerful enough
modnn <- keras_model_sequential() %>%
  layer_dense(units = 5000, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 1)
summary(modnn)
modnn %>% compile(loss = "mse",
                   optimizer = optimizer_rmsprop(),
                   metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
# fit the model
history <- modnn %>% fit(
  train_X_matrix, train_Y, epochs =100 , batch_size = 600,
  validation_data = list(validation_X_matrix,validation_Y))


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
  train_X_matrix, train_Y, epochs = 100, batch_size = 1000,
  validation_data = list(validation_X_matrix,validation_Y))
?fit.keras.engine.training.Model

# plot mean absolute error of training and test data
plot(history)

# 2) multilayer model (wide)
# step 1 make model powerful enough
widemodelnn <- keras_model_sequential()
widemodelnn %>%
  layer_dense(units = 30000, activation = "relu",
              input_shape = ncol(train_X_matrix)) %>%
  layer_dense(units = 15000, activation = "relu") %>%
  layer_dense(units = 1)

widemodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
widehistory <- widemodelnn %>%
    fit(train_X_matrix, train_Y, epochs = 100, batch_size = 800,
  validation_data = list(validation_X_matrix,validation_Y))

# plot
plot(widehistory)

# step 3 reguralize architectrure
regwidemodelnn <- keras_model_sequential()
regwidemodelnn %>%
  layer_dense(units = 60000, activation = "relu",
              input_shape =nccol(train_X_matrix)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 30000, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)

regwidemodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 4 learning convergence 
regwidehistory <- regwidemodelnn %>%
    fit(train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))

# plot
plot(regwidehistory)

# 3) multilayer model (deep)

# step 1 make model powerful enough
deepmodelnn <- keras_model_sequential()
deepmodelnn %>%
  layer_dense(units = 50000, activation = "relu",
              input_shape =nccol(train_X_matrix)) %>%
  layer_dense(units = 25000, activation = "relu") %>%
  layer_dense(units = 12500, activation = "relu") %>%
  layer_dense(units = 6250, activation = "relu") %>%
  layer_dense(units = 1)

deepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 2 learning convergence (#epochs and batch size)
deephistory <- deepmodelnn %>%
    fit(train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))

# plot
plot(deephistory)

# step 3 reguralize architectrure
regdeepmodelnn <- keras_model_sequential()
regdeepmodelnn %>%
  layer_dense(units = 50000, activation = "relu",
              input_shape =nccol(train_X_matrix)) %>%
  layer_dense(units = 25000, activation = "relu") %>%
  layer_dense(units = 12500, activation = "relu") %>%
  layer_dense(units = 6250, activation = "relu") %>%
  layer_dense(units = 1)

regdeepmodelnn %>% compile(loss = "mse",
                    optimizer = optimizer_rmsprop(),
                    metrics = list("mean_absolute_error"))

# step 4 learning convergence 
regdeephistory <- regdeepmodelnn %>%
    fit(train_X_data[-average_daily_rate,], train_X_data[average_daily_rate,], epochs = 300, batch_size = 500,
  validation_data = list(validation_X_data[-average_daily_rate,], validation_X_data[average_daily_rate,]))

# plot
plot(regdeephistory)