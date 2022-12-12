# Load the caret package
library(caret)

# Load the data
data <- read.csv("my_data.csv")

# Split the data into training and test sets
set.seed(123)
train_index <- createDataPartition(data$target, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define the range of values to explore for the max_depth and eta parameters
max_depth_values <- c(3, 5, 7)
eta_values <- c(0.1, 0.3, 0.5)

# Create the grid of values to search over
param_grid <- expand.grid(max_depth = max_depth_values, eta = eta_values)

# Fit the model using 10-fold cross-validation
model <- train(target ~ ., data = train_data,
               method = "xgbTree",
               trControl = trainControl
