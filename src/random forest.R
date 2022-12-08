library(randomForest)
library(dummy)
set.seed(1)
# Read files
train_X <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')

train_X_data <- data.frame(train_X,train_y)
str(train_X_data)

# Kepp top 15 values for country
cat <- categories(train_X_data[("country")], p = 15)
shouldBecomeOther<-!(train_X_data$country %in% c("Portugal", "United Kingdom", "France", "Spain", "Germany", "Italy", "Ireland","Belgium" , "Brazil", "United States", "Netherlands", "Switzerland", "Austria", "Sweden"))
train_X_data$country[shouldBecomeOther]<- "other"
str(train_X_data$country)
unique(train_X_data$country)
shouldBecomeOther

cat <- categories(validation_X[("country")], p = 15)
prin
print(validation_X$country)

# Convert all columns to factor
train_X_data <- as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE)
validation_X <-as.data.frame(unclass(train_X_data), stringsAsFactors = TRUE,levels = levels(train_X_data))

library(caret)
library(e1071)
control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3)

metric <- "Accuracy"
set.seed(123)
mtry <- sqrt(ncol(train_X))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(average_daily_rate~.,
                      data=train_X_data,
                      method='rf',
                      metric='Accuracy',
                      tuneGrid=tunegrid,
                      trControl=control)
print(rf_default)

sapply(lapply(train_X_data, unique), length)
str(train_X_data)

# Random forest
rf.train <- randomForest(average_daily_rate ~ ., data = train_X_data, mtry = 7, importance = TRUE, ntree = 15)
rf.train
yhat.rf <- predict(rf.train, newdata = validation_X)
mean((yhat.rf - validation_y$average_daily_rate)^2)

