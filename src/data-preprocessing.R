#Here we perform the data preprocessing
#First we read our datas
#install.packages("readr")
#install.packages('readr', dependencies = TRUE, repos = 'http://cran.rstudio.com/')
#library(readr)
#library(tidyverse)
#train <- read_csv(file = 'data/bronze/train.csv')
#train <- read.csv(file = 'data/bronze/train.csv', header = TRUE, fileEncoding = 'latin1')


train <- read.csv(file = 'data/bronze/train.csv')
str(train)


test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X)



#Next, we split the independent & dependent variables in the training set.
train_X <- subset(train, select = -c(average_daily_rate))
str(train_X)

train_y <- train$average_daily_rate
train_y <- gsub('  \u0080','',train_y) #remove the euro sign
train_y <- as.integer(train_y) #convert from chr to int
str(train_y)

# we drop booking agent and booking company
train_X <- subset(train_X , select = -c(booking_company, booking_agent))
str(train_X)
test_X <- subset(test_X, select = -c(booking_company, booking_agent))
str(test_X)

train_X_impute <- train_X
test_X_impute <- test_X

colMeans(is.na(train_X_impute))
colMeans(is.na(test_X_impute))
