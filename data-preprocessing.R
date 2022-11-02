#Here we perform the data preprocessing
#First we read our datas

#install.packages("readr")
install.packages('readr', dependencies = TRUE, repos='http://cran.rstudio.com/')
library(readr)

train <- read.csv(file = 'data/train.csv', header = TRUE, sep=",", fileEncoding = "latin1")
str(train)

test_X <- read.csv(file = "data/test.csv", header = TRUE, sep=",", fileEncoding = "latin1")
str(test_X)

#Next, we split the independent & dependent variables in the training set.
train_X <- subset(train, select = -c(average_daily_rate))
str(train_X)

train_y <- train$average_daily_rate
train_y <- gsub('  \u0080', '', train_y) #remove the euro sign
train_y <- as.integer(train_y) #convert from chr to int
str(train_y)

# we drop booking agent and booking company
train_X <- subset(train_X , select = -c(booking_company, booking_agent))
str(train_X)
test_X <- subset(test_X, select = -c(booking_company, booking_agent))
str(test_X)

test_X$last_status_date <- as.Date(test_X$last_status_date)

test_X$lead_time <- as.integer(sub("day(s)", "", test_X$lead_time))
train_X$lead_time <- sub("day(s)","",train_X$lead_time)

test_X$nr_booking_changes <- as.integer(test_X$nr_booking_changes)

str(train_X)
