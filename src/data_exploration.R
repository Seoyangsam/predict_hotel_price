# DATA EXPLORATION 

# we read our data
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE,fileEncoding = 'latin1' )
str(train)

test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X)

test_id <- test_X$id
write.table(test_id, file = "data/bronze/test_id.csv", sep = ",", row.names = F)

# basic descriptive statistics 
summary(train)
summary(train$average_daily_rate)

names(train)
nrow(train)
ncol(train)

# histograms 
average_daily_rate <- train$average_daily_rate
parkingspaces <- train$


