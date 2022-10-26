#Here we perform the data preprocessing

#First we read our datas

train <- read.csv(file = 'data/train.csv', header = TRUE, fileEncoding = 'latin1')
str(train)
head(train)

test <- read.csv(file = 'data/test.csv', header = TRUE, fileEncoding = 'latin1')
head(test)

