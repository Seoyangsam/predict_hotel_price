# here we will perform a polynomial regression 

#read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')

test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)
str(validation_X)

# OPTION 1

#polynomial regression 
poly.fit <- poly ( average_daily_rate ~ . , data = train_X_data)

#prepare data  
train_X_data$average_daily_rate <- train_X_data$x
train_X_data <- subset(train_X_data, select = -c(x,arrival_date,last_status_date))

validation_X_data$average_daily_rate <- validation_X_data$x
validation_X_data <- subset(validation_X_data, select = -c(x,arrival_date,last_status_date))

# delete arrival date and last status date from test set
test_set <- subset(test_set, select = -c(arrival_date,last_status_date))

#polynomial regression 
poly.fit1 <- lm(average_daily_rate ~ poly (train_X_data,1 ) , data = validation_X_data)
poly.fit2 <- lm(average_daily_rate ~ poly (train_X_data,2 ) , data = validation_X_data)
poly.fit3 <- lm(average_daily_rate ~ poly (train_X_data,3 ) , data = validation_X_data)
poly.fit4 <- lm(average_daily_rate ~ poly (train_X_data,4 ) , data = validation_X_data)
poly.fit5 <- lm(average_daily_rate ~ poly (train_X_data,5 ) , data = validation_X_data)
anova(poly.fit1, poly.fit2, poly.fit3, poly.fit4, poly.fit5)


# OPTION 2 (with k-fold cross validation)

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 5

#create k equal-sized folds
folds <- cut(seq(1,nrow(train_X_data)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
    
    #define training and testing data
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- train_X_data[testIndexes, ]
    trainData <- train_X_data[-testIndexes, ]
    
    #use k-fold cv to evaluate models
    for (j in 1:degree){
        fit.train = lm(average_daily_rate ~ poly(train_X_data,j), data=trainData)
        fit.test = predict(fit.train, newdata=testData)
        mse[i,j] = mean((fit.test-testData$score)^2) 
    }
}

#find MSE for each degree 
colMeans(mse)