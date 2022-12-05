# here we will perform a polynomial regression 

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')

test_set <- read.csv(file = 'data/gold/test_X_scale2.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)
str(validation_X)

# OPTION 1: anova test to see which poly the best fit is for each variable 

# nr of babies 
poly_nrbabies_1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_nrbabies_2 <- lm(average_daily_rate ~ . - nr_babies + poly(nr_babies,2) , data = train_X_data)
anova(poly_nrbabies_1,poly_nrbabies_2)
    # not significant so we keep "nr_babies" of degree 1 

# lead time 
poly_leadtime1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_leadtime2 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 2) , data = train_X_data)
poly_leadtime3 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 3) , data = train_X_data)
poly_leadtime4 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 4) , data = train_X_data)
anova(poly_leadtime1,poly_leadtime2,poly_leadtime3,poly_leadtime4)
    # p-value <0,05 so significant: we take degree 2 for "lead time"

# nr of adults 
poly_nradults1 <-  lm(average_daily_rate ~ . , data = train_X_data)
poly_nradults2 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,2) , data = train_X_data)
poly_nradults3 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,3) , data = train_X_data)
poly_nradults4 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,4) , data = train_X_data)
anova(poly_nradults1,poly_nradults2,poly_nradults3,poly_nradults4)
    # p-value <0,05 so significant: we take less complex, degree 2 for "nr_adults"

# nr of children 
poly_nrchildren1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_nrchildren2 <- lm(average_daily_rate ~ . - nr_children + poly(nr_children,2) , data = train_X_data)
anova(poly_nrchildren1, poly_nrchildren2)
    # p-value <0,05 so significant: we take degree 2 for "nr_children"

# nr of nights 
poly_nrnights1 <-  lm(average_daily_rate ~ . , data = train_X_data)
poly_nrnights2 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,2), data = train_X_data)
poly_nrnights3 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,3), data = train_X_data)
poly_nrnights4 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,4), data = train_X_data)
anova(poly_nrnights1, poly_nrnights2,poly_nrnights3,poly_nrnights4)
    # p-value <0,05 so significant: we take less complex, degree 2 for "nr_nights"











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