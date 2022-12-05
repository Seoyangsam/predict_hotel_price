
# here we will perform GAM 
library(GAM)

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE)


# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)
str(validation_X)

# GAM
gam.1 <- GAM(average_daily_rate ~ s(train_X_data, df = 5) + )
gam.2 <- 
anova(gam.1,gam.2)

preds <- predict(gam.1 , newdata = validation_X_data)

gam.lr <- GAM ( I(average_daily_rate > 100) ~ train_X_data + s(train_X_data, df = 5), family = binomial, data = validation_X_data)

# data preparing 
#1
train_y_data <- subset(train_X_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(lm.fit, train_X_data)
validation_X_matrix <- model.matrix(average_daily_rate ~., data = validation_X_data)
colnames(train_X_data)
#2
train_and_validation <- rbind(train_X, validation_X)
dependant_y <- rbind(train_y, validation_y)
write.table(train_and_validation, file = "data/gold/train_and_validation.csv", sep = ",", row.names = FALSE, col.names=TRUE)

train_val_data <- data.frame(train_and_validation, dependant_y)
#3
train_val_y_data <- subset(train_val_data, select= c(average_daily_rate))

train_X_matrix <- model.matrix(lm.fit, train_val_data)
test_set_matrix <- model.matrix(~., data = test_set)