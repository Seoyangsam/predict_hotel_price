#First we read our datas
train_X_impute <- read.csv(file = 'data/silver/train_X.csv', header = TRUE, stringsAsFactors = FALSE, fileEncoding = 'latin1')
str(train_X_impute)

test_X_impute <- read.csv(file = 'data/silver/test_X.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X_impute)

validation_X_impute <- read.csv(file = 'data/silver/validation_X.csv', header = TRUE, fileEncoding = 'latin1')
str(validation_X_impute)

#integer encoding for meal_booked, save for later
#union(unique(train_X$meal_booked), unique(test_X$meal_booked))
#meal_booked_levels <- c("meal package NOT booked", "bed & breakfast (BB)", "breakfast + one other meal // usually dinner (half board)", "full board [BREAKF -- lunch -- Dinner]") # in correct order!
#train_X$meal_booked <- as.numeric(factor(train_X$meal_booked, levels = meal_booked_levels))
#test_X$meal_booked <- as.numeric(factor(test_X$meal_booked, levels = meal_booked_levels))

library(dummy)
# get categories and dummies
cats <- categories(train_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")])
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
str(dummies_train)
dummies_train <- subset(dummies_train, select = -c(booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))

## merge with overall training set
train_X <- subset(train_X, select = -c(booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
train_X <- cbind(train_X, dummies_train)
## merge with overall test set
test_X <- subset(test_X, select = -c(booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
test_X <- cbind(test_X, dummies_test)

#convert the predictors to factors
train_X[sapply(train_X, is.character)] <- lapply(train_X[sapply(train_X, is.character)], as.factor)
str(train_X)
test_X[sapply(test_X, is.character)] <- lapply(test_X[sapply(test_X, is.character)], as.factor)
str(test_X)

#save the dataset
write.table(train_X, file = "data/silver/train_X.csv", sep = "\t", row.names = F)
write.table(test_X, file = "data/silver/test_X.csv", sep = "\t", row.names = F)