#First we read our datas
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
str(train_X_cleaned)
 
test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X_cleaned)

validation_X_cleaned <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE, fileEncoding = 'latin1')
str(validation_X_cleaned)

train_X_ft_engineering <- train_X_cleaned
test_X_ft_engineering <- test_X_cleaned
validation_X_ft_engineering <- validation_X_cleaned

#integer encoding for meal_booked, save for later
#union(unique(train_X$meal_booked), unique(test_X$meal_booked))
#meal_booked_levels <- c("meal package NOT booked", "bed & breakfast (BB)", "breakfast + one other meal // usually dinner (half board)", "full board [BREAKF -- lunch -- Dinner]") # in correct order!
#train_X$meal_booked <- as.numeric(factor(train_X$meal_booked, levels = meal_booked_levels))
#test_X$meal_booked <- as.numeric(factor(test_X$meal_booked, levels = meal_booked_levels))


library(dummy)
# get categories and dummies
cats <- categories(train_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")])
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
str(dummies_train)
dummies_train <- subset(dummies_train, select = -c(assigned_room_type_P, reserved_room_type_P, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(assigned_room_type_P, reserved_room_type_P, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))
# apply on validation set (exclude reference categories)
dummies_validation <- dummy(validation_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
dummies_validation <- subset(dummies_validation, select = -c(assigned_room_type_P, reserved_room_type_P, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))

#for country we select a subset of top 20 countries with highest frequency
cats2 <- categories(train_X_ft_engineering[, c("country")], p = 20)
# apply on train set (exclude reference categories)
dummies_train2 <- dummy(train_X_ft_engineering[, c("country")],
                       object = cats)
dummies_train2 <- subset(dummies_train2, select = -c(country_Portugal))
# apply on test set (exclude reference categories) and validation set
dummies_test2 <- dummy(test_X_ft_engineering[, c("country")], object = cats)
dummies_test2 <- subset(dummies_test2, select = -c(country_Portugal))

dummies_validation2 <- dummy(validation_X_ft_engineering[, c("country")], object = cats)
dummies_validation2 <- subset(dummies_validation2, select = -c(country_Portugal))

## merge with overall training set
train_X_ft_engineering <- subset(train_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
train_X_ft_engineering <- cbind(train_X_ft_engineering, dummies_train)
## merge with overall test set
test_X_ft_engineering <- subset(test_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
test_X_ft_engineering <- cbind(test_X_ft_engineering, dummies_test)
## merge with overall validation set
validation_X_ft_engineering <- subset(validation_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
validation_X_ft_engineering <- cbind(validation_X_ft_engineering, dummies_train)

#save the dataset
write.table(train_X, file = "data/silver/train_X.csv", sep = "\t", row.names = F)
write.table(test_X, file = "data/silver/test_X.csv", sep = "\t", row.names = F)