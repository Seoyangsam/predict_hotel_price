#First we read our datas
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE)
str(train_X_cleaned)

test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE)
str(test_X_cleaned)

validation_X_cleaned <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE)
str(validation_X_cleaned)

# new names for dataframes to avoid confusion
train_X_ft_engineering <- train_X_cleaned
test_X_ft_engineering <- test_X_cleaned
validation_X_ft_engineering <- validation_X_cleaned


library(dummy)
# get categories and dummies
cats <- categories(train_X_ft_engineering[, c("assigned_room_type","customer_type", "last_status", "market_segment", "meal_booked", "month_arrival_date", "day_arrival_date")], p=15)
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "meal_booked", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_train <- subset(dummies_train, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB. , month_arrival_date_January, day_arrival_date_monday))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "meal_booked", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB., month_arrival_date_January, day_arrival_date_monday))
# apply on validation set (exclude reference categories)
dummies_validation <- dummy(validation_X_ft_engineering[, c("assigned_room_type", "customer_type", "last_status", "market_segment", "meal_booked", "month_arrival_date", "day_arrival_date")],
                       object = cats)
dummies_validation <- subset(dummies_validation, select = -c(assigned_room_type_A, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB., month_arrival_date_January, day_arrival_date_monday))

## merge with overall training set
train_X_ft_engineering <- subset(train_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, meal_booked, month_arrival_date, day_arrival_date))
train_X_ft_engineering <- cbind(train_X_ft_engineering, dummies_train)
## merge with overall test set
test_X_ft_engineering <- subset(test_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, meal_booked, month_arrival_date, day_arrival_date))
test_X_ft_engineering <- cbind(test_X_ft_engineering, dummies_test)
## merge with overall validation set
validation_X_ft_engineering <- subset(validation_X_ft_engineering, select = -c(assigned_room_type, customer_type, last_status, market_segment, meal_booked, month_arrival_date, day_arrival_date))
validation_X_ft_engineering <- cbind(validation_X_ft_engineering, dummies_validation)


# create new dataframes to avoid overwriting the existing dataframes
train_X_scale <- train_X_ft_engineering
test_X_scale <- test_X_ft_engineering
validation_X_scale <- validation_X_ft_engineering

# get all numeric columns for scaling
scale_cols <- c("car_parking_spaces","lead_time","nr_adults","nr_children","nr_nights","special_requests", "nr_previous_bookings", "previous_cancellations")

# apply on training set
mean_train <- colMeans(train_X_scale[, scale_cols])
sd_train <- sapply(train_X_scale[, scale_cols], sd)
train_X_scale[, scale_cols] <- scale(train_X_scale[, scale_cols], center = TRUE, scale = TRUE)

# apply on test set
test_X_scale[, scale_cols] <- scale(test_X_scale[, scale_cols], center = mean_train, scale = sd_train)

# apply on validation set
validation_X_scale[, scale_cols] <- scale(validation_X_scale[, scale_cols], center = mean_train, scale = sd_train)

# now, we check the distributions
colMeans(train_X_scale[, scale_cols])
sapply(train_X_scale[, scale_cols], sd)

colMeans(test_X_scale[, scale_cols])
sapply(test_X_scale[, scale_cols], sd)

colMeans(validation_X_scale[, scale_cols])
sapply(validation_X_scale[, scale_cols], sd)

#save the dataset
write.table(train_X_scale, file = "data/gold/train_X_scale.csv", sep = ",", row.names = F)
write.table(test_X_scale, file = "data/gold/test_X_scale.csv", sep = ",", row.names = F)
write.table(validation_X_scale, file = "data/gold/validation_X_scale.csv", sep = ",", row.names = F)
