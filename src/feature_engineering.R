#First we read our datas
train_X_cleaned <- read.csv(file = 'data/silver/train_X_cleaned.csv', header = TRUE)
str(train_X_cleaned)

test_X_cleaned <- read.csv(file = 'data/silver/test_X_cleaned.csv', header = TRUE)
str(test_X_cleaned)

validation_X_cleaned <- read.csv(file = 'data/silver/validation_X_cleaned.csv', header = TRUE)
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
cats <- categories(train_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "country",  "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")], p=20)
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "country", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
str(dummies_train)
dummies_train <- subset(dummies_train, select = -c(assigned_room_type_A, reserved_room_type_A, country_Portugal, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "country", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(assigned_room_type_A, reserved_room_type_A, country_Portugal, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))
# apply on validation set (exclude reference categories)
dummies_validation <- dummy(validation_X_ft_engineering[, c("assigned_room_type", "reserved_room_type", "country", "booking_distribution_channel", "customer_type", "last_status", "market_segment", "meal_booked")],
                       object = cats)
dummies_validation <- subset(dummies_validation, select = -c(assigned_room_type_A, reserved_room_type_A, country_Portugal, booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation, meal_booked_bed...breakfast..BB.))

## merge with overall training set
train_X_ft_engineering <- subset(train_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, country, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
train_X_ft_engineering <- cbind(train_X_ft_engineering, dummies_train)
## merge with overall test set
test_X_ft_engineering <- subset(test_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, country, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
test_X_ft_engineering <- cbind(test_X_ft_engineering, dummies_test)
## merge with overall validation set
validation_X_ft_engineering <- subset(validation_X_ft_engineering, select = -c(assigned_room_type, reserved_room_type, country, booking_distribution_channel, customer_type, last_status, market_segment, meal_booked))
validation_X_ft_engineering <- cbind(validation_X_ft_engineering, dummies_validation)

#convert the predictors to factors
#train_X_ft_engineering[sapply(train_X_ft_engineering, is.character)] <- lapply(train_X_ft_engineering[sapply(train_X_ft_engineering, is.character)], as.factor)
#str(train_X_ft_engineering)
#test_X_ft_engineering[sapply(test_X_ft_engineering, is.character)] <- lapply(test_X_ft_engineering[sapply(test_X_ft_engineering, is.character)], as.factor)
#str(test_X_ft_engineering)
#validation_X_ft_engineering[sapply(validation_X_ft_engineering, is.character)] <- lapply(validation_X_ft_engineering[sapply(validation_X_ft_engineering, is.character)], as.factor)
#str(validation_X_ft_engineering)

# create new dataframes to avoid overwriting the existing dataframes
train_X_scale <- train_X_ft_engineering
test_X_scale <- test_X_ft_engineering
validation_X_scale <- validation_X_ft_engineering

# get all numeric columns for scaling
scale_cols <- c("days_in_waiting_list", "lead_time","nr_adults","nr_babies","nr_booking_changes","nr_children","nr_nights","nr_previous_bookings","previous_bookings_not_canceled","previous_cancellations","special_requests")

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
