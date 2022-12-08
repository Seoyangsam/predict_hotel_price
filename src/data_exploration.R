# DATA EXPLORATION 

# we read our data
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE)
str(train)

test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE)
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
hist(test_X$car_parking_spaces)
hist(test_X$days_in_waiting_list)
hist(test_X$nr_adults)
hist(test_X$nr_children)
hist(test_X$special_requests)
hist(test_X$nr_nights)
hist(test_X$nr_previous_bookings)
hist(test_X$nr_booking_changes)

# box plots
boxplot(train$car_parking_spaces)
boxplot(train$days_in_waiting_list)
boxplot(train$nr_adults)
boxplot(train$nr_adults)
boxplot(train$nr_booking_changes) 
boxplot(train$nr_nights)
boxplot(train$nr_previous_bookings)
boxplot(train$special_requests)

boxplot(train$nr_previous_bookings, main = "Number of previous bookings of the client", xlab = "Number of bookings", col = "blue", border = "blue", horizontal = TRUE, notch = TRUE)

# bar charts 
barplot(train$car_parking_spaces)
barplot(train$days_in_waiting_list)
barplot(train$nr_adults)
barplot(train$nr_booking_changes) 
barplot(train$nr_nights)
barplot(train$nr_previous_bookings)
barplot(train$special_requests, horiz = TRUE)

# scatter plots
plot(train$nr_adults, train$average_daily_rate, main = "Average daily rate in function of number of adults")

