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
hist(test_X$car_parking_spaces)
hist(test_X$days_in_waiting_list)
hist(test_X$nr_adults)
hist(test_X$nr_children)
hist(test_X$special_requests)
hist(test_X$nr_nights)
hist(test_X$nr_previous_bookings)
hist(test_X$nr_booking_changes)

# box plots
boxplot(test_X$car_parking_spaces)
boxplot(test_X$days_in_waiting_list)
boxplot(test_X$nr_adults)
boxplot(test_X$nr_booking_changes) 
boxplot(test_X$nr_nights)
boxplot(test_X$nr_previous_bookings)
boxplot(test_X$special_requests)

boxplot(test_X$nr_previous_bookings, main = "Number of previous bookings of the client", xlab = "Number of bookings", col = "blue", border = "blue", horizontal = TRUE, notch = TRUE)

# bar charts 
barplot(test_X$car_parking_spaces)
barplot(test_X$days_in_waiting_list)
barplot(test_X$nr_adults)
barplot(test_X$nr_booking_changes) 
barplot(test_X$nr_nights)
barplot(test_X$nr_previous_bookings)
barplot(test_X$special_requests, horiz = TRUE)

# scatter plots
plot(train$nr_adults, train$average_daily_rate, main = "Average daily rate in function of number of adults")



