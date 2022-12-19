# DATA EXPLORATION 

install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# we read our data
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE)
test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE)

train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

test_id <- test_X$id
write.table(test_id, file = "data/bronze/test_id.csv", sep = ",", row.names = F)

# basic descriptive statistics 
summary(train)
summary(train$average_daily_rate)

names(train)
nrow(train)
ncol(train)

# mean (numeric variables)

mean(train$lead_time, na.rm = TRUE) #error
mean(train$nr_babies, na.rm = TRUE) #error

round(colMeans(train[sapply(train, is.numeric)], na.rm = TRUE),6)
#              special_requests              car_parking_spaces 
#                       0.571835                       0.062582
#           days_in_waiting_list                      nr_adults
#                       2.306868                      1.857714
#             nr_booking_changes                    nr_children
#                       1.458086                       0.103822
#                      nr_nights           nr_previous_bookings
#                       3.429912                       0.218973
# previous_bookings_not_canceled         previous_cancellations
#                       0.127316                       0.088081


# median (numeric variables)

median(train$special_requests, na.rm = TRUE) # 0
median(train$car_parking_spaces, na.rm = TRUE) # 0
median(train$days_in_waiting_list, na.rm = TRUE) # 0
median(train$nr_adults, na.rm = TRUE) # 2
median(train$nr_booking_changes, na.rm = TRUE) # 1
median(train$nr_children, na.rm = TRUE) # 0
median(train$nr_nights, na.rm = TRUE) # 3
median(train$nr_previous_bookings, na.rm = TRUE) # 0
median(train$previous_bookings_not_canceled, na.rm = TRUE) # 0
median(train$previous_cancellations, na.rm = TRUE) # 0
median(train$nr_babies, na.rm = TRUE) # na
median(train$lead_time, na.rm = TRUE) # 254


# modus (categorical variables)

# Create the function.
getMode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

getMode(train$assigned_room_type) # "A"
getMode(train$reserved_room_type) # "A"
getMode(train$arrival_date) # "December  5  2015"
getMode(train$booking_agent) # "9"
getMode(train$booking_company) # "NULL"
getMode(train$booking_distribution_channel) # "TA/TO"
getMode(train$canceled) # "no cancellation"
getMode(train$country) # "Portugal"
getMode(train$customer_type) # "Transient"
getMode(train$deposit) # "nodeposit"
getMode(train$hotel_type) # "City Hotel"
getMode(train$is_repeated_guest) # "no"
getMode(train$last_status) # "Check-Out"
getMode(train$last_status_date) # NA
getMode(train$market_segment) # "Online travel agent"
getMode(train$meal_booked) # "bed & breakfast (BB)"
getMode(train$average_daily_rate) # "62 \ 200"


# correlation 
c <- select(test_X, c(previous_cancellations, nr_previous_bookings, nr_booking_changes, previous_bookings_not_canceled))
correlation <- cor(c)
round(correlation,2)

c <- select(test_X_cleaned, c(previous_cancellations, nr_previous_bookings))
correlation <- cor(c)
round(correlation,2)

# histograms 
hist(train$car_parking_spaces)
hist(train$days_in_waiting_list)
hist(train$nr_adults)
hist(train$nr_children)
hist(train$special_requests)
hist(train$nr_nights)
hist(train$nr_previous_bookings)
hist(train$nr_booking_changes)
# hist(train$average_daily_rate)
hist(train_y$average_daily_rate, col = "darkseagreen2", main = " ", xlab = "Average daily rate", ylab = "Frequency")

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

df <- data.frame(reservedRoomType = c(train$reserved_room_type),
                assigned = c(train$assigned_room_type),
                customerType = c(train$customer_type),
                arrivalDate = c(train$arrival_date),
                bookingAgent = c(train$booking_agent),
                bookingCompany = c(train$booking_company),
                distributionChannel = c(train$booking_distribution_channel),
                canceled = c(train$canceled),
)
        
ggplot(df, aes(x = Assigned_room_type))
geom_bar()

#create data frame
df <- data.frame(result = c('W', 'L', 'W', 'W', 'W', 'L', 'W', 'L', 'W', 'L'),
                 team = c('B', 'B', 'B', 'B', 'D', 'A', 'A', 'A', 'C', 'C'),
                 points = c(12, 28, 19, 22, 32, 45, 22, 28, 13, 19),
                 rebounds = c(5, 7, 7, 12, 11, 4, 10, 7, 8, 8))

#create bar chart of teams
ggplot(df, aes(x=team)) +
  geom_bar()

# scatter plots
plot(train$nr_adults, train$average_daily_rate, main = "Average daily rate in function of number of adults")

