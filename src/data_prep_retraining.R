#First we read our datas
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE, fileEncoding = 'latin1')
str(train)

test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X)

test_id <- test_X$id
write.table(test_id, file = "data/bronze/test_id.csv", sep = ",", row.names = F)

# Kepp top 15 values for country
library(dummy)
install.packages("dummy")
cat <- categories(train[("country")], p = 14)
shouldBecomeOther<-!(train$country %in% c("Portugal", "United Kingdom", "France", "Spain", "Germany", "Italy", "Ireland","Belgium" , "Brazil", "United States", "Netherlands", "Switzerland", "Austria", "Sweden"))
train$country[shouldBecomeOther]<- "other"
unique(train$country)
shouldBecomeOther<-!(test_X$country %in% c("Portugal", "United Kingdom", "France", "Spain", "Germany", "Italy", "Ireland","Belgium" , "Brazil", "United States", "Netherlands", "Switzerland", "Austria", "Sweden"))
test_X$country[shouldBecomeOther]<- "other"

# Create a validation set out of the training set
#set.seed(1)
#sample_size <- floor(0.30 * nrow(train))
#validation_ind <- sample(nrow(train), sample_size, replace = FALSE)
#validation <- train[validation_ind,]
#train <- train[-validation_ind,]

#Next, we split the independent & dependent variables in the training set.
train_X <- subset(train, select = -c(average_daily_rate))
str(train_X)


train_y <- train$average_daily_rate
train_y <- gsub(' .*','',train_y) #remove the euro sign
train_y <- as.double(train_y) #convert from chr to float
str(train_y)

# we drop id, booking agent and booking company
#new: drop nr booking changes, days in waiting list
train_X <- subset(train_X , select = -c(id, booking_company, booking_agent, nr_booking_changes, days_in_waiting_list, nr_babies, country, booking_distribution_channel))
str(train_X)
test_X <- subset(test_X, select = -c(id, booking_company, booking_agent, nr_booking_changes, days_in_waiting_list, nr_babies, country, booking_distribution_channel))
str(test_X)

# missing values
colMeans(is.na(test_X))
colMeans(is.na(train_X))

# create new dataframes to avoid overwriting the existing dataframes
train_X_impute <- train_X
test_X_impute <- test_X

# Impute function
impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}

# calculate mode of list
modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# flag function for NA values
naFlag <- function(df, df_val = NULL) {
  if (is.null(df_val)) {
    df_val <- df
  }
  mask <- sapply(df_val, anyNA)
  out <- lapply(df[mask], function(x)as.numeric(is.na(x)))
  if (length(out) > 0) names(out) <- paste0(names(out), "_flag")
  return(as.data.frame(out))
}

# impute missing values
# impute all categorical variables

train_X_impute$customer_type <- impute(train_X_impute$customer_type, method = modus)
test_X_impute$customer_type <- impute(test_X_impute$customer_type, val = modus(train_X_impute$customer_type, na.rm = T))

train_X_impute$hotel_type <- impute(train_X_impute$hotel_type, method = modus)
test_X_impute$hotel_type <- impute(test_X_impute$hotel_type, val = modus(train_X_impute$hotel_type, na.rm = T))

train_X_impute$last_status<-ifelse(is.na(train_X_impute$last_status)==TRUE & train_X_impute$canceled=="stay cancelled", "Canceled", train_X_impute$last_status)
train_X_impute$last_status<-ifelse(is.na(train_X_impute$last_status)==TRUE & train_X_impute$canceled=="no cancellation", "Check-Out",train_X_impute$last_status)

test_X_impute$last_status<-ifelse(is.na(test_X_impute$last_status)==TRUE & test_X_impute$canceled=="stay cancelled", "Canceled", test_X_impute$last_status)
test_X_impute$last_status<-ifelse(is.na(test_X_impute$last_status)==TRUE & test_X_impute$canceled=="no cancellation", "Check-Out",test_X_impute$last_status)

# !!!
# last status and canceled provide the same information so drop canceled
train_X_impute <- subset(train_X_impute , select = -c(canceled))
test_X_impute <- subset(test_X_impute , select = -c(canceled))
str(train_X_impute)
# !!!
# new column to check whether reserved room type and assigned room type are the same
train_X_impute$desired_room <- ifelse(train_X_impute$assigned_room_type == train_X_impute$reserved_room_type,1,0)
test_X_impute$desired_room <- ifelse(test_X_impute$assigned_room_type == test_X_impute$reserved_room_type,1,0)
str(train_X_impute)
# !!!
# drop reserved_room_type
train_X_impute <- subset(train_X_impute , select = -c(reserved_room_type))
test_X_impute <- subset(test_X_impute , select = -c(reserved_room_type))
str(train_X_impute)

train_X_impute$market_segment <- impute(train_X_impute$market_segment, method = modus)
test_X_impute$market_segment <- impute(test_X_impute$market_segment, val = modus(train_X_impute$market_segment, na.rm = T))

# string to date object

#library(tidyverse)
install.packages("lubridate")
install.packages("nycflights13")
library(lubridate)
library(nycflights13)

train_X_impute$arrival_date <- mdy(train_X_impute$arrival_date)
test_X_impute$arrival_date <- mdy(test_X_impute$arrival_date)
#devtools::install_github("tidyverse/tidyverse")

library(anytime)
install.packages("anytime")

# impute missing values for last status date as arrival date + nr nights
# niet meer nodig(ook veranderen in data cleaning)


#make columns with week, year and day for arrival date and last status date
#train_X_impute$year_arrival_date <- format(train_X_impute$arrival_date, format="%Y")
train_X_impute$month_arrival_date <- format(train_X_impute$arrival_date, format="%m")

#test_X_impute$year_arrival_date <- format(test_X_impute$arrival_date, format="%Y")
test_X_impute$month_arrival_date <- format(test_X_impute$arrival_date, format="%m")


#train_X_impute$year_last_status_date <- format(train_X_impute$last_status_date, format="%Y")
#train_X_impute$month_last_status_date <- format(train_X_impute$last_status_date, format="%m")

#test_X_impute$year_last_status_date <- format(test_X_impute$last_status_date, format="%Y")
#test_X_impute$month_last_status_date <- format(test_X_impute$last_status_date, format="%m")

#validation_X_impute$year_last_status_date <- format(validation_X_impute$last_status_date, format="%Y")
#validation_X_impute$month_last_status_date <- format(validation_X_impute$last_status_date, format="%m")

train_X_impute$day_arrival_date <- as.POSIXlt(train_X_impute$arrival_date)$wday
test_X_impute$day_arrival_date <- as.POSIXlt(test_X_impute$arrival_date)$wday

#train_X_impute$day_last_status_date <- as.POSIXlt(train_X_impute$last_status_date)$wday
#test_X_impute$day_last_status_date <- as.POSIXlt(test_X_impute$last_status_date)$wday
#validation_X_impute$day_last_status_date <- as.POSIXlt(validation_X_impute$last_status_date)$wday

# days

train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 2] <- "monday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 3] <- "tuesday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 4] <- "wednesday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 5] <- "thursday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 6] <- "friday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 0] <- "saturday"
train_X_impute$day_arrival_date[train_X_impute$day_arrival_date == 1] <- "sunday"

test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 2] <- "monday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 3] <- "tuesday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 4] <- "wednesday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 5] <- "thursday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 6] <- "friday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 0] <- "saturday"
test_X_impute$day_arrival_date[test_X_impute$day_arrival_date == 1] <- "sunday"


#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 2] <- "monday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 3] <- "tuesday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 4] <- "wednesday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 5] <- "thursday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 6] <- "friday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 0] <- "saturday"
#train_X_impute$day_last_status_date[train_X_impute$day_last_status_date == 1] <- "sunday"

#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 2] <- "monday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 3] <- "tuesday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 4] <- "wednesday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 5] <- "thursday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 6] <- "friday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 0] <- "saturday"
#test_X_impute$day_last_status_date[test_X_impute$day_last_status_date == 1] <- "sunday"


# months

train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "01"] <- "January"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "02"] <- "February"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "03"] <- "March"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "04"] <- "April"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "05"] <- "May"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "06"] <- "June"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "07"] <- "July"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "08"] <- "August"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "09"] <- "September"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "10"] <- "October"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "11"] <- "November"
train_X_impute$month_arrival_date[train_X_impute$month_arrival_date == "12"] <- "December"

test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "01"] <- "January"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "02"] <- "February"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "03"] <- "March"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "04"] <- "April"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "05"] <- "May"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "06"] <- "June"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "07"] <- "July"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "08"] <- "August"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "09"] <- "September"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "10"] <- "October"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "11"] <- "November"
test_X_impute$month_arrival_date[test_X_impute$month_arrival_date == "12"] <- "December"

#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "01"] <- "January"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "02"] <- "February"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "03"] <- "March"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "04"] <- "April"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "05"] <- "May"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "06"] <- "June"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "07"] <- "July"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "08"] <- "August"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "09"] <- "September"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "10"] <- "October"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "11"] <- "November"
#train_X_impute$month_last_status_date[train_X_impute$month_last_status_date == "12"] <- "December"

#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "01"] <- "January"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "02"] <- "February"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "03"] <- "March"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "04"] <- "April"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "05"] <- "May"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "06"] <- "June"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "07"] <- "July"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "08"] <- "August"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "09"] <- "September"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "10"] <- "October"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "11"] <- "November"
#test_X_impute$month_last_status_date[test_X_impute$month_last_status_date == "12"] <- "December"

#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "01"] <- "January"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "02"] <- "February"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "03"] <- "March"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "04"] <- "April"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "05"] <- "May"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "06"] <- "June"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "07"] <- "July"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "08"] <- "August"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "09"] <- "September"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "10"] <- "October"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "11"] <- "November"
#validation_X_impute$month_last_status_date[validation_X_impute$month_last_status_date == "12"] <- "December"

#train_X_impute$year_arrival_date[train_X_impute$year_arrival_date == 2015] <- "year_2015"
#train_X_impute$year_arrival_date[train_X_impute$year_arrival_date == 2016] <- "year_2016"
#train_X_impute$year_arrival_date[train_X_impute$year_arrival_date == 2017] <- "year_2017"

#test_X_impute$year_arrival_date[test_X_impute$year_arrival_date == 2015] <- "year_2015"
#test_X_impute$year_arrival_date[test_X_impute$year_arrival_date == 2016] <- "year_2016"
#test_X_impute$year_arrival_date[test_X_impute$year_arrival_date == 2017] <- "year_2017"

#validation_X_impute$year_arrival_date[validation_X_impute$year_arrival_date == 2015] <- "year_2015"
#validation_X_impute$year_arrival_date[validation_X_impute$year_arrival_date == 2016] <- "year_2016"
#validation_X_impute$year_arrival_date[validation_X_impute$year_arrival_date == 2016] <- "year_2017"

#train_X_impute$year_last_status_date[train_X_impute$year_last_status_date == 2015] <- "year_2015"
#train_X_impute$year_last_status_date[train_X_impute$year_last_status_date == 2016] <- "year_2016"
#train_X_impute$year_last_status_date[train_X_impute$year_last_status_date == 2017] <- "year_2017"

#test_X_impute$year_last_status_date[test_X_impute$year_last_status_date == 2015] <- "year 2015"
#test_X_impute$year_last_status_date[test_X_impute$year_last_status_date == 2016] <- "year 2016"
#test_X_impute$year_last_status_date[test_X_impute$year_last_status_date == 2017] <- "year 2017"

#validation_X_impute$year_last_status_date[validation_X_impute$year_last_status_date == 2015] <- "year 2015"
#validation_X_impute$year_last_status_date[validation_X_impute$year_last_status_date == 2016] <- "year 2016"
#validation_X_impute$year_last_status_date[validation_X_impute$year_last_status_date == 2017] <- "year 2017"




# !!!
# drop arrival date and last status date
train_X_impute <- subset(train_X_impute , select = -c(arrival_date))
test_X_impute <- subset(test_X_impute , select = -c(arrival_date))

train_X_impute <- subset(train_X_impute , select = -c(last_status_date))
test_X_impute <- subset(test_X_impute , select = -c(last_status_date))


# impute all numerical variables
train_X_impute$car_parking_spaces <- impute(train_X_impute$car_parking_spaces, method = median)
test_X_impute$car_parking_spaces <- impute(test_X_impute$car_parking_spaces, val = median(train_X_impute$car_parking_spaces, na.rm = T))

train_X_impute$nr_adults <- impute(train_X_impute$nr_adults, method = median)
test_X_impute$nr_adults <- impute(test_X_impute$nr_adults, val = median(train_X_impute$nr_adults, na.rm = T))

train_X_impute$nr_children <- impute(train_X_impute$nr_children, method = median)
test_X_impute$nr_children <- impute(test_X_impute$nr_children, val = median(train_X_impute$nr_children, na.rm = T))

# !!!
# code to determine nr previous bookings accurately based on previous cancellations and previous bookings not canceled
train_X_impute$nr_previous_bookings <- ifelse(is.na(train_X_impute$nr_previous_bookings) == TRUE & is.na(train_X_impute$previous_bookings_not_canceled) == FALSE & is.na(train_X_impute$previous_cancellations) == FALSE, train_X_impute$previous_bookings_not_canceled + train_X_impute$previous_cancellations, train_X_impute$nr_previous_bookings)
test_X_impute$nr_previous_bookings <- ifelse(is.na(test_X_impute$nr_previous_bookings) == TRUE & is.na(test_X_impute$previous_bookings_not_canceled) == FALSE & is.na(test_X_impute$previous_cancellations) == FALSE, test_X_impute$previous_bookings_not_canceled + test_X_impute$previous_cancellations, test_X_impute$nr_previous_bookings)

train_X_impute$previous_bookings_not_canceled <- ifelse(is.na(train_X_impute$previous_bookings_not_canceled) == TRUE & is.na(train_X_impute$nr_previous_bookings) == FALSE & is.na(train_X_impute$previous_cancellations) == FALSE, train_X_impute$nr_previous_bookings - train_X_impute$previous_cancellations, train_X_impute$previous_bookings_not_canceled)
test_X_impute$previous_bookings_not_canceled <- ifelse(is.na(test_X_impute$previous_bookings_not_canceled) == TRUE & is.na(test_X_impute$nr_previous_bookings) == FALSE & is.na(test_X_impute$previous_cancellations) == FALSE, test_X_impute$nr_previous_bookings - test_X_impute$previous_cancellations, test_X_impute$previous_bookings_not_canceled)

train_X_impute$previous_cancellations <- ifelse(is.na(train_X_impute$previous_cancellations) == TRUE & is.na(train_X_impute$nr_previous_bookings) == FALSE & is.na(train_X_impute$previous_bookings_not_canceled) == FALSE, train_X_impute$nr_previous_bookings - train_X_impute$previous_bookings_not_canceled, train_X_impute$previous_cancellations)
test_X_impute$previous_cancellations <- ifelse(is.na(test_X_impute$previous_cancellations) == TRUE & is.na(test_X_impute$nr_previous_bookings) == FALSE & is.na(test_X_impute$previous_bookings_not_canceled) == FALSE, test_X_impute$nr_previous_bookings - test_X_impute$previous_bookings_not_canceled, test_X_impute$previous_cancellations)

# if we were unable to calculate it, use median
train_X_impute$nr_previous_bookings <- impute(train_X_impute$nr_previous_bookings, method = median)
test_X_impute$nr_previous_bookings <- impute(test_X_impute$nr_previous_bookings, val = median(train_X_impute$nr_previous_bookings, na.rm = T))

train_X_impute$previous_bookings_not_canceled <- impute(train_X_impute$previous_bookings_not_canceled, method = median)
test_X_impute$previous_bookings_not_canceled <- impute(test_X_impute$previous_bookings_not_canceled, val = median(train_X_impute$previous_bookings_not_canceled, na.rm = T))

train_X_impute$previous_cancellations <- impute(train_X_impute$previous_cancellations, method = median)
test_X_impute$previous_cancellations <- impute(test_X_impute$previous_cancellations, val = median(train_X_impute$previous_cancellations, na.rm = T))

# !!!
# drop previous_bookings_not_canceled
train_X_impute <- subset(train_X_impute , select = -c(previous_bookings_not_canceled))
test_X_impute <- subset(test_X_impute , select = -c(previous_bookings_not_canceled))

# Change lead time to integer to calculate mean
train_X_impute$lead_time <- gsub("[  day(s)]",'',train_X_impute$lead_time)
train_X_impute$lead_time <-as.integer(train_X_impute$lead_time)

test_X_impute$lead_time <- gsub("[  day(s)]",'',test_X_impute$lead_time)
test_X_impute$lead_time <-as.integer(test_X_impute$lead_time)

train_X_impute$lead_time <- impute(train_X_impute$lead_time, method = mean)
test_X_impute$lead_time <- impute(test_X_impute$lead_time, val = mean(train_X_impute$lead_time, na.rm = T))

# replace n/a in babies with 0
#train_X_impute["nr_babies"][train_X_impute["nr_babies"] == "n/a"] <- 0
#test_X_impute["nr_babies"][test_X_impute["nr_babies"] == "n/a"] <- 0
#validation_X_impute["nr_babies"][validation_X_impute["nr_babies"] == "n/a"] <- 0


# Now check again if there are missing values
colMeans(is.na(test_X_impute))
colMeans(is.na(train_X_impute))

# change values bigger than 1 to 1 for car parking spaces
#train_X_impute$car_parking_spaces[train_X_impute$car_parking_spaces > 1] <- 1
unique(train_X_impute$car_parking_spaces)
unique(train_X_impute$nr_adults)
unique(train_X_impute$nr_babies)
unique(train_X_impute$nr_children)
unique(train_X_impute$nr_nights)
unique(train_X_impute$nr_previous_bookings)
unique(train_X_impute$previous_cancellations)
unique(train_X_impute$special_requests)

train_X_outlier <- train_X_impute
test_X_outlier <- test_X_impute


#check for outliers
train_X_outlier$car_parking_spaces[train_X_outlier$car_parking_spaces > 3] <- 3
train_X_outlier$lead_time[train_X_outlier$lead_time > 365] <- 365
train_X_outlier$nr_adults[train_X_outlier$nr_adults > 10] <- 10
train_X_outlier$nr_nights[train_X_outlier$nr_nights > 21] <- 21
train_X_outlier$nr_children[train_X_outlier$nr_children > 5] <- 5
train_X_outlier$nr_previous_bookings[train_X_outlier$nr_previous_bookings > 15] <- 15
train_X_outlier$previous_cancellations[train_X_outlier$previous_cancellations > 10] <- 10

# still working on
#train_X_data <- data.frame(train_X_outlier,train_y)
#test_complementary <- train_X_data[train_X_outlier$market_segment == "Complementary", ]
#str(test_complementary)
#test1 <- test_complementary[test_complementary$train_y == 0, ]
#test3 <- test_complementary[test_complementary$train_y == 70, ]
#str(test1)
#str(test_complementary)
#str(test3)
# test_complementary has 340obs, test1 has 309obs
#write.table(test_complementary, file = "data/silver/test_complementary.csv", sep = ",", row.names = F)
#test_complementary2 <- train_X_data[train_X_outlier$market_segment != "Complementary", ]
#test2 <- test_complementary2[test_complementary2$train_y == 0, ]
# 614 obs in test2
#str(test2)
#unique(test_complementary2$train_y)
#unique(test_complementary$train_y)
#str(test_complementary2)

unique(train_X_outlier$nr_adults)
unique(train_X_outlier$meal_booked)
# flags
# !!!
# drop columns we do not want to flag
train_X <- subset(train_X , select = -c(arrival_date,assigned_room_type,canceled,car_parking_spaces,customer_type,deposit,is_repeated_guest,last_status,last_status_date,lead_time,market_segment,meal_booked,nr_children,nr_nights,nr_previous_bookings,previous_bookings_not_canceled,previous_cancellations,reserved_room_type,special_requests))
test_X <- subset(test_X , select = -c(arrival_date,assigned_room_type,canceled,car_parking_spaces,customer_type,deposit,is_repeated_guest,last_status,last_status_date,lead_time,market_segment,meal_booked,nr_children,nr_nights,nr_previous_bookings,previous_bookings_not_canceled,previous_cancellations,reserved_room_type,special_requests))
str(test_X)

train_X_outlier <- cbind(train_X_outlier,
                        naFlag(df = train_X))
test_X_impute <- cbind(test_X_impute,
                       naFlag(df = test_X, df_val = train_X))
str(test_X_impute)


#convert deposit into 1 and 0
train_X_outlier$deposit<-ifelse(train_X_outlier$deposit=="deposit equal to total cost of stay --- no refund",1,0)
test_X_impute$deposit<-ifelse(test_X_impute$deposit=="deposit equal to total cost of stay --- no refund",1,0)

#convert is_repeated_guest into 1 and 0
train_X_outlier$is_repeated_guest<-ifelse(train_X_outlier$is_repeated_guest=="yes",1,0)
test_X_impute$is_repeated_guest<-ifelse(test_X_impute$is_repeated_guest=="yes",1,0)

#convert hotel_type into 1 and 0
train_X_outlier$hotel_type<-ifelse(train_X_outlier$hotel_type=="City Hotel",1,0)
test_X_impute$hotel_type<-ifelse(test_X_impute$hotel_type=="City Hotel",1,0)

#write it to silver file
train_X_cleaned <- train_X_outlier
test_X_cleaned <- test_X_impute

write.table(train_X_cleaned, file = "data/silver/train_X_cleaned_retrain.csv", sep = ",", row.names = F)
write.table(test_X_cleaned, file = "data/silver/test_X_cleaned_retrain.csv", sep = ",", row.names = F)
write.table(train_y, file = "data/gold/train_y_retrain.csv", sep = ",", row.names = F, col.names = c("average_daily_rate"))


