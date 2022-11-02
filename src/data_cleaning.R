#First we read our datas
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE, stringsAsFactors = FALSE, fileEncoding = 'latin1')
str(train)

test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X)

#Next, we split the independent & dependent variables in the training set.
train_X <- subset(train, select = -c(average_daily_rate))
str(train_X)

train_y <- train$average_daily_rate
train_y <- gsub(' .*','',train_y) #remove the euro sign
train_y <- as.double(train_y) #convert from chr to float
str(train_y)

# we drop id, booking agent and booking company
train_X <- subset(train_X , select = -c(id, booking_company, booking_agent))
str(train_X)
test_X <- subset(test_X, select = -c(id, booking_company, booking_agent))
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

train_X_impute$booking_distribution_channel <- impute(train_X_impute$booking_distribution_channel, method = modus) 
test_X_impute$booking_distribution_channel <- impute(test_X_impute$booking_distribution_channel, val = modus(train_X$booking_distribution_channel, na.rm = T))

train_X_impute$country <- impute(train_X_impute$country, method=modus)
test_X_impute$country <- impute(test_X_impute$country, val = modus(train_X$country, na.rm = T))

train_X_impute$customer_type <- impute(train_X_impute$customer_type, method = modus)
test_X_impute$customer_type <- impute(test_X_impute$customer_type, val = modus(train_X$customer_type, na.rm = T))

train_X_impute$hotel_type <- impute(train_X_impute$hotel_type, method = modus)
test_X_impute$hotel_type <- impute(test_X_impute$hotel_type, val = modus(train_X$hotel_type, na.rm = T))

train_X_impute$last_status <- impute(train_X_impute$last_status, method = modus)
test_X_impute$last_status <- impute(test_X_impute$last_status, val = modus(train_X$last_status, na.rm = T))

train_X_impute$market_segment <- impute(train_X_impute$market_segment, method = modus)
test_X_impute$market_segment <- impute(test_X_impute$market_segment, val = modus(train_X$market_segment, na.rm = T))

# impute all numerical variables
train_X_impute$car_parking_spaces <- impute(train_X_impute$car_parking_spaces, method = median)
test_X_impute$car_parking_spaces <- impute(test_X_impute$car_parking_spaces, val = median(train_X$car_parking_spaces, na.rm = T))

train_X_impute$days_in_waiting_list <- impute(train_X_impute$days_in_waiting_list, method = median)
test_X_impute$days_in_waiting_list <- impute(test_X_impute$days_in_waiting_list, val = median(train_X$days_in_waiting_list, na.rm = T))

train_X_impute$nr_adults <- impute(train_X_impute$nr_adults, method = median)
test_X_impute$nr_adults <- impute(test_X_impute$nr_adults, val = median(train_X$nr_adults, na.rm = T))

train_X_impute$nr_children <- impute(train_X_impute$nr_children, method = median)
test_X_impute$nr_children <- impute(test_X_impute$nr_children, val = median(train_X$nr_children, na.rm = T))

train_X_impute$nr_previous_bookings <- impute(train_X_impute$nr_previous_bookings, method = median)
test_X_impute$nr_previous_bookings <- impute(test_X_impute$nr_previous_bookings, val = median(train_X$nr_previous_bookings, na.rm = T))

train_X_impute$previous_bookings_not_canceled <- impute(train_X_impute$previous_bookings_not_canceled, method = median)
test_X_impute$previous_bookings_not_canceled <- impute(test_X_impute$previous_bookings_not_canceled, val = median(train_X$previous_bookings_not_canceled, na.rm = T))

train_X_impute$previous_cancellations <- impute(train_X_impute$previous_cancellations, method = median)
test_X_impute$previous_cancellations <- impute(test_X_impute$previous_cancellations, val = median(train_X$previous_cancellations, na.rm = T))

# Change lead time to integer to calculate mean
train_X$lead_time <- gsub("[  day(s)]",'',train_X$lead_time)
train_X$lead_time <-as.integer(train_X$lead_time)

test_X$lead_time <- gsub("[  day(s)]",'',test_X$lead_time)
test_X$lead_time <-as.integer(test_X$lead_time)

train_X_impute$lead_time <- impute(train_X_impute$lead_time, method = mean)
test_X_impute$lead_time <- impute(test_X_impute$lead_time, val = mean(train_X$lead_time, na.rm = T))

# replace n/a in babies with 0
train_X_impute["nr_babies"][train_X_impute["nr_babies"] == "n/a"] <- 0
test_X_impute["nr_babies"][test_X_impute["nr_babies"] == "n/a"] <- 0

train_X_impute$nr_booking_changes <- impute(train_X_impute$nr_booking_changes, val = 0)
test_X_impute$nr_booking_changes <- impute(test_X_impute$nr_booking_changes, val = 0)

train_X_impute$nr_babies
test_X_impute$nr_booking_changes

# Now check again if there are missing values
colMeans(is.na(test_X_impute))
colMeans(is.na(train_X_impute))


# flags
