#Here we perform the data preprocessing
#First we read our datas
train <- read.csv(file = 'data/bronze/train.csv', header = TRUE, fileEncoding = 'latin1')
str(train)


test_X <- read.csv(file = 'data/bronze/test.csv', header = TRUE, fileEncoding = 'latin1')
str(test_X)



#Next, we split the independent & dependent variables in the training set.
train_X <- subset(train, select = -c(average_daily_rate))
str(train_X)

train_y <- train$average_daily_rate
train_y <- gsub(" .*", "", train_y) #remove the euro sign
train_y <- as.integer(train_y) #convert from chr to int
str(train_y)

# we drop id, booking agent and booking company
train_X <- subset(train_X , select = -c(id, booking_company, booking_agent))
str(train_X)
test_X <- subset(test_X, select = -c(id, booking_company, booking_agent))
str(test_X)

# missing values
colMeans(is.na(test_X))
colMeans(is.na(train_X))

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

# flags

#change the format of date
train_X$arrival_date <- gsub("'",'',train_X$arrival_date)
train_X$arrival_date<-as.Date(train_X$arrival_date,format="%B %d %Y")

#convert canceled into 1 and 0
train_X$canceled<-ifelse(train_X$canceled=="stay cancelled",1,0)
test_X$canceled<-ifelse(test_X$canceled=="stay cancelled",1,0)

#convert deposit into 1 and 0
train_X$deposit<-ifelse(train_X$deposit=="deposit equal to total cost of stay --- no refund",1,0)
test_X$deposit<-ifelse(test_X$deposit=="deposit equal to total cost of stay --- no refund",1,0)

#convert is_repeated_guest into 1 and 0
train_X$is_repeated_guest<-ifelse(train_X$is_repeated_guest=="yes",1,0)
test_X$is_repeated_guest<-ifelse(test_X$is_repeated_guest=="yes",1,0)

#convert hotel_type into 1 and 0
train_X$hotel_type<-ifelse(train_X$hotel_type=="City Hotel",1,0)
test_X$hotel_type<-ifelse(test_X$hotel_type=="City Hotel",1,0)

#integer encoding for meal_booked
#union(unique(train_X$meal_booked), unique(test_X$meal_booked))
#meal_booked_levels <- c("meal package NOT booked", "bed & breakfast (BB)", "breakfast + one other meal // usually dinner (half board)", "full board [BREAKF -- lunch -- Dinner]") # in correct order!
#train_X$meal_booked <- as.numeric(factor(train_X$meal_booked, levels = meal_booked_levels))
#test_X$meal_booked <- as.numeric(factor(test_X$meal_booked, levels = meal_booked_levels))

install.packages("dummy")
library(dummy)
# get categories and dummies
cats <- categories(train_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment")])
# apply on train set (exclude reference categories)
dummies_train <- dummy(train_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment")],
                       object = cats)
dummies_train <- subset(dummies_train, select = -c(booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation))
# apply on test set (exclude reference categories)
dummies_test <- dummy(test_X[, c("booking_distribution_channel", "customer_type", "last_status", "market_segment")],
                       object = cats)
dummies_test <- subset(dummies_test, select = -c(booking_distribution_channel_Corporate, customer_type_Contract, last_status_Canceled, market_segment_Aviation))

## merge with overall training set
train_X <- subset(train_X, select = -c(booking_distribution_channel, customer_type, last_status, market_segment))
train_X <- cbind(train_X, dummies_train)
## merge with overall test set
test_X <- subset(test_X, select = -c(booking_distribution_channel, customer_type, last_status, market_segment))
test_X <- cbind(test_X, dummies_test)

#convert the predictors to factors
train_X[sapply(train_X, is.character)] <- lapply(train_X[sapply(train_X, is.character)], as.factor)
str(train_X)
test_X[sapply(test_X, is.character)] <- lapply(test_X[sapply(test_X, is.character)], as.factor)
str(test_X)

# Hanne: check percentages of missing values for each column
train_impute <- train
train_X_impute <- train_X
test_X_impute <- test_X

colMeans(is.na(train_X_impute))
colMeans(is.na(test_X_impute))
colMeans(is.na(train_impute))

#save the dataset
write.table(train_X, file = "data/silver/train_X.csv", sep = "\t", row.names = F)
write.table(test_X, file = "data/silver/test_X.csv", sep = "\t", row.names = F)

#impute missing values
train_X$nr_adults[is.na(train_X$nr_adults)] <- 1
test_X$nr_adults[is.na(test_X$nr_adults)] <- 1

train_X$nr_babies[is.na(train_X$nr_babies)] <- 0
train_X$nr_babies <-as.integer(train_X$nr_babies)
test_X$nr_babies[is.na(test_X$nr_babies)] <- 0
test_X$nr_babies <-as.integer(test_X$nr_babies)

train_X$nr_booking_changes[is.na(train_X$nr_booking_changes)] <- 0
train_X$nr_booking_changes <-as.integer(train_X$nr_booking_changes)
test_X$nr_booking_changes[is.na(test_X$nr_booking_changes)] <- 0
test_X$nr_booking_changes <-as.integer(test_X$nr_booking_changes)

train_X$nr_children[is.na(train_X$nr_children)] <- 0
test_X$nr_children[is.na(test_X$nr_children)] <- 0

train_X$nr_previous_bookings[is.na(train_X$nr_previous_bookings)] <- 0
test_X$nr_previous_bookings[is.na(test_X$nr_previous_bookings)] <- 0

train_X$previous_bookings_not_canceled[is.na(train_X$previous_bookings_not_canceled)] <- 0
test_X$previous_bookings_not_canceled[is.na(test_X$previous_bookings_not_canceled)] <- 0

train_X$previous_cancellations[is.na(train_X$previous_cancellations)] <- 0
test_X$previous_cancellations[is.na(test_X$previous_cancellations)] <- 0

train_X$lead_time <- gsub("[  day(s)]",'',train_X$lead_time)
train_X$lead_time <-as.integer(train_X$lead_time)
train_X$lead_time[is.na(train_X$lead_time)] <- mean(train_X$lead_time, na.rm = T)

test_X$lead_time <- gsub("[  day(s)]",'',test_X$lead_time)
test_X$lead_time <-as.integer(test_X$lead_time)
test_X_impute$Age[is.na(test_X_impute$Age)] <- mean(train_X$Age, na.rm = T)

test_X$last_status_date <- as.Date(test_X$last_status_date)
train_X$last_status_date <- as.Date(train_X$last_status_date)

test_X$lead_time <- as.integer(sub("day(s)", "", test_X$lead_time))
train_X$lead_time <- as.integer(sub("day(s)","",train_X$lead_time))

test_X$nr_booking_changes <- as.integer(test_X$nr_booking_changes)
train_X$nr_booking_changes <- as.integer(test_X$nr_booking_changes)

str(test_X)
str(train_X)
