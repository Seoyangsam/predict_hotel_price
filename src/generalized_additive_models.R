
# here we will perform GAM 
library(GAM)

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')
validation_set <- read.csv(file = 'data/bronze/validation_set.csv', header = TRUE)
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_and_validation <- read.csv(file = 'data/gold/train_and_validation.csv', header = TRUE, fileEncoding = 'latin1')

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)

# determine optimal degree of freedom
fit_lead_time <- smooth.spline(train_X_data$lead_time, train_X_data$average_daily_rate, cv = TRUE)
fit_nr_adults <- smooth.spline(train_X_data$nr_adults, train_X_data$average_daily_rate, cv = TRUE)
fit_nr_babies <- smooth.spline(train_X_data$nr_babies, train_X_data$average_daily_rate, cv = TRUE)
fit_nr_children <- smooth.spline(train_X_data$nr_children, train_X_data$average_daily_rate, cv = TRUE)
fit_nr_nights <- smooth.spline(train_X_data$nr_nights, train_X_data$average_daily_rate, cv = TRUE)
fit_nr_previous_bookings <- smooth.spline(train_X_data$nr_previous_bookings, train_X_data$average_daily_rate, cv = TRUE)
fit_previous_cancellations <- smooth.spline(train_X_data$previous_cancellations, train_X_data$average_daily_rate, cv = TRUE)
fit_special_requests <- smooth.spline(train_X_data$special_requests, train_X_data$average_daily_rate, cv = TRUE)

# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# GAM
gam <- lm(average_daily_rate ~ . - lead_time - nr_adults - nr_babies - nr_children - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + s(lead_time, fit_lead_time$df) + s(nr_adults, fit_nr_adults$df) + s(nr_babies, fit_nr_babies$df) + s(nr_children, fit_nr_children$df) + s(nr_nights, fit_nr_nights$df) + s(nr_previous_bookings, fit_nr_previous_bookings$df) + s(previous_cancellations, fit_previous_cancellations$df) + s(special_requests, fit_special_requests$df), data = train_X_data)

# predict on validation set
preds <- predict(gam, newdata = validation_X)


# SECOND STEP: RE-TRAIN ON TRAINING + VALIDATION SET AND PREDICT ON TEST SET

