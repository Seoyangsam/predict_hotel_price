# here we will perform a polynomial regression 
library(glmnet)
install.packages("Metrics")
library(Metrics)

# read files
train_X <- read.csv(file = 'data/gold/train_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
train_y <- read.csv(file = 'data/gold/train_y.csv', header = TRUE, fileEncoding = 'latin1')

validation_y <- read.csv(file = 'data/gold/validation_y.csv', header = TRUE, fileEncoding = 'latin1')
validation_X <- read.csv(file = 'data/gold/validation_X_scale.csv', header = TRUE, fileEncoding = 'latin1')

test_set <- read.csv(file = 'data/gold/test_X_scale.csv', header = TRUE, fileEncoding = 'latin1')
test_id <- read.csv(file = 'data/bronze/test_id.csv', header = TRUE, fileEncoding = 'latin1')

train_and_validation <- read.csv(file = 'data/gold/train_and_validation.csv', header = TRUE, fileEncoding = 'latin1')
dependant_y <- read.csv(file = 'data/gold/dependant_y.csv', header = TRUE, fileEncoding = 'latin1')

# FIRST STEP: TRAIN ON TRAINING SET AND PREDICT ON VALIDATION SET

# dependent and independent variables in 1 dataframe
train_X_data <- data.frame(train_X,train_y)
validation_X_data <- data.frame(validation_X,validation_y)
str(validation_X)

# ANOVA TEST FOR EACH VARIABLE TO SEE WHICH POLY FITS BEST PER VARIABLE  

# number of car parking spaces  
poly_parkingspaces1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_parkingspaces2 <- lm(average_daily_rate ~ . - car_parking_spaces + poly(car_parking_spaces,2) , data = train_X_data)
poly_parkingspaces3 <- lm(average_daily_rate ~ . - car_parking_spaces + poly(car_parking_spaces,3) , data = train_X_data)
anova(poly_parkingspaces1, poly_parkingspaces2, poly_parkingspaces3)
    # not significant so we keep "car_parking_spaces" of degree 1 

# lead time 
poly_leadtime1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_leadtime2 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 2) , data = train_X_data)
poly_leadtime3 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 3) , data = train_X_data)
poly_leadtime4 <- lm(average_daily_rate ~ . - lead_time + poly(lead_time, 4) , data = train_X_data)
anova(poly_leadtime1,poly_leadtime2,poly_leadtime3,poly_leadtime4)
    # p-value <0,05 so significant: we take degree 2 for "lead_time"

# nr of adults 
poly_nradults1 <-  lm(average_daily_rate ~ . , data = train_X_data)
poly_nradults2 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,2) , data = train_X_data)
poly_nradults3 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,3) , data = train_X_data)
poly_nradults4 <-  lm(average_daily_rate ~ . - nr_adults + poly(nr_adults,4) , data = train_X_data)
anova(poly_nradults1, poly_nradults2, poly_nradults3, poly_nradults4)
    # p-value <0,05 so significant: we take less complex, degree 2 for "nr_adults"

# nr of babies 
poly_nrbabies_1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_nrbabies_2 <- lm(average_daily_rate ~ . - nr_babies + poly(nr_babies,2) , data = train_X_data)
anova(poly_nrbabies_1,poly_nrbabies_2)
    # not significant so we keep "nr_babies" of degree 1 

# nr of children 
poly_nrchildren1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_nrchildren2 <- lm(average_daily_rate ~ . - nr_children + poly(nr_children,2) , data = train_X_data)
anova(poly_nrchildren1, poly_nrchildren2)
    # p-value <0,05 so significant: we take degree 2 for "nr_children"

# nr of nights 
poly_nrnights1 <-  lm(average_daily_rate ~ . , data = train_X_data)
poly_nrnights2 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,2), data = train_X_data)
poly_nrnights3 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,3), data = train_X_data)
poly_nrnights4 <-  lm(average_daily_rate ~ . - nr_nights + poly(nr_nights,4), data = train_X_data)
anova(poly_nrnights1, poly_nrnights2, poly_nrnights3, poly_nrnights4)
    # p-value <0,05 so significant: we take less complex, degree 2 for "nr_nights"

# nr of previous bookings 
poly_nrprevbookings1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_nrprevbookings2 <- lm(average_daily_rate ~ . - nr_previous_bookings + poly(nr_previous_bookings,2), data = train_X_data)
poly_nrprevbookings3 <- lm(average_daily_rate ~ . - nr_previous_bookings + poly(nr_previous_bookings,3), data = train_X_data)
poly_nrprevbookings4 <- lm(average_daily_rate ~ . - nr_previous_bookings + poly(nr_previous_bookings,4), data = train_X_data)
anova(poly_nrprevbookings1, poly_nrprevbookings2, poly_nrprevbookings3, poly_nrprevbookings4)
    # not significant so we keep "previous_bookings" of degree 1 

# previous cancellations
poly_prevcancel1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_prevcancel2 <- lm(average_daily_rate ~ . - previous_cancellations + poly(previous_cancellations,2), data = train_X_data)
poly_prevcancel3 <- lm(average_daily_rate ~ . - previous_cancellations + poly(previous_cancellations,3), data = train_X_data)
anova(poly_prevcancel1, poly_prevcancel2, poly_prevcancel3)
    # p-value <0,05 so significant: we take degree 2 for "previous_cancellations"

# special requests
poly_specialrequests1 <- lm(average_daily_rate ~ . , data = train_X_data)
poly_specialrequests2 <- lm(average_daily_rate ~ . - special_requests + poly(special_requests,2), data = train_X_data)
poly_specialrequests3 <- lm(average_daily_rate ~ . - special_requests + poly(special_requests,3), data = train_X_data)
anova(poly_specialrequests1, poly_specialrequests2, poly_specialrequests3)
    # not significant so we keep "special_requests" of degree 1 


# POLYNOMIAL REGRESSION MODEL 
poly.fit <- lm(average_daily_rate ~ . - car_parking_spaces - lead_time - nr_adults - nr_children - nr_nights - nr_previous_bookings - previous_cancellations - special_requests + poly(car_parking_spaces,1) + poly(lead_time,2) + poly(nr_adults,2) + poly(nr_children,2) + poly(nr_nights,2) + poly(nr_previous_bookings,1) + poly(previous_cancellations,2) + poly(special_requests,1) , data = train_X_data)    

# make predictions on validation set
pred.valset <- predict(poly.fit, newdata = validation_X )

# MSE 
pred_valset_error <- sqrt(mean((pred.valset - validation_y$average_daily_rate)^2))
write.table(pred_valset_error, file = "data/results/polynomial_model_RMSE.csv", sep = ",", row.names = FALSE, col.names=TRUE)

# MAE 
pred_valset_mae <- mae(validation_y$average_daily_rate, pred.valset)
write.table(pred_valset_mae, file = "data/results/polynomial_model_MAE.csv", sep = ",", row.names = FALSE, col.names=TRUE)
