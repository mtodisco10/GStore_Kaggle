#Disable Scientific Notation
options(scipen=999)
#import libraries
library(data.table)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(highcharter)
library(ggExtra)
library(dummies)
library(fastDummies)
library(randomForest)
library(xgboost)
library(chron)

#Read in the Data
classes <- c('character','integer','character','character','character','character','character','character','character','integer','integer','integer')
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE, colClasses = classes)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE, colClasses = classes)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE, colClasses = c('character','character'))

# convert date column from character to Date class
train_data$date <- as.Date(as.character(train_data$date), format='%Y%m%d')
test_data$date <- as.Date(as.character(test_data$date), format='%Y%m%d')

#Date Features
train_data$month_num <- month(train_data$date)
train_data$week_num <- wday(train_data$date)
train_data$day_num <- days(train_data$date)

test_data$month_num <- month(test_data$date)
test_data$week_num <- wday(test_data$date)
test_data$day_num <- days(test_data$date)

train_data$date <- NULL
test_data$date <- NULL

# convert visitStartTime to POSIXct
train_data$visitStartTime <- as_datetime(train_data$visitStartTime)
test_data$visitStartTime <- as_datetime(test_data$visitStartTime)

#Get hour of visit
train_data$hour_num <- hour(train_data$visitStartTime)
test_data$hour_num <- hour(test_data$visitStartTime)

#Function to create a variable for time since last session
time_since_last_session <- function(df, time_unit='secs'){
  time_since_df <- df %>%
    select(fullVisitorId, sessionId, visitStartTime) %>%
    group_by(fullVisitorId) %>%
    arrange(visitStartTime) %>%
    mutate(time_diff = as.numeric(visitStartTime - lag(visitStartTime), units = time_unit))
  time_since_df[is.na(time_since_df$time_diff), "time_diff"] <- 0
  df <- merge(df, time_since_df[, c('sessionId','time_diff')], by='sessionId', all.x=TRUE)
}

train_data <- time_since_last_session(train_data, 'secs')
test_data <- time_since_last_session(test_data, 'secs')

#Parsing and flattening the json fields
parse_json <- function(df, col_lst){
  parsed_df <- df
  for (col in col_lst){
    flat_fields <- paste("[", paste(df[, col], collapse = ","), "]") %>% fromJSON(flatten = T)
    parsed_df <- cbind(parsed_df, flat_fields)
    }
  return(parsed_df)
}

json_cols = c('device', 'geoNetwork', 'totals', 'trafficSource')

train_data <- parse_json(train_data, json_cols)
test_data <- parse_json(test_data, json_cols)

#Drop old json columns
train_data_subset <- subset(train_data, select = -c(device, geoNetwork, totals, trafficSource))
test_data_subset <- subset(test_data, select = -c(device, geoNetwork, totals, trafficSource))

#Convert values in the data to N/A
set_na_values <- function(df, na_vals){
  for (col in names(df)){
    set(df, i=which(df[[col]] %in% na_vals), j=col, value=NA)
  }
}

na_vals <- c('unknown.unknown','(not set)','not available in demo dataset','(not provided)','(none)','<NA>')

set_na_values(train_data_subset, na_vals)
set_na_values(test_data_subset, na_vals)

#Remove columns with only a single value or less
remove_single_val_cols <- function(df){
  unique_col_vals <- sapply(df, function(x){length(unique(x[!is.na(x)]))})
  df <- subset(df, select = names(unique_col_vals[unique_col_vals > 1]))
}

train_data_subset <- remove_single_val_cols(train_data_subset)
test_data_subset <- remove_single_val_cols(test_data_subset)

#add bounces & newVisits back in
train_data_subset <- cbind(train_data_subset, subset(train_data, select = c(bounces, newVisits)))
test_data_subset <- cbind(test_data_subset, subset(test_data, select = c(bounces, newVisits)))

# character columns to convert to numeric
num_cols <- c('hits', 'pageviews', 'bounces', 'newVisits',
              'transactionRevenue')

# change columns to numeric
train_data_subset[, num_cols] <- sapply(train_data_subset[, num_cols], as.numeric)
test_data_subset[, c('hits', 'pageviews', 'bounces', 'newVisits')] <- sapply(test_data_subset[, c('hits', 'pageviews', 'bounces', 'newVisits')], as.numeric)

#Take log of the target variable
train_data_subset$transactionRevenue <- log1p(train_data_subset$transactionRevenue)

#Group browsers
train_data_subset$browser <- ifelse(train_data_subset$browser %in% c('Chrome', 'Safari', 'Firefox', 'Internet, Explorer', 'Edge', 'Android Webview', 'Safari (in-app)', 'Opera Mini', 'Opera', 'UC Browser', 'Coc Coc'), train_data_subset$browser, 'Other')
test_data_subset$browser <- ifelse(test_data_subset$browser %in% c('Chrome', 'Safari', 'Firefox', 'Internet, Explorer', 'Edge', 'Android Webview', 'Safari (in-app)', 'Opera Mini', 'Opera', 'UC Browser', 'Coc Coc'), test_data_subset$browser, 'Other')

#Group Operating Systems
train_data_subset$operatingSystem <- ifelse(train_data_subset$operatingSystem %in% c('Windows', 'Macintosh', 'Android', 'iOS', 'Linux', 'Chrome OS', 'Windows Phone'), train_data_subset$operatingSystem, 'Other')
test_data_subset$operatingSystem <- ifelse(test_data_subset$operatingSystem %in% c('Windows', 'Macintosh', 'Android', 'iOS', 'Linux', 'Chrome OS', 'Windows Phone'), test_data_subset$operatingSystem, 'Other')

#Group SubContinents
train_data_subset$subContinent <- ifelse(train_data_subset$subContinent %in% c('Polynesia', 'Micronesian Region', 'Melanesia', '(not set)'), train_data_subset$subContinent, 'Other')
test_data_subset$subContinent <- ifelse(test_data_subset$subContinent %in% c('Polynesia', 'Micronesian Region', 'Melanesia', '(not set)'), test_data_subset$subContinent, 'Other')

#Fill in NA for Medium
train_data_subset[is.na(train_data_subset$medium), "medium"] <- "Not Found"
test_data_subset[is.na(test_data_subset$medium), "medium"] <- "Not Found"

#Encode categorical variables and drop originals
cat_encode <- function(df, cat_cols){
  for (col in cat_cols){
    df[, paste0(col, '_encoded')] <- as.numeric(factor(df[ , col]))
    df[, col] <- NULL
  }
  return(df)
}

cat_cols <- c('channelGrouping', 'browser', 'operatingSystem', 'subContinent', 'deviceCategory', 'continent', 'source','medium', 'country')

train_data_encoded <- cat_encode(train_data_subset, cat_cols)
test_data_encoded <- cat_encode(test_data_subset, cat_cols)

#Fill in NA Values w/ Zero

na_cols <- c('pageviews','newVisits','bounces','continent_encoded','source_encoded')

train_data_encoded[na_cols][is.na(train_data_encoded[na_cols])] <- 0
test_data_encoded[na_cols][is.na(test_data_encoded[na_cols])] <- 0

#Fill in transactionRevenue NA's
train_data_encoded['transactionRevenue'][is.na(train_data_encoded['transactionRevenue'])] <- 0

#Drop visitStartTime column
drop_cols <- c('device','geoNetwork','socialEngagementType','')
train_data_encoded$visitStartTime <- NULL
test_data_encoded$visitStartTime <- NULL

#Remove columns with NA
dtrain_subset <- Filter(function(x)!any(is.na(x)), train_data_encoded)
dtest_subset <- Filter(function(x)!any(is.na(x)), test_data_encoded)

#drop id columns
dtrain_subset <- subset(dtrain_subset, select = -c(fullVisitorId, sessionId, visitId))
dtest_subset <- subset(dtest_subset, select = -c(fullVisitorId, sessionId, visitId))

#train test split
set.seed(123)
smp_siz = floor(.8*nrow(dtrain_subset))

ind = sample(seq_len(nrow(dtrain_subset)),size = smp_siz)

X_train <- dtrain_subset[ind, ]
X_train$transactionRevenue <- NULL
X_test <- dtrain_subset[-ind, ]
X_test$transactionRevenue <- NULL

y_train <- dtrain_subset[ind, ]$transactionRevenue
y_test <- dtrain_subset[-ind, ]$transactionRevenue

#X_test_scaled = scale(dtest, center=attr(X_train_scaled, "scaled:center"), 
#                      scale=attr(X_train_scaled, "scaled:scale"))

# fit <- randomForest(y_train ~ .,
#                     data = X_train, 
#                     importance = TRUE, 
#                     ntree = 10)
# 
# prediction <- predict(fit, dtest_subset)

#length(sort(prediction))
#length(y_test)
#mean((y_test - prediction)^2)

# data.frame(actual= y_test, prediction = prediction)
# submit <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = prediction)
# 
# submit_df <- data.frame(submit %>%
#   group_by(fullVisitorId) %>%
#   summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))
# 
# submit_df[is.na(submit_df$PredictedLogRevenue), "PredictedLogRevenue"] <- 0
# 
# submit_df[submit_df$PredictedLogRevenue == -Inf, "PredictedLogRevenue"] <- 0
# 
# write.csv(submit_df, file = "7thforest.csv", row.names = FALSE)

library(catboost)

dtrain_pool <- catboost.load_pool(data = X_train, label = y_train)

dval_pool <- catboost.load_pool(data = X_test, label = y_test)

params <- list(iterations=400,
               learning_rate=0.05,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 50,
               od_type='Iter',
               metric_period = 50,
               od_wait=100,
               use_best_model=TRUE)

model <- catboost.train(dtrain_pool, dval_pool, params)

dtest_pool = catboost.load_pool(data = dtest_subset)

preds <- catboost.predict(model, dtest_pool)
#mean((y_test - preds)^2)
preds <- sapply(preds, function(x){ifelse(x < 0, 0, x)})
head(preds)
submit_catboost <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = preds)

submit_catboost <- data.frame(submit_catboost %>%
                              group_by(fullVisitorId) %>%
                              summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

write.csv(submit_catboost, file = "catboost1.csv", row.names = FALSE)
