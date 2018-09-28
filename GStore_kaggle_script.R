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
train_data$day_num <- as.numeric(days(train_data$date))

test_data$month_num <- month(test_data$date)
test_data$week_num <- wday(test_data$date)
test_data$day_num <- as.numeric(days(test_data$date))

train_data$date <- NULL
test_data$date <- NULL

# convert visitStartTime to POSIXct
train_data$visitStartTime <- as_datetime(train_data$visitStartTime)
test_data$visitStartTime <- as_datetime(test_data$visitStartTime)

#Get hour of visit
train_data$hour_num <- hour(train_data$visitStartTime)
test_data$hour_num <- hour(test_data$visitStartTime)

#Combine Hour & Month to get new feature
train_data$hour_month <- (train_data$hour_num * train_data$month_num) / 10
test_data$hour_month <- (test_data$hour_num * test_data$month_num) / 10

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

#Ad Content Feature Flags
train_data_subset$adContent_flag <- ifelse(is.na(train_data_subset$adContent), 1, 0)
test_data_subset$adContent_flag <- ifelse(is.na(test_data_subset$adContent), 1, 0)

train_data_subset$adwords_flag <- ifelse(is.na(train_data_subset$adwordsClickInfo.page), 1, 0)
test_data_subset$adwords_flag <- ifelse(is.na(test_data_subset$adwordsClickInfo.page), 1, 0)

#Metro Feature Flag
train_data_subset$metro_flag <- ifelse(is.na(train_data_subset$metro), 1, 0)
test_data_subset$metro_flag <- ifelse(is.na(test_data_subset$metro), 1, 0)

#Network Domain Feature Flag
train_data_subset$networkDomain_flag <- ifelse(is.na(train_data_subset$networkDomain), 1, 0)
test_data_subset$networkDomain_flag <- ifelse(is.na(test_data_subset$networkDomain), 1, 0)

#columns to fill na w/ 0
na_cols <- c('bounces','newVisits','isTrueDirect')
train_data_subset[na_cols][is.na(train_data_subset[na_cols])] <- 0
train_data_subset['transactionRevenue'][is.na(train_data_subset['transactionRevenue'])] <- 0
test_data_subset[na_cols][is.na(test_data_subset[na_cols])] <- 0

#Remove columns with only a single value or less
remove_single_val_cols <- function(df){
  unique_col_vals <- sapply(df, function(x){length(unique(x[!is.na(x)]))})
  df <- subset(df, select = names(unique_col_vals[unique_col_vals > 1]))
}

train_data_subset <- remove_single_val_cols(train_data_subset)
test_data_subset <- remove_single_val_cols(test_data_subset)

# character columns to convert to numeric
num_cols <- c('hits', 'pageviews', 'bounces', 'newVisits',
              'transactionRevenue','isMobile')

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
dtrain_subset <- subset(dtrain_subset[dtrain_subset$bounces==0, ], select=-c(bounces))
dtest_subset <- subset(dtest_subset, select=-c(bounces))
smp_siz = floor(.75*nrow(dtrain_subset))

ind = sample(seq_len(nrow(dtrain_subset)),size = smp_siz)

X_train <- dtrain_subset[ind, ]
X_train$transactionRevenue <- NULL
X_test <- dtrain_subset[-ind, ]
X_test$transactionRevenue <- NULL

#Scaling
X_train <- scale(X_train)
X_test <- scale(X_test)
dtest_subset <- scale(dtest_subset)

y_train <- dtrain_subset[ind, ]$transactionRevenue
y_test <- dtrain_subset[-ind, ]$transactionRevenue

# 
# y_train <- dtrain_subset[ind, ]$transactionRevenue
# y_test <- dtrain_subset[-ind, ]$transactionRevenue

#X_test_scaled = scale(dtest, center=attr(X_train_scaled, "scaled:center"), 
#                      scale=attr(X_train_scaled, "scaled:scale"))


library(catboost)

dtrain_pool <- catboost.load_pool(data = X_train, label = y_train)

dval_pool <- catboost.load_pool(data = X_test, label = y_test)

params <- list(iterations=3000,
               learning_rate=0.02,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 50,
               od_type='Iter',
               metric_period = 50,
               od_wait=100,
               use_best_model=TRUE)

model <- catboost.train(dtrain_pool, 
                        dval_pool, 
                        params)

feature_imp <- catboost.get_feature_importance(model, 
                                pool = NULL, 
                                fstr_type = "FeatureImportance")

feature_mat <- data.frame(cbind(data.frame(names(X_train)), data.frame(feature_imp)))
feature_mat[order(desc(feature_mat$feature_imp)),  ]

#ggplot(dtrain_subset, aes(x=bounces, y=transactionRevenue)) + geom_point()

dtest_pool = catboost.load_pool(data = dtest_subset)

preds <- catboost.predict(model, dtest_pool)

#mean((y_test - preds)^2)

dtest_subset$preds <- sapply(preds, function(x){ifelse(x < 0, 0, x)})
dtest_subset$bounces <- as.numeric(test_data$bounces)
dtest_subset['bounces'][is.na(dtest_subset['bounces'])] <- 0

dtest_subset$preds <- ifelse(dtest_subset$bounces == 1, 0, dtest_subset$preds)

submit_catboost <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = dtest_subset$preds)
head(dtest_subset)
submit_catboost <- data.frame(submit_catboost %>%
                              group_by(fullVisitorId) %>%
                              summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))
View(head(submit_catboost,10))
write.csv(submit_catboost, file = "catboost8.csv", row.names = FALSE)
head(submit_catboost)
