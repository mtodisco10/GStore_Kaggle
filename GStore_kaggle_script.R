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
library(caret)
library(chron)

#Read in the Data
classes <- c('character','integer','character','character','character','character','character','character','character','integer','integer','integer')
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE, colClasses = classes)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE, colClasses = classes)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE, colClasses = c('character','character'))

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

parsed_train <- parse_json(train_data, json_cols)
parsed_test <- parse_json(test_data, json_cols)

y <- log1p(as.numeric(parsed_train$transactionRevenue))
y[is.na(y)] <- 0

#Drop old json columns
parsed_train_subset <- subset(parsed_train, select = -c(device, geoNetwork, totals, trafficSource))
parsed_test_subset <- subset(parsed_test, select = -c(device, geoNetwork, totals, trafficSource))

#Drop campaignCode & transactionRevenue from training data
parsed_train_subset$campaignCode <- NULL
parsed_train_subset$transactionRevenue <- NULL

#Storing index and fields to use later
id <- parsed_test_subset[, "fullVisitorId"]
tri <- 1:nrow(parsed_train_subset)
idx <- ymd(parsed_train_subset$date) < ymd("20170601")
te_bounces <- parsed_test_subset[, "bounces"]

#Reordering test data columns to match the order of the train data columns
parsed_test_subset <- subset(parsed_test_subset, select = names(parsed_train_subset)) 

#Combining train & test data
tr_te <- rbind(parsed_train_subset, parsed_test_subset)

# convert date column from character to Date class
tr_te$date <- as.Date(as.character(tr_te$date), format='%Y%m%d')

#Date Features
tr_te$month <- month(tr_te$date)
tr_te$wday <- wday(tr_te$date)
tr_te$week <- week(tr_te$date)
tr_te$day <- day(tr_te$date)
tr_te$yday <- yday(tr_te$date)
tr_te$qday <- qday(tr_te$date)
tr_te$date <- NULL

# convert visitStartTime to POSIXct
tr_te$visitStartTime <- as_datetime(tr_te$visitStartTime)

#Get hour of visit
tr_te$hour_num <- hour(tr_te$visitStartTime)

#Combine Hour & Month to get new feature
tr_te$hour_month <- tr_te$hour_num * tr_te$month_num

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

tr_te <- time_since_last_session(tr_te, 'secs')
tr_te$time_diff_log <- log1p(tr_te$time_diff)

#Convert values in the data to N/A
set_na_values <- function(df, na_vals){
  for (col in names(df)){
    set(df, i=which(df[[col]] %in% na_vals), j=col, value=NA)
  }
}

na_vals <- c('unknown.unknown','(not set)','not available in demo dataset','(not provided)','(none)','<NA>')

set_na_values(tr_te, na_vals)

#Ad Content Feature Flags
tr_te$adContent_flag <- ifelse(is.na(tr_te$adContent), 0L, 1L)

tr_te$adwords_flag <- ifelse(is.na(tr_te$adwordsClickInfo.page), 0L, 1L)

#Metro Feature Flag
tr_te$metro_flag <- ifelse(is.na(tr_te$metro), 0L, 1L)

#Network Domain Feature Flag
tr_te$networkDomain_flag <- ifelse(is.na(tr_te$networkDomain), 0L, 1L)

#Video Ad
tr_te$videoAd <- ifelse(!tr_te$adwordsClickInfo.isVideoAd, 1L, 0L)

#columns to fill na w/ 0
na_cols <- c('bounces','newVisits','isTrueDirect','pageviews','videoAd')
tr_te[na_cols][is.na(tr_te[na_cols])] <- 0

# character columns to convert to numeric
num_cols <- c('hits', 'pageviews', 'newVisits','visits')

# change columns to numeric
tr_te[, num_cols] <- sapply(tr_te[, num_cols], as.numeric)

#Mobile
tr_te$isMobile <- ifelse(tr_te$isMobile, 1L, 0)

#Ratio columns
#Hits/pageview
tr_te$hits_per_pageview <- tr_te$hits / tr_te$pageviews

#hits/visits
tr_te$hits_per_visit <- tr_te$hits / tr_te$visits
tr_te$hits_per_visit_log <- log1p(tr_te$hits / tr_te$visits)

#pageview/visits
tr_te$pageviews_per_visit <- tr_te$pageviews / tr_te$visits
tr_te$pageviews_per_visit_log <- log1p(tr_te$pageviews / tr_te$visits)

#newVisits * hits
tr_te$newVisits_times_hits <- tr_te$newVisits * tr_te$hits
tr_te$newVisits_times_hits_log <- log1p(tr_te$newVisits * tr_te$hits)

#newVisits * pageviews
tr_te$newVisits_times_pageviews <- tr_te$newVisits * tr_te$pageviews
tr_te$newVisits_times_pageviews_log <- log1p(tr_te$newVisits * tr_te$pageviews)

#Remove columns with only a single value or less
remove_single_val_cols <- function(df){
  unique_col_vals <- sapply(df, function(x){length(unique(x[!is.na(x)]))})
  df <- subset(df, select = names(unique_col_vals[unique_col_vals > 1]))
}

tr_te_subset <- remove_single_val_cols(tr_te)

fn <- funs(mean, median, var, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

# sum_by_vn <- train_data_subset %>%
#   select(visitNumber, hits, pageviews) %>% 
#   group_by(visitNumber) %>% 
#   summarise_all(fn) 


#City: New York
#City: San Fran
#pageviews_mean_vn
#hits_mean_vn
#pageviews_mean_country
#hits_mean_country
#pageviews_mean_city
#hits_mean_city
#goggle vs apple

#channelGrouping
tr_te_subset$channelGrouping_social <- ifelse(tr_te_subset$channelGrouping == 'Social', 1L, 0L)

tr_te_subset$channelGrouping_affiliates <- ifelse(tr_te_subset$channelGrouping == 'Affiliates', 1L, 0L)

#Group browsers
tr_te_subset$browser <- ifelse(tr_te_subset$browser %in% c('Chrome', 'Safari', 'Firefox', 'Internet, Explorer', 'Edge', 'Android Webview', 'Safari (in-app)', 'Opera Mini', 'Opera', 'UC Browser', 'Coc Coc'), tr_te_subset$browser, 'Other')

tr_te_subset$firefox <- ifelse(tr_te_subset$browser == 'Firefox', 1L, 0L)

tr_te_subset$chrome <- ifelse(tr_te_subset$browser == 'Chrome', 1L, 0L)

#Group Operating Systems
tr_te_subset$operatingSystem <- ifelse(tr_te_subset$operatingSystem %in% c('Windows', 'Macintosh', 'Android', 'iOS', 'Linux', 'Chrome OS', 'Windows Phone'), tr_te_subset$operatingSystem, 'Other')

tr_te_subset$chrome_OS <- ifelse(tr_te_subset$operatingSystem == 'Chrome OS', 1L, 0L)

tr_te_subset$macintosh_OS <- ifelse(tr_te_subset$operatingSystem == 'Macintosh', 1L, 0L)

#Fill in NA for Medium
tr_te_subset[is.na(tr_te_subset$medium), "medium"] <- "Not Found"

tr_te_subset$medium_affiliate <- ifelse(tr_te_subset$medium == 'affiliate', 1L, 0L)

#Encode categorical variables and drop originals
cat_encode <- function(df, cat_cols){
  for (col in cat_cols){
    df[, paste0(col, '_encoded')] <- as.integer(factor(df[ , col]))
    df[, col] <- NULL
  }
  return(df)
}

cat_cols <- c('channelGrouping', 'browser', 'operatingSystem', 'deviceCategory', 'continent','medium', 'country')

tr_te_encoded <- cat_encode(tr_te_subset, cat_cols)

#Fill in NA Values w/ Zero

na_cols <- c('continent_encoded')

tr_te_encoded[na_cols][is.na(tr_te_encoded[na_cols])] <- 0

#Drop visitStartTime column
tr_te_encoded$visitStartTime <- NULL

#Remove columns with NA
tr_te_final <- Filter(function(x)!any(is.na(x)), tr_te_encoded)

#drop id columns
tr_te_final <- subset(tr_te_final, select = -c(fullVisitorId, sessionId, visitId))

#train test split
set.seed(123)
library(catboost)
dtest = catboost.load_pool(data = subset(tr_te_final, select = -c(bounces))[-tri, ])
dtrain <- catboost.load_pool(data = subset(tr_te_final, select = -c(bounces))[tri, ][idx, ], label =  y[idx])
dval <- catboost.load_pool(data = subset(tr_te_final, select = -c(bounces))[tri, ][!idx, ], label = y[!idx])

params <- list(iterations=5000,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 50,
               od_type='Iter',
               metric_period = 50,
               od_wait=100,
               use_best_model=TRUE)

model <- catboost.train(dtrain, dval, params)

feature_imp <- catboost.get_feature_importance(model, 
                                               pool = NULL, 
                                               fstr_type = "FeatureImportance")

feature_mat <- data.frame(cbind(data.frame(names(subset(tr_te_final, select = -c(bounces)))), data.frame(feature_imp)))
feature_mat[order(desc(feature_mat$feature_imp)),  ]
nrow(dtest)
preds <- catboost.predict(model, dtest) %>%
  as_tibble() %>% 
  set_names("y") %>% 
  mutate(y = ifelse(y < 0, 0, y)) #%>% 
  #bind_cols(id) %>% 
  #bind_cols(te_bounces) %>%
  #mutate(y = ifelse(is.na(bounces) , y , 0)) %>%  #### when bounce == 1 ,  then we predict revenue = 0
  #mutate(y = expm1(y)) %>%  #### This is for log(sum) prediction, comment this line, incase of LB high scores
  #select(-bounces) %>%
  #group_by(fullVisitorId) %>% 
  #summarise(y = sum(y))

submit_catboost <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = preds)

submit_catboost <- data.frame(submit_catboost %>%
                                group_by(fullVisitorId) %>%
                                summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

##############################################

dtrain_subset <- subset(dtrain_subset, select=-c(bounces))
dtest_subset <- subset(dtest_subset, select=-c(bounces))
smp_siz = floor(.75*nrow(dtrain_subset))

ind = sample(seq_len(nrow(dtrain_subset)),size = smp_siz)

X_train <- dtrain_subset[ind, ]
X_train$transactionRevenue <- NULL
X_test <- dtrain_subset[-ind, ]
X_test$transactionRevenue <- NULL

#Scaling
X_train_scaled <- X_train
X_test_scaled <- X_test
dtest_subset_scaled <- dtest_subset
#dtest_subset_scaled$bounces <- as.numeric(dtest_subset_scaled$bounces)

y_train <- dtrain_subset[ind, ]$transactionRevenue
y_test <- dtrain_subset[-ind, ]$transactionRevenue

# 
# y_train <- dtrain_subset[ind, ]$transactionRevenue
# y_test <- dtrain_subset[-ind, ]$transactionRevenue

#X_test_scaled = scale(dtest, center=attr(X_train_scaled, "scaled:center"), 
#                      scale=attr(X_train_scaled, "scaled:scale"))


library(catboost)

dtrain_pool <- catboost.load_pool(data = X_train_scaled, label = y_train)

dval_pool <- catboost.load_pool(data = X_test_scaled, label = y_test)

params <- list(iterations=5000,
               learning_rate=0.01,
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

dtest_pool = catboost.load_pool(data = X_test_scaled)

preds <- catboost.predict(model, dtest_pool)

preds <- sapply(preds, function(x){ifelse(x < 1, 0, x)})

dtest_subset$bounces <- test_data$bounces
dtest_subset['bounces'][is.na(dtest_subset['bounces'])] <- 0
mean((y_test - preds)^2)

dtest_subset$preds <- ifelse(dtest_subset$bounces == 1, 0, preds)

##################

dtrain_pool <- catboost.load_pool(data = subset(dtrain_subset, select = -c(transactionRevenue)), label = dtrain_subset$transactionRevenue)

params <- list(iterations=5000,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 50,
               od_type='Iter',
               metric_period = 50,
               od_wait=100,
               use_best_model=TRUE)

model <- catboost.train(dtrain_pool, params=params)

preds <- catboost.predict(model, dtest_pool)

preds <- sapply(preds, function(x){ifelse(x < 1, 0, x)})
dtest_subset$bounces <- test_data$bounces
dtest_subset['bounces'][is.na(dtest_subset['bounces'])] <- 0
dtest_subset$preds <- ifelse(dtest_subset$bounces == 1, 0, preds)

submit_catboost <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = dtest_subset$preds)

submit_catboost <- data.frame(submit_catboost %>%
                              group_by(fullVisitorId) %>%
                              summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

View(head(submit_catboost, 100))
write.csv(submit_catboost, file = "catboost10.csv", row.names = FALSE)

#bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")



# xgb_model <- xgboost(data=as.matrix(X_train), label=y_train, max.depth = 2, eta = 2, nthread=2, nrounds = 100, objective = 'reg:linear', eval_metric = 'rmse', silent = 0)
# 
# pred <- predict(xgb_model, as.matrix(dtest_subset))
# 
# log1p(pred)
# log10(pred)
min(dtrain_subset$transactionRevenue)
