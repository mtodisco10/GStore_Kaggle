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

# convert visitStartTime to POSIXct
train_data$visitStartTime <- as_datetime(train_data$visitStartTime)
test_data$visitStartTime <- as_datetime(test_data$visitStartTime)
  
#Parsing and flattening the json fields
device_train_df <- paste("[", paste(train_data$device, collapse = ","), "]") %>% fromJSON(flatten = T)
geo_network_train_df <- paste("[", paste(train_data$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
totals_train_df <- paste("[", paste(train_data$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
traffic_source_train_df <- paste("[", paste(train_data$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

device_test_df <- paste("[", paste(test_data$device, collapse = ","), "]") %>% fromJSON(flatten = T)
geo_network_test_df <- paste("[", paste(test_data$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
totals_test_df <- paste("[", paste(test_data$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
traffic_source_test_df <- paste("[", paste(test_data$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

#Bind new dataframes to the original train dataframe
train_data <- cbind(train_data, device_train_df, geo_network_train_df, totals_train_df, traffic_source_train_df)
test_data <- cbind(test_data, device_test_df, geo_network_test_df, totals_test_df, traffic_source_test_df)

#Drop old json columns
train_data <- subset(train_data, select = -c(device, geoNetwork, totals, trafficSource))
test_data <- subset(test_data, select = -c(device, geoNetwork, totals, trafficSource))

#Date Features
train_data$month_num <- month(train_data$date)
train_data$week_num <- wday(train_data$date)
train_data$day_num <- days(train_data$date)
train_data$hour_num <- hour(train_data$visitStartTime)

test_data$month_num <- month(test_data$date)
test_data$week_num <- wday(test_data$date)
test_data$day_num <- days(test_data$date)
test_data$hour_num <- hour(test_data$hour_num)

train_data$date <- NULL
test_data$date <- NULL

#Convert values in the data to N/A
na_vals <- c('unknown.unknown', '(not set)', 'not available in demo dataset', 
             '(not provided)', '(none)', '<NA>')
for(col in names(train_data)) {
  set(train_data, i=which(train_data[[col]] %in% na_vals), j=col, value=NA)
}

for(col in names(test_data)) {
  set(test_data, i=which(test_data[[col]] %in% na_vals), j=col, value=NA)
}

unique_train <- sapply(train_data, function(x) { length(unique(x[!is.na(x)])) })
unique_test <- sapply(test_data, function(x) { length(unique(x[!is.na(x)])) })

train_data_subset <- subset(train_data, select = names(unique_train[unique_train > 1]))
test_data_subset <- subset(test_data, select = names(unique_test[unique_test > 1]))

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

train_data_subset$channelGrouping_encoded <- as.numeric(factor(train_data_subset$channelGrouping))
train_data_subset$channelGrouping <- NULL
test_data_subset$channelGrouping_encoded <- as.numeric(factor(test_data_subset$channelGrouping))
test_data_subset$channelGrouping <- NULL

train_data_subset$browser <- ifelse(train_data_subset$browser %in% c('Chrome', 'Safari', 'Firefox', 'Internet, Explorer', 'Edge', 'Android Webview', 'Safari (in-app)', 'Opera Mini', 'Opera', 'UC Browser', 'Coc Coc'), train_data_subset$browser, 'Other')
test_data_subset$browser <- ifelse(test_data_subset$browser %in% c('Chrome', 'Safari', 'Firefox', 'Internet, Explorer', 'Edge', 'Android Webview', 'Safari (in-app)', 'Opera Mini', 'Opera', 'UC Browser', 'Coc Coc'), test_data_subset$browser, 'Other')

train_data_subset$browser_encoded <- as.numeric(factor(train_data_subset$browser))
train_data_subset$browser <- NULL
test_data_subset$browser_encoded <- as.numeric(factor(test_data_subset$browser))
test_data_subset$browser <- NULL

train_data_subset$operatingSystem <- ifelse(train_data_subset$operatingSystem %in% c('Windows', 'Macintosh', 'Android', 'iOS', 'Linux', 'Chrome OS', 'Windows Phone'), train_data_subset$operatingSystem, 'Other')
test_data_subset$operatingSystem <- ifelse(test_data_subset$operatingSystem %in% c('Windows', 'Macintosh', 'Android', 'iOS', 'Linux', 'Chrome OS', 'Windows Phone'), test_data_subset$operatingSystem, 'Other')

train_data_subset$operatingSystem_encoded <- as.numeric(factor(train_data_subset$operatingSystem))
train_data_subset$operatingSystem <- NULL
test_data_subset$operatingSystem_encoded <- as.numeric(factor(test_data_subset$operatingSystem))
test_data_subset$operatingSystem <- NULL

train_data_subset$subContinent <- ifelse(train_data_subset$subContinent %in% c('Polynesia', 'Micronesian Region', 'Melanesia', '(not set)'), train_data_subset$subContinent, 'Other')
test_data_subset$subContinent <- ifelse(test_data_subset$subContinent %in% c('Polynesia', 'Micronesian Region', 'Melanesia', '(not set)'), test_data_subset$subContinent, 'Other')

train_data_subset$subContinent_encoded <- as.numeric(factor(train_data_subset$subContinent))
train_data_subset$subContinent<- NULL
test_data_subset$subContinent_encoded <- as.numeric(factor(test_data_subset$subContinent))
test_data_subset$subContinent <- NULL

train_data_subset$deviceCategory_encoded <- as.numeric(factor(train_data_subset$deviceCategory))
train_data_subset$deviceCategory <- NULL
test_data_subset$deviceCategory_encoded <- as.numeric(factor(test_data_subset$deviceCategory))
test_data_subset$deviceCategory <- NULL

train_data_subset$continent_encoded <- as.numeric(factor(train_data_subset$continent))
train_data_subset$continent <- NULL
test_data_subset$continent_encoded <- as.numeric(factor(test_data_subset$continent))
test_data_subset$continent <- NULL

train_data_subset$source_encoded <- as.numeric(factor(train_data_subset$continent))
train_data_subset$source <- NULL
test_data_subset$source_encoded <- as.numeric(factor(test_data_subset$continent))
test_data_subset$source <- NULL

train_data_subset[is.na(train_data_subset$pageviews), "pageviews"] <- 0
test_data_subset[is.na(test_data_subset$pageviews), "pageviews"] <- 0

train_data_subset[is.na(train_data_subset$transactionRevenue), "transactionRevenue"] <- 0


train_data_subset[is.na(train_data_subset$newVisits), "newVisits"] <- 0
test_data_subset[is.na(test_data_subset$newVisits), "newVisits"] <- 0

train_data_subset[is.na(train_data_subset$continent_encoded), "continent_encoded"] <- 0
test_data_subset[is.na(test_data_subset$continent_encoded), "continent_encoded"] <- 0

train_data_subset[is.na(train_data_subset$medium), "medium"] <- "Not Found"
test_data_subset[is.na(test_data_subset$medium), "medium"] <- "Not Found"

train_data_subset$medium_encoded <- as.numeric(factor(train_data_subset$medium))
train_data_subset$medium <- NULL
test_data_subset$medium_encoded <- as.numeric(factor(test_data_subset$medium))
test_data_subset$medium <- NULL

train_data_subset$country_encoded <- as.numeric(factor(train_data_subset$country))
train_data_subset$country <- NULL
test_data_subset$country_encoded <- as.numeric(factor(test_data_subset$country))
test_data_subset$country <- NULL

train_data_subset$visitStartTime <- NULL
test_data_subset$visitStartTime <- NULL

train_data_subset[c("bounces","source_encoded","country_encoded")][is.na(train_data_subset[c("bounces","source_encoded","country_encoded")])] <- 0
test_data_subset[c("bounces","source_encoded","country_encoded")][is.na(test_data_subset[c("bounces","source_encoded","country_encoded")])] <- 0

#Remove columns with NA
dtrain_subset <- Filter(function(x)!any(is.na(x)), train_data_subset)
dtest_subset <- Filter(function(x)!any(is.na(x)), test_data_subset)

#drop id columns
dtrain_subset <- subset(dtrain_subset, select = -c(fullVisitorId, sessionId, visitId))
dtest_subset <- subset(dtest_subset, select = -c(fullVisitorId, sessionId, visitId))

#train test split
set.seed(123)
smp_siz = floor(.75*nrow(dtrain_subset))

ind = sample(seq_len(nrow(dtrain_subset)),size = smp_siz)

X_train <- dtrain_subset[ind, ]
X_train$transactionRevenue <- NULL
X_test <- dtrain_subset[-ind, ]
X_test$transactionRevenue <- NULL

y_train <- dtrain_subset[ind, ]$transactionRevenue
y_test <- dtrain_subset[-ind, ]$transactionRevenue

X_train


#X_test_scaled = scale(dtest, center=attr(X_train_scaled, "scaled:center"), 
#                      scale=attr(X_train_scaled, "scaled:scale"))

fit <- randomForest(y_train ~ .,
                    data = X_train, 
                    importance = TRUE, 
                    ntree = 10)

prediction <- predict(fit, dtest_subset)

#length(sort(prediction))
#length(y_test)
#mean((y_test - prediction)^2)

data.frame(actual= y_test, prediction = prediction)
submit <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = prediction)

submit_df <- data.frame(submit %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

submit_df[is.na(submit_df$PredictedLogRevenue), "PredictedLogRevenue"] <- 0

submit_df[submit_df$PredictedLogRevenue == -Inf, "PredictedLogRevenue"] <- 0

write.csv(submit_df, file = "7thforest.csv", row.names = FALSE)

View(head(X_train))
library(catboost)

y = dtrain$transactionRevenue

dtrain_pool <- catboost.load_pool(data = X_train_scaled[train_ind, ], label =  log1p(y[train_ind]))

dval_pool <- catboost.load_pool(data = X_train_scaled[!train_ind, ], label = log1p(y[!train_ind]))

params <- list(iterations=1000,
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

dtest_scaled = scale(dtest, center=attr(X_train_scaled, "scaled:center"), 
                      scale=attr(X_train_scaled, "scaled:scale"))

dtest_scaled = catboost.load_pool(data = dtest_scaled)
preds <- catboost.predict(model, dtest_scaled)

submit_catboost <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = preds)

submit_catboost <- data.frame(submit_catboost %>%
                              group_by(fullVisitorId) %>%
                              summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

write.csv(submit_catboost, file = "catboost1.csv", row.names = FALSE)
