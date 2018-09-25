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

#Divide transactionRevenue by 1,000,000
train_data_subset$transactionRevenue <- log1p(train_data_subset$transactionRevenue)

#Missing Values
data.table(
  pmiss = sapply(train_data_subset, function(x) { (sum(is.na(x)) / length(x)) }),
  column = names(train_data_subset)
) %>%
  ggplot(aes(x = reorder(column, -pmiss), y = pmiss)) +
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title='Missing data by feature',
    x='Feature',
    y='% missing')

#What was the time range the data was collected?
time_range <- range(train_data_subset$date)

#one year from aug '16 - aug '17

#Transaction Revenue
train_data_subset %>% 
  ggplot(aes(x=log(transactionRevenue), y=..density..)) + 
  geom_histogram(fill='steelblue', na.rm=TRUE, bins=40) + 
  geom_density(aes(x=log(transactionRevenue)), fill='orange', color='orange', alpha=0.3, na.rm=TRUE) + 
  labs(
    title = 'Distribution of transaction revenue',
    x = 'Natural log of transaction revenue'
  )

train_data_subset$channelGrouping_encoded <- as.numeric(factor(train_data_subset$channelGrouping))
test_data_subset$channelGrouping_encoded <- as.numeric(factor(test_data_subset$channelGrouping))

train_data_subset$browser <- ifelse(train_data_subset$browser %in% c('Chrome','Safari','Firefox', 'Internet Explorer','Edge'), train_data_subset$browser, 'Other')
test_data_subset$browser <- ifelse(test_data_subset$browser %in% c('Chrome','Safari','Firefox', 'Internet Explorer','Edge'), test_data_subset$browser, 'Other')

train_data_subset$browser_encoded <- as.numeric(factor(train_data_subset$browser))
test_data_subset$browser_encoded <- as.numeric(factor(test_data_subset$browser))

train_data_subset$operatingSystem <- ifelse(train_data_subset$operatingSystem %in% c('Windows','Macintosh','Android','iOS','Linux','Chrome OS'), train_data_subset$operatingSystem, 'Other')
test_data_subset$operatingSystem <- ifelse(test_data_subset$operatingSystem %in% c('Windows','Macintosh','Android','iOS','Linux','Chrome OS'), test_data_subset$operatingSystem, 'Other')

train_data_subset$operatingSystem_encoded <- as.numeric(factor(train_data_subset$operatingSystem))
test_data_subset$operatingSystem_encoded <- as.numeric(factor(test_data_subset$operatingSystem))

train_data_subset$deviceCategory_encoded <- as.numeric(factor(train_data_subset$deviceCategory))
test_data_subset$deviceCategory_encoded <- as.numeric(factor(test_data_subset$deviceCategory))

train_data_subset$continent_encoded <- as.numeric(factor(train_data_subset$continent))
test_data_subset$continent_encoded <- as.numeric(factor(test_data_subset$continent))

train_data_subset$source_encoded <- as.numeric(factor(train_data_subset$continent))
test_data_subset$source_encoded <- as.numeric(factor(test_data_subset$continent))

train_data_subset[is.na(train_data_subset$pageviews), "pageviews"] <- 0
test_data_subset[is.na(test_data_subset$pageviews), "pageviews"] <- 0

train_data_subset[is.na(train_data_subset$transactionRevenue), "transactionRevenue"] <- 0
test_data_subset[is.na(test_data_subset$transactionRevenue), "transactionRevenue"] <- 0

train_data_subset[is.na(train_data_subset$newVisits), "newVisits"] <- 0
test_data_subset[is.na(test_data_subset$newVisits), "newVisits"] <- 0

train_data_subset[is.na(train_data_subset$continent_encoded), "continent_encoded"] <- 0
test_data_subset[is.na(test_data_subset$continent_encoded), "continent_encoded"] <- 0

train_data_subset[is.na(train_data_subset$medium), "medium"] <- "Not Found"
test_data_subset[is.na(test_data_subset$medium), "medium"] <- "Not Found"

train_data_subset$medium_encoded <- as.numeric(factor(train_data_subset$medium))
test_data_subset$medium_encoded <- as.numeric(factor(test_data_subset$medium))

#Remove columns with NA
dtrain <- subset(train_data_subset, select = c(isMobile, hits, pageviews, newVisits, channelGrouping_encoded, browser_encoded, operatingSystem_encoded, deviceCategory_encoded, continent_encoded, medium_encoded, transactionRevenue))

dtest <- subset(test_data_subset, select = c(isMobile, hits, pageviews, newVisits, channelGrouping_encoded, browser_encoded, operatingSystem_encoded, deviceCategory_encoded, continent_encoded, medium_encoded))

#train test split
smp_siz = floor(1*nrow(dtrain))
set.seed(123)

train_ind = sample(seq_len(nrow(dtrain)),size = smp_siz)

train <- dtrain[train_ind,]
test <- dtrain[-train_ind,]

fit <- randomForest(transactionRevenue ~ isMobile + hits + pageviews + newVisits + channelGrouping_encoded + browser_encoded + operatingSystem_encoded + deviceCategory_encoded + continent_encoded + medium_encoded,
                    data = train, 
                    importance = TRUE, 
                    ntree = 25)

prediction <- predict(fit, dtest)

submit <- data.frame(fullVisitorId = test_data$fullVisitorId, PredictedLogRevenue = prediction)

submit_df <- data.frame(submit %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue=sum(PredictedLogRevenue)))

submit_df[is.na(submit_df$PredictedLogRevenue), "PredictedLogRevenue"] <- 0

submit_df[submit_df$PredictedLogRevenue == -Inf, "PredictedLogRevenue"] <- 0

write.csv(submit_df, file = "3rdforest.csv", row.names = FALSE)
