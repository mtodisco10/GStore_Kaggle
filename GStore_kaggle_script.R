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

#Read in the Data
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE)

# convert date column from character to Date class
train_data$date <- as.Date(as.character(train_data$date), format='%Y%m%d')

# convert visitStartTime to POSIXct
train_data$visitStartTime <- as_datetime(train_data$visitStartTime)
  
#Parsing and flattening the json fields
device_df <- paste("[", paste(train_data$device, collapse = ","), "]") %>% fromJSON(flatten = T)
geo_network_df <- paste("[", paste(train_data$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
totals_df <- paste("[", paste(train_data$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
traffic_source_df <- paste("[", paste(train_data$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

#Bind new dataframes to the original train dataframe
train_data <- cbind(train_data, device_df, geo_network_df, totals_df, traffic_source_df)

#Drop old json columns
train_data <- subset(train_data, select = -c(device, geoNetwork, totals, trafficSource))

#Convert values in the data to N/A
na_vals <- c('unknown.unknown', '(not set)', 'not available in demo dataset', 
             '(not provided)', '(none)', '<NA>')
for(col in names(train_data)) {
  
  set(train_data, i=which(train_data[[col]] %in% na_vals), j=col, value=NA)
  
}


unique <- sapply(train_data, function(x) { length(unique(x[!is.na(x)])) })

train_data_subset <- subset(train_data, select = names(unique[unique > 1]))

#add bounces & newVisits back in
train_data_subset <- cbind(train_data_subset, subset(train_data, select = c(bounces, newVisits)))

glimpse(train_data_subset)

# character columns to convert to numeric
num_cols <- c('hits', 'pageviews', 'bounces', 'newVisits',
              'transactionRevenue')

# change columns to numeric
train_data_subset[, num_cols] <- sapply(train_data_subset[, num_cols], as.numeric)

#Divide transactionRevenue by 1,000,000
train_data_subset$transactionRevenue <- train_data_subset$transactionRevenue / 1e+06

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
print(time_range)
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

View(train_data_subset)

