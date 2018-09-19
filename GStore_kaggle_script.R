#Disable Scientific Notation
options(scipen=999)
library(jsonlite)

#Read in the Data
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE)
  
#Parsing and flattening the json fields
device_df <- paste("[", paste(train_data$device, collapse = ","), "]") %>% fromJSON(flatten = T)
geo_network_df <- paste("[", paste(train_data$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
totals_df <- paste("[", paste(train_data$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
traffic_source_df <- paste("[", paste(train_data$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

