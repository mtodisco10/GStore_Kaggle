#Disable Scientific Notation
options(scipen=999)
library(jsonlite)

#Read in the Data
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE)
  

full_visit_df <- data.frame(visits=character(),
                            hits=character(),
                            pageviews=character(),
                            newVisits=character(),
                            transactionRevenue=character())


for (visit in train_data$totals[1:1000]){
  visit_df <- data.frame(fromJSON(visit))
  visit_df$newVisits <- ifelse("newVisits" %in% names(visit_df), visit_df$newVisits, 0)
  visit_df$transactionRevenue <- ifelse("transactionRevenue" %in% names(visit_df), visit_df$transactionRevenue, 0)
  visit_df <- visit_df[c('visits','hits','pageviews','newVisits','transactionRevenue')]
  full_visit_df <- rbind(full_visit_df, visit_df)
}

