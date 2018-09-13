#Disable Scientific Notation
options(scipen=999)
library(jsonlite)

#Read in the Data
test_data <- read.csv('dataFiles/test.csv', stringsAsFactors = FALSE)
train_data <- read.csv('dataFiles/train.csv', stringsAsFactors = FALSE)
sample_submission <- read.csv('dataFiles/sample_submission.csv', stringsAsFactors = FALSE)
  

for (visit in train_data$totals[1000:1001]){
  print(fromJSON(visit)$transactionRevenue)
}
