###############################################################
#
#  Modifies TradesPerDay to add 0s for days without trades
#
##############################################################
#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#source("Daily Activity 150326.R")

tpd<-read.csv("TradesPerDay 150307.csv")
MBD <- read.csv("Daily Brier data.csv")

dailyData <- colnames(MBD)
dataColName <- rep(as.character("2103-11-25"),length(dailyData))
dates <- rep(as.Date("2013-11-25"),length(dailyData)-1)
for (x in 2:length(dailyData)) {
   dataColName[x] <- strsplit(dailyData[x],"X")[[1]][2]
   #dates[x-1] <- as.Date(dataColName[x],"%Y.%m.%d")
   dates[x-1] <- as.Date(strsplit(dailyData[x],"X")[[1]][2],"%Y.%m.%d")
}
numDays <- min(length(dataColName),length(tpd$trade_id))
questionIds <- levels(factor(tpd$quest_id))
numRows <- length(questionIds)


activityMatrix <- matrix(rep(0, (numDays+1)*numRows),ncol=(numDays+1))    # numDays+1 for question_id column

#for (r in 1:numRows) {                         # qda -> question daily activity
for (r in 1:5) {
  activityMatrix[r,1] <- questionIds[r]         # put question id in first column
  for (d in 2:numDays+1) {                      # first column is questionId
    index <- (d-1)+((r-1)*numDays)
    if (tpd$quest_id[index]==questionIds[r] &tpd$created_at[index] < dates[d-1]) {
      activityMatrix[r,d] <- 0
    } else if (tpd$quest_id[index]==questionIds[r] &tpd$created_at[index]==dates[d-1]) {
      activityMatrix[r,d] <- tpd$trade_id[index]
            } else {next}
  }
  
  
}
  
  activityMatrix[q,1] <- tpd$question_id
  

 