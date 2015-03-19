##################################################
#
#  Transforming trades/day/question form data base query to matrix with 0s for question-days without trades
#  
#
#################################################


start <- Sys.time()

activityData <-read.csv("tradesPerDay 150207.csv")
activityDataIndex <- with(activityData, order(activityData$quest_id, activityData$created_at))
activityData <- activityData[activityDataIndex,]

tstart <- ax.POSIXct("2012-11-25")      #########  need to incorporate date selection from Incentive selectsion
tStop <- as.POSIXct("2015-02-07")       #########

days <- seq(1,ceiling(as.double(tstop - tstart)),1)

Activity <- matrix(0,nrow=length(unique(qiqactivityData$quest_id)),ncol=length(days))

for (q in activityData$quest_id) {
  for (d in 1:length(days) ) {
    
  }
  
}

write.table(data.frame(rsq,raqExp,acqu,acop,ru,rf,rc),file="Brier Scores All Qestions.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)
