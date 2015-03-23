#############################################
#
#  Formating/Generating data for regressions
#  Using Monthly Brier data.csv
#
#############################################
#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#source("Monthly Regression Data Formating.R")
#"questionId","2014-01-07","2014-02-07","2014-03-07", "2014-04-07","2014-05-07","2014-06-07","2014-07-07","2014-08-07","2014-09-07",
#"2014-10-07","2014-11-07","2014-12-07","2015-01-07","2015-02-07","2015-03-07", "2015-04-07"


source("Get_Data_150203lb.R")

startData <- Sys.time()
print("Data formating started")

MBD <-read.csv("Monthly Brier data.csv")
dataColName <- c("questionId","2014-01-07","2014-02-07","2014-03-07",
                 "2014-04-07","2014-05-07","2014-06-07","2014-07-07","2014-08-07","2014-09-07","2014-10-07","2014-11-07","2014-12-07","2015-01-07",
                 "2015-02-07","2015-03-07")

#setAData <- read.csv("cat19questionB.csv")
#setBData <- read.csv("cat20questionB.csv")

regressionData <- matrix(nrow=(length(MBD$questionId)*length(MBD)),ncol=7)
colnames(regressionData)=c("questionId","month","brier","set","inExp","active","1stActive")


expFirst <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expStop <- as.POSIXct("2015-03-07")

i <- 0
for (q in 1:length(MBD$questionId)) {   # remove all NAs
  for (m in 2:(length(MBD)-1)) {
    index <- ((q-1)*(length(MBD)-2))+(m-1)
    
    regressionData[index,"questionId"] <- as.integer(MBD$questionId[q])
    regressionData[index,"month"] <- dataColName[m]
    regressionData[index,"brier"] <- MBD[q,m]
    #regressionData[index,"set"] <- index
    
    ### setting incentive experiement set Variable - N => not in experiment
    if (regressionData[index,"questionId"]%in%setAData$question_id) {
      regressionData[index,"set"] <- "A"
    } else if (regressionData[index,"questionId"]%in%setBDataQ4$question_id) {
        regressionData[index,"set"] <- "B"
      } else {
          regressionData[index,"set"] <- "N"
        }
    
    ### setting incentive Experiment Variable - Y => in experiment, N=> not in experiment
    if(regressionData[index,"set"]=="A"|regressionData[index,"set"]=="B") {
      regressionData[index,"inExp"] <- "Y"
    } else {
        regressionData[index,"inExp"] <- "N"
    }

    ### setting incentive Experiment Active Variable - Y => in experiment, N=> not in experiment
    if ((regressionData[index,"month"]=="2014-12-07"|regressionData[index,"month"]=="2015-02-07") &regressionData[index,"set"]=="A") {
      regressionData[index,"active"] <- "A"
    } else if ((regressionData[index,"month"]=="2014-12-07"|regressionData[index,"month"]=="2015-02-07") &regressionData[index,"set"]=="B") {
      regressionData[index,"active"] <- "N"
      } else if ((regressionData[index,"month"]=="2015-01-07"|regressionData[index,"month"]=="2015-03-07") &regressionData[index,"set"]=="A") {
        regressionData[index,"active"] <- "N"
        } else if ((regressionData[index,"month"]=="2015-01-07"|regressionData[index,"month"]=="2015-03-07") &regressionData[index,"set"]=="B") {
            regressionData[index,"active"] <- "A"
          } else {
            regressionData[index,"active"] <- "NA"
            }
    
    ### setting incentive Experiment 1st Active Variable - 1 => question is first active in that month, 2 => secind active month
    if (regressionData[index,"month"]=="2014-12-07" &regressionData[index,"set"]=="A") {
      regressionData[index,"1stActive"] <- "1"
    } else  if (regressionData[index,"month"]=="2015-02-07" &regressionData[index,"set"]=="A") {
        regressionData[index,"1stActive"] <- "2"
      } else if (regressionData[index,"month"]=="2015-01-07" &regressionData[index,"set"]=="B") {
          regressionData[index,"1stActive"] <- "1"
        } else  if (regressionData[index,"month"]=="2015-03-07" &regressionData[index,"set"]=="B") {
            regressionData[index,"1stActive"] <- "2"
          } else {
              regressionData[index,"1stActive"] <- "NA"
          }
    
  }
 i <- i+1 
}



write.table(regressionData,file="Monthly Regression Data.csv",sep=",",append=F,
            col.names=c("questionId","month","brier","set","inExp","active","1stActive"),row.names=F)


#regressionDataClean <- regressionData
regressionDataClean <- regressionData[!is.na(regressionData[,"brier"]),]

write.table(regressionDataClean,file="Monthly Regression Data(clean).csv",sep=",",append=F,
            col.names=c("questionId","month","brier","set","inExp","active","1stActive"),row.names=F)

durationData <- as.double(difftime(Sys.time(),startData,units="sec"))   #reports time to retrieve files
print (c("Data Generation Complete", durationData))
