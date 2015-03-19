#############################################
#
#  Formating/Generating data for regressions
#  Using Monthly Brier data.csv
#
#############################################
#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#source("Daily Regression Data Formating.R")
#"questionId","2014-01-07","2014-02-07","2014-03-07", "2014-04-07","2014-05-07","2014-06-07","2014-07-07","2014-08-07","2014-09-07",
#"2014-10-07","2014-11-07","2014-12-07","2015-01-07","2015-02-07","2015-03-07", "2015-04-07"


startData <- Sys.time()
print("Data formating started")

MBD <- read.csv("Daily Brier data.csv")
dailyData <- colnames(MBD)
dataColName <- rep(as.Date("2013-11-25"),length(dailyData)-1)
for (x in 2:length(dailyData)) {
  #daily[x] <- as.POSIXct(strsplit(daily[x],"X")[[1]][2])
  dataColName[x-1] <- as.Date(strsplit(dailyData[x],"X")[[1]][2],"%Y.%m.%d")
}



setAData <- read.csv("cat19questionB.csv")
setBData <- read.csv("cat20questionB.csv")

regressionData <- matrix(nrow=(length(MBD$questionId)*length(MBD)),ncol=7)
colnames(regressionData)=c("questionId","day","brier","set","inExp","active","1stActive")


expFirst <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expStop <- as.POSIXct("2015-03-07")

i <- 0
for (q in 1:length(MBD$questionId)) {   # remove all NAs
  for (m in 2:(length(MBD)-1)) {
    index <- ((q-1)*(length(MBD)-2))+(m-1)
    print(c(q,m,index))
    
    regressionData[index,"questionId"] <- as.integer(MBD$questionId[q])
    #print("1")
    regressionData[index,"day"] <- trunc(as.Date(dataColName[m]),unit="days")
    #print("2")
    regressionData[index,"brier"] <- MBD[q,m]
    #print("3")
    #regressionData[index,"set"] <- index
    
    ### setting incentive experiement set Variable - N => not in experiment
    if (regressionData[index,"questionId"]%in%setAData$question_id) {
      regressionData[index,"set"] <- "A"
      #print("4")
    } else if (regressionData[index,"questionId"]%in%setBData$question_id) {
        regressionData[index,"set"] <- "B"
        #print("5")
      } else {
          regressionData[index,"set"] <- "N"
          #print("6")
        }
    
    ### setting incentive Experiment Variable - Y => in experiment, N=> not in experiment
    if(regressionData[index,"set"]=="A"|regressionData[index,"set"]=="B") {
      regressionData[index,"inExp"] <- "Y"
      #print("7")
    } else {
        regressionData[index,"inExp"] <- "N"
        #print("8")
    }

#    ### setting incentive Experiment Active Variable - Y => in experiment, N=> not in experiment
#    if ((regressionData[index,"month"]=="2014-12-07"|regressionData[index,"month"]=="2015-02-07") &regressionData[index,"set"]=="A") {
#      regressionData[index,"active"] <- "A"
#    } else if ((regressionData[index,"month"]=="2014-12-07"|regressionData[index,"month"]=="2015-02-07") &regressionData[index,"set"]=="B") {
#      regressionData[index,"active"] <- "N"
#      } else if ((regressionData[index,"month"]=="2015-01-07"|regressionData[index,"month"]=="2015-03-07") &regressionData[index,"set"]=="A") {
#        regressionData[index,"active"] <- "N"
#        } else if ((regressionData[index,"month"]=="2015-01-07"|regressionData[index,"month"]=="2015-03-07") &regressionData[index,"set"]=="B") {
#            regressionData[index,"active"] <- "A"
#          } else {
#            regressionData[index,"active"] <- "NA"
#            }
    
#    ### setting incentive Experiment 1st Active Variable - 1 => question is first active in that month, 2 => secind active month
#    if (regressionData[index,"month"]=="2014-12-07" &regressionData[index,"set"]=="A") {
#      regressionData[index,"1stActive"] <- "1"
#    } else  if (regressionData[index,"month"]=="2015-02-07" &regressionData[index,"set"]=="A") {
#        regressionData[index,"1stActive"] <- "2"
#      } else if (regressionData[index,"month"]=="2015-01-07" &regressionData[index,"set"]=="B") {
#          regressionData[index,"1stActive"] <- "1"
#        } else  if (regressionData[index,"month"]=="2015-03-07" &regressionData[index,"set"]=="B") {
#            regressionData[index,"1stActive"] <- "2"
#          } else {
#              regressionData[index,"1stActive"] <- "NA"
#          }
    
  }
 i <- i+1 
}

write.table(regressionData,file="Daily Regressiond Data.csv",sep=",",append=F,
            col.names=c("questionId","day","brier","set","inExp","active","1stActive"),row.names=F)


durationData <- as.double(difftime(Sys.time(),startData,units="sec"))   #reports time to retrieve files
print (c("Data Generation Complete", durationData))
