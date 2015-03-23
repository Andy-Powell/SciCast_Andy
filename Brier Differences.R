#######################################
#
#  Adding change  in Brier, time difference between trades, (qtr,1stActive) score to tradeData.csv
#
######################################

# setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
# source("Brier Differences.R")

RBD <- read.csv("tradeData.csv")    #RBD => raw brier data
RBD$tradedAt <- as.POSIXct(RBD$tradedAt)
RBD$set <- as.character(RBD$set)
RBD$inExp <- NA
RBD$firstActive <- NA
RBD$quarter <- NA

expFirst <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expEnd <- as.POSIXct("2015-03-07")

#brierDiff <- rep(2,length(RBD))

#ord <- order(RBD$questionId, RBD$tradedAt)
#RBD <- RBD(ord)

RBD$brierDiff[1] <- NA
RBD$diffWeight[1] <- NA
RBD$set[1] <- "N"
RBD$inExp[1] <- "out"
#for (tr in 2:length(RBD$tradedAt)) {
#for (tr in 2:150) {
#  if (RBD$questionId[tr]==RBD$questionId[tr-1] &difftime(as.POSIXct(RBD$tradedAt[tr]),as.POSIXct(RBD$tradedAt[tr-1]),"days")>0) {
#    RBD$brierDiff[tr] <- difftime(as.POSIXct(RBD$tradedAt[tr]),as.POSIXct(RBD$tradedAt[tr-1]),"days")
#  } else {brierDiff[tr] <- NA}
#  print(c(RBD$questionId[tr],RBD$questionId[tr-1],difftime(RBD$tradedAt[tr],RBD$tradedAt[tr-1],"days"),RBD$brierDiff[tr]))
#}

for (tr in 2:length(RBD$tradedAt)) {
#for (tr in 2:500) {
  if (RBD$questionId[tr]==RBD$questionId[tr-1] &difftime(as.POSIXct(RBD$tradedAt[tr]),as.POSIXct(RBD$tradedAt[tr-1]))>0) {
    RBD$brierDiff[tr] <- RBD$Brier[tr]-RBD$Brier[tr-1]
#    RBD$diffWeight[tr] <- difftime((RBD$tradedAt[tr]),(RBD$tradedAt[tr-1]),units="days")
#    RBD$diffWeight[tr] <- as.POSIXct(RBD$tradedAt[tr])-as.POSIXct(RBD$tradedAt[tr-1])
    RBD$diffWeight[tr] <- (as.integer(RBD$tradedAt[tr])-as.integer(RBD$tradedAt[tr-1]))/(24*60*60)
  } else {RBD$brierDiff[tr] <- NA}
  #print(c(tr,RBD$questionId[tr],RBD$questionId[tr-1],RBD$brierDiff[tr],RBD$diffWeight[tr]))
  print(tr)


### setting incentive experiement set Variable - N => not in experiment
if (is.na(RBD$set[tr])) {RBD$set[tr] <- "N"}
 
### setting incentive Experiment Variable - Y => in experiment, N=> not in experiment
if(RBD$set[tr]=="A"|RBD$set[tr]=="B") {
  RBD$inExp[tr] <- "in"
} else {
  RBD$inExp[tr]  <- "out"
}


### setting incentive Experiment 1st Active Variable - 1 => question is first active in that month, 2 => secind active month
if (RBD$tradedAt[tr]>=expFirst &RBD$tradedAt[tr]<expChange1) {
  RBD$quarter[tr] <- 1
  if (RBD$set[tr]== "A") {RBD$firstActive[tr] <- "1st"}
} else  if (RBD$tradedAt[tr]>=expChange1 &RBD$tradedAt[tr]<expChange2) {
          RBD$quarter[tr] <- 2
          if (RBD$set[tr]== "B") {RBD$firstActive[tr] <- "1st"}
        }  else if (RBD$tradedAt[tr]>=expChange2 &RBD$tradedAt[tr]<expChange3) {
                  RBD$quarter[tr] <- 3
                  if (RBD$set[tr]== "A") {RBD$firstActive[tr] <- "2nd"}
                }  else if (RBD$tradedAt[tr]>=expChange3 &RBD$tradedAt[tr]<expEnd) {
                          RBD$quarter[tr] <- 4
                          if (RBD$set[tr]== "B") {RBD$firstActive[tr] <- "2nd"}
                        } else {
                          RBD$firstActive[tr] <- "NA"
                          }






}

write.table(RBD,file="Trade Regression Data.csv",sep=",",append=F,col.names=colnames(RBD),row.names=F)

RBD <- RBD[!is.na(RBD$Brier),]

write.table(RBD,file="Trade Regression Data (clean).csv",sep=",",append=F,col.names=colnames(RBD),row.names=F)
