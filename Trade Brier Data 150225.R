###########################################
#
# Generates a data.frame (tradeData) of 
# QuestionId, tradeId, UserId, tradedAt, newValue, ResolvedAt, ResolveValue, Brier Scores
# Called by "Incentive Experiment Trade Brier Generation"
# requires tat, tit, pit, qit, nvt, qiq, rvq, raq, clq 
# usually prepared by "Incentive Experiment Trade Brier Generation"
#
# Calls "Incentive Accuracy Mechanics Basic" which genreates Brier Scores
#
############################################

startBrier <- Sys.time()
print("Brier data calculations started")

expFirst <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expLast <- as.POSIXct("2015-03-07")


tatData<-tat; titData<-tit; pitData<-pit; qitData<-qit; nvtData<-nvt; ovtData<-ovt; qiqData<-qiq; rvqData<-rvq; raqData<-raq; clqData <- clq; rsqData <- rsq

thQuestionId <- as.integer(rep(NA,length(tatData)))
tradeId  <- as.integer(rep(NA,length(tatData)))
thUserId <- as.integer(rep(NA,length(tatData)))
tradedAt <- as.POSIXct(rep(NA,length(tatData)))
newValue <- as.character(rep(NA,length(tatData)))
oldValue <- as.character(rep(NA,length(tatData)))
thBrier <- as.numeric(rep(2,length(tatData)))
thResolvedAt <- as.POSIXct(rep(NA,length(tatData)))
thResolveValue <- as.character(rep(NA,length(tatData)))
brier <- as.numeric(rep(2,length(tatData)))

Set <- as.character(rep(NA,length(tatData)))
Active <- as.character(rep(NA,length(tatData)))

firstActive <- as.POSIXct(rep(NA,length(tatData)))
timeToRes <- as.numeric(rep(2,length(tatData)))
timeSince1stActive <- as.numeric(rep(2,length(tatData)))

qnQuestionId <- as.integer(rep(NA,length(qiqData)))
qnResolvedAt <- as.POSIXct(rep(NA,length(qiqData)))
qnResolveValue <- as.character(rep(NA,length(qiqData)))
qnClass <- as.character(rep(NA,length(qiqData)))

tradeData <- data.frame(thQuestionId,tradeId,thUserId,tradedAt,newValue,oldValue,thResolvedAt,thResolveValue,thBrier,Set,Active,stringsAsFactors=FALSE)
questionData <- data.frame(qnQuestionId,qnResolvedAt,qnResolveValue,qnClass,stringsAsFactors=FALSE)
  
tradeData$thQuestionId <- qitData
tradeData$tradeId <- titData
tradeData$thUserId <- pitData
tradeData$tradedAt <- tatData
tradeData$newValue <- as.character(nvtData)
tradeData$oldValue <- as.character(ovtData)

questionData$qnQuestionId <- qiqData
questionData$qnResolvedAt <- raqData
questionData$qnResolveValue <- as.character(rvqData)
questionData$qnClass <- as.character(clqData)


###### Setting Set variable #####

for (q in incentiveSet){
tradeData$Set[tradeData$thQuestionId==q] <- "A"
}

for (q in controlSet){
tradeData$Set[tradeData$thQuestionId==q] <- "B"
}



##### Setting Activity Variable #####
tradeData$Active[tradeData$tradedAt<expChange1 & tradeData$Set=="A"] <- "Y"
tradeData$Active[tradeData$tradedAt<expChange1 & tradeData$Set=="B"] <- "N"

tradeData$Active[tradeData$tradedAt<expChange3 &tradeData$tradedAt>=expChange2 & tradeData$Set=="A"] <- "Y"
tradeData$Active[tradeData$tradedAt<expChange3 &tradeData$tradedAt>=expChange2 & tradeData$Set=="B"] <- "N"

tradeData$Active[tradeData$tradedAt<expChange2 &tradeData$tradedAt>=expChange1 & tradeData$Set=="A"] <- "N"
tradeData$Active[tradeData$tradedAt<expChange2 &tradeData$tradedAt>=expChange1 & tradeData$Set=="B"] <- "Y"

tradeData$Active[tradeData$tradedAt>=expChange3 & tradeData$Set=="A"] <- "N"
tradeData$Active[tradeData$tradedAt>=expChange3 & tradeData$Set=="B"] <- "Y"

rsqNorm <- rsq

for (t in 1:length(tatData)){
#for (t in 1:5){
  print(tatData[t])
  tradeData$thResolvedAt[t] <- raqData[qiqData==qitData[t]]
  tradeData$thResolveValue[t] <- as.character(rvqData[qiqData==qitData[t]])
  
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvqData[qiqData==qitData[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  qnClass[t] <- as.character(clqData[qiqData==qitData[t]])
  
    
  source("Incentive Accuracy Mechanics Basic 150221.R")
  #print(brier[t])
  tradeData$thBrier[t] <- brier[t]
  
  tradeData$timeToRes[t] <- difftime(tradeData$thResolvedAt[t],tradeData$tradedAt[t],units="days")

  if (is.na(tradeData$Set[t])) {
    firstActive[t] <- NA
  } else if (tradeData$Set[t]=="A") {
   firstActive[t] <- as.POSIXct("2014-11-07")
  } else {
    firstActive[t] <- as.POSIXct("2014-12-07")
    }
  tradeData$timeSince1stActive[t] <- difftime(tradeData$tradedAt[t],firstActive[t],units="days")
  
}
  
tradeData$timeSince1stActive[tradeData$timeSince1stActive<0]<- NA


tradeData[complete.cases(tradeData),]

duration <- as.double(difftime(Sys.time(),startBrier,units="sec"))   #reports time to retrieve files
print("Brier data calculations Complete")
print(duration)

#write.table(tradeData,file="tradeData.csv",sep=",",append=F,col.names=c("questionId","tradeId","userId","tradedAt","newValue","oldValue,"resolvedAt","resolveValue","Brier","set","active", "timeToRes","timeSinceActive"),row.names=F)
