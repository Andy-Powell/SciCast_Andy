###############################################
#
#
#
##############################################

start <- Sys.time() 
print("Last Trade calculations started")

tatData<-tat; titData<-tit; pitData<-pit; qitData<-qit; nvtData<-nvt; ovtData<-ovt; astData<-ast; #apotData<-apot
citData<-cit;# rstData<-rst; mdtData<-mdt; asqtData<-asqt; asotData<-asot

questionId <- as.integer(rep(NA,length(qiq)))
lastTrade  <- as.POSIXct(rep(NA,length(qiq)))
firstTrade <- as.POSIXct(rep(NA,length(qiq)))
oldValue <- as.character(rep(NA,length(qiq)))
newValue <- as.character(rep(NA,length(qiq)))
resolvedAt <- as.POSIXct(rep(NA,length(qiq)))
#resolveValue <- as.character(rep(NA,length(qiq)))


tradeData <- data.frame(questionId,lastTrade,firstTrade,oldValue,newValue,stringsAsFactors=FALSE)
#tradeData <- data.frame(questionId,lastTrade,firstTrade,oldValue,newValue,resolvedAt,resolveValue,stringsAsFactors=FALSE)
#tradeData <- data.frame(questionId=integer(),lastTrade=as.POSIXct(numeric()),firstTrade=as.POSIXct(numeric),oldValue=as.Character(),newValue=as.Character(resolvedAt=POSIXct(),resolveValue=as.Character(),stringsAsFactors=FALSE)
  
x <- 0
for (q in sort(qiq)) {
#for (q in 630:630) {p
  x <- x+1
  #tradeDataNow <- tradeData[tradeData$questionId==q,c("questionId","lastTrade","firstTrade","oldValue","newValue")]
  tatNow <- tatData[qit==q]
  #print(c(q,length(tradeDataNow)))
  if (length(tatNow)< 1)  {
    tradeData$questionId[x] <- q
    next
  }
  
  ovtNow <- ovtData[qit==q]
  nvtNow <- nvtData[qit==q]
  
  firstTradeNum <- which(tatNow==min(tatNow))
  lastTradeNum <- which(tatNow==max(tatNow))
  
  tradeData$questionId[x] <- q
  tradeData$firstTrade[x] <- tatNow[firstTradeNum]
  tradeData$lastTrade[x] <- tatNow[lastTradeNum]
  tradeData$newValue[x] <- as.character(nvtNow[lastTradeNum])  
  tradeData$oldValue[x] <- as.character(ovtNow[firstTradeNum])

  #tradeData$resolvedAtData[x] <- saq[qiq==q]
  #tradeData$resolveValue[x] <- rvq[qiq==q]
  #print(q)
}



write.table(tradeData,file="tradeData.csv",sep=",",append=F,col.names=c("questionId","lastTrade","firstTrade","oldValue","newValue"),row.names=F)

print("Last Trade calculations started")
duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print("Last Trade calculations completed")
print(duration)