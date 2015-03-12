############################################
#
#   Finds Brier scores for questions that no trades
#
############################################

### seting weights by finding the time between trades
print(expStart)
print(tradeData$lastTrade[tradeData$questionId==qstn])

#if (expStart > tradeData$lastTrade[tradeData$questionId==qstn]){
if (difftime(expStart,tradeData$lastTrade[tradeData$questionId==qstn])>0) {

### setting act (brier score) before first trade ###

  tmp2 <- as.double(strsplit(as.vector(tradeData$newValue[tradeData$questionId==qstn])[or[1]],",")[[1]])
  print(tmp2)
  print(tmp1)
  actt <-rep(0,length(tmp1)-1)
  for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
  }
  #act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)
  act <- acun[q] <- sum(actt)/(length(tmp1)-1)
  print(act)
} else {
  act <- NA
  print(c("1",act))
}
    
    