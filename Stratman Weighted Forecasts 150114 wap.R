				

# Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
acqu <- acun <- acop <- nfqu <- rep(2,length(rsq)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsq)); ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
 base <- tstart-28*24*60*60

print(rsq)

for (q in 1:length(rsq)) {
 ra[q] <- raq[qiq==rsq[q]]
# Uses as question start date the first day on which there  was a valid safe mode forecast placed!
 astart <- min(tat[qit==rsq[q] &mdt==1 &asqt<0])
 w <- which(tat%in%tat[tat>=astart &qit==rsq[q] &asqt%in%c(-1,rsq) &asot==roqat])   # selects which forecasts   roqat is correct option on assumtion question 
 #which selects those traded_ats which meet the conditions
 time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
 # time is all the traded_ats and the time question resolved
 
## dividing tradehistory into days ##
#
#questionDataWeighted <- matrix(0,nrow=length(rsq),ncol=length(days))

#numTrades <- rep(0,length(days))
#a <- 1
#for (t in 1:length(w)) {             ### testing only ###
#  print(t)
#  #print(difftime(tradeDate[a], trunc(tat[t], "days"))) 
#  d2 <- c(tradeDate[a],as.POSIXct(trunc(time[t],"days")),difftime(tradeDate[a], as.POSIXct(trunc(time[t], "days"))))
#  #d3 <- c(tradeDate[a],as.POSIXct(trunc(tat[t],"days")),difftime(as.POSIXct(trunc(tat[t])), tradeDate[a], "days"))
#  #print(difftime(as.POSIXct(trunc(tat[t], "days")),tradeDate[a]))
#  #print(d2)
#  #print(d3)
#  #if(difftime(tradeDate[a], as.POSIXct(trunc(tat[t], "days"))) > 0) {
#  while(difftime(as.POSIXct(trunc(time[t],"days")),tradeDate[a]) > 0) {
#    m1 <- c("no trades",as.Date(tradeDate[a]),t,a)
#    #print(m1)
#    numTrades[a] <- 0
#    a <- a+1
#  }
#  if (difftime(as.POSIXct(trunc(time[t], "days")),trunc(tradeDate[a],"days")) < 1) {
#    numTrades[a] <- numTrades[a]+1
#    m2 <- c("Trades=", numTrades[a],"difftime=",difftime(as.POSIXct(trunc(time[t], "days")),trunc(tradeDate[a],"days")),t,a)
#    print(m2)
#    if (trunc(time[t+1], "days")!=tradeDate[a]) {
#      a <- a+1
#      print("new day")
#    }
#  }
#}


lt <- length(time)
nfqu[q] <- lt-1
 # nfqu - number ofr forcasts  lt - ra[q]


  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  #vector of resoluton values   should be all 0s except for one 1  (non mixtures)
  ac <- acd <- act <- rep(2,lt); pocot <- hitt <- rep(0,lt)
# Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
   acd[1] <- time[1]-base -(time[1]-60*60-base)  #acd[1]  = 1 hour
   pocot[1] <- pocou[q] <- 1/length(tmp1)    #percent on correct option ?broken?
   if (lt>1) {
    for (t in 1:(lt-1)) {
     acd[t+1] <- time[t+1]-base -(time[t]-base)
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     b <- which(tmp1==max(tmp1))   #for mixtrue resolutoins max(tmp1) amy not be 1
    }
   }
  
  # if ordered (opq==2), length(temp1)==2 -> "binary ordered (one Q?) or scaled (usually)"
  # if non-ordered (orq==1), length(temp1)==2 -> "binary"; length(temp1)>2 ->  multi 

  # RPS
  # if ordered, calculate cumulative dist score  (works for binary/ordered -> scaled continuous)(for scaled questoins, resoluton is a mixture of %0 and %1)
  # brier score for the time before the first forecast
  if (orq[qiq==rsq[q]]==2) {     # if an ordered question
   actt <-rep(0,length(tmp1)-1)  # --1 because no point in calculating score for last option
   for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2  # special case for first forcast - why doubling non-binary?
   }
   act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)   # average of scores before teh first forecast
   # acun[q] uniform dist for each question
  if (lt>1) {    
   for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])  #new forcast value
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
     actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
    }
    act[t+1] <- sum(actt)/(length(tmp1)-1)
    
    # for multi questions
    if (length(tmp1)>2) {
     pocot[t+1] <- mean(tmp2[b]) # pocot=% on correct option, new prob on the resolved option
     # hit rate
     if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}  # does the person's highest prob match resolved max probablity
    }
    else {
     pocot[t+1] <- NA  # don't know why - posible broken here?
     hitt[t+1] <- NA
    }
   }
   }
  }
  # non-ordered questions ("bianry" & "unordered multi")
  if (orq[qiq==rsq[q]]==1) {  #orq==1 is non-ordered,  all "binary" are ordered
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2  # uniform before first forecast
    print(acun[q])
   if (lt>1) {                        # at least one forecast
    for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     act[t+1] <- sum( (tmp2-tmp1)^2 )  #squared error
     pocot[t+1] <- mean(tmp2[b])
     if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
   }
  }
  acqu[q] <- sum(act*acd)/sum(acd)  # squared error * time-weight)/total time weight
  pocos[q] <-sum(pocot*acd)/sum(acd)
  hit[q] <- sum(hitt*acd)/sum(acd)   #hit*  are hit rate for different calcsw ?broken?
}

