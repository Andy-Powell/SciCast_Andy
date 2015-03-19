###########################################################
## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.
###########################################################
#
# First run Get_Data.R.

start <- Sys.time() 
print("Active Accuracy calculations started")

#lp <- length(pip)
raqExp <- as.POSIXct(rep(Sys.time(), length(rsq)))


# Market Accuracy
# Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
acquAct <- acun <- acop <- nfqu <- acquAct <- rep(2,length(rsq)); pocosAct <- pocou <- pocoop <- hitAct <- hitop <- rep(0,length(rsq))
ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
 base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
#for (q in 36:36) {
  ra[q] <- raq[qiq==rsq[q]]
  raqExp[q] <- raq[qiq==rsq[q]]
# Uses as question start date the first day on which there  was a valid safe mode forecast placed!
  astart <- min(tat[qit==rsq[q] &asqt<0])
  w <- which(tat%in%tat[tat>=expStart &tat<expStop &qit==rsq[q] &asqt%in%c(-1,rsq) &asot==roqat])
  time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
  lt <- length(time); nfqu[q] <- lt-1
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  ac <- acd <- act <- rep(2,lt); pocot <- hitt <- rep(0,lt)
# Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
  acd[1] <- time[1]-base -(time[1]-60*60-base)
  pocot[1] <- pocou[q] <- 1/length(tmp1)
  if (lt>1) {
    for (t in 1:(lt-1)) {
     acd[t+1] <- time[t+1]-base -(time[t]-base)
#     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     b <- which(tmp1==max(tmp1))
    }
  }

  if (clq[qiq==rsq[q]]=="ordered multinomial") {
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
    }
    act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)
    #print(actt)
    #print(acqu[q])
    if (lt>1) {
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
        #print(tmp2)
        actt <-rep(0,length(tmp1)-1)
        for (o in 1:(length(tmp1)-1)) {
          actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
        }
      act[t+1] <- sum(actt)/(length(tmp1)-1)
      if (length(tmp1)>2) {
        pocot[t+1] <- mean(tmp2[b])
        if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
      } else {
          pocot[t+1] <- NA
          hitt[t+1] <- NA
        }
      }
    }
   #print (c("ordered",tmp1,"-", act[t]))
  }

  if (clq[qiq==rsq[q]]=="unordered multinomial") {
      act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
    if (lt>1) {
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
        act[t+1] <- sum( (tmp2-tmp1)^2 )
        pocot[t+1] <- mean(tmp2[b])
        if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
      }
    }
    #print (c("unordered",tmp1,"-", act[t]))
  }


  if (clq[qiq==rsq[q]]=="binary") {
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
   if (lt>1) {
    for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     act[t+1] <- sum( (tmp2-tmp1)^2 )
     pocot[t+1] <- mean(tmp2[b])
     if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
   }
   #print (c("binary",tmp1,"-", act[t]))
  }

  if (clq[qiq==rsq[q]]=="scaled") {
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
    if (lt>1) {
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
        act[t+1] <- sum( (tmp2-tmp1)^2 )
        pocot[t+1] <- mean(tmp2[b])
        if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
      }
    }
    #print (c("scale",tmp1,"-", act[t]))
  }
  
  if (clq[qiq==rsq[q]]=="shares") {
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
    if (lt>1) {
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
        act[t+1] <- sum( (tmp2-tmp1)^2 )
        pocot[t+1] <- mean(tmp2[b])
        if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
      }
    }
    #print (c("scale",tmp1,"-", act[t]))
  }

  acquAct[q] <- sum(act*acd)/sum(acd)
  pocosAct[q] <-sum(pocot*acd)/sum(acd)
  hitAct[q] <- sum(hitt*acd)/sum(acd)
}


duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print ("Active Accuracy calcuations Complete")
print(duration)

