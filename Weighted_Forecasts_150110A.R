start <- Sys.time() 
print("weighte forecast calculations started")

# Reordering to simplify other operations later (Analysis_Setup).
ord <- order(qit,tat)
tat<-tat[ord]; tit<-tit[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; as<-as[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]


frc <- numeric(); rqb <- length(rsq)
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q] &pit%in%pip[igrp==0]])}  				# Checking the total forecasts on each question included for analysis.
# th_traded_at() for th_user_id that are in the list of "appropriate" questions
# pit%in%pip[igrp==0] -> th_user_id is in list of pr_user_ids that are not internal users 
##### what happened to no admin or specifically exlcuded users? #######
rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 

# for multi questions   # possibly normalize remaining probablity using previous forcasts on other options - how close are other edits
for (t in 1:length(tat)) {
  temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  if (is.na(temp1[1])==F) {
    rvqt[t,1:length(temp1)] <- temp1
    if (mdt[t]>0) {
      dflt <- (1-rst[t])/(length(temp1)-1); svt[t,1:length(temp1)] <- rep(dflt,length(temp1))				# Assume non-attended options have uniform distribution.
      svt[t,(cit[t]+1)] <- rst[t]
    }
    if (sum(temp1%%1)==0) {													# Not mixture resolutions
      roqt[t] <- which(rvqt[t,]==1)-1
    }
  }
  if (asqt[t]%in%rsq) {
    temp2 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==asqt[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
    if (is.na(temp2[1])==F) {
      rvqat[t,1:length(temp2)] <- temp2
      if (sum(temp2%%1)==0) {
        roqat[t] <- which(rvqat[t,]==1)-1
      }
    }
  }
}




# Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
acqu <- acun <- acop <- nfqu <- rep(2,length(rsq))
pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsq))
ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
#for (q in 1:6) {      # fore testing
  ra[q] <- raq[qiq==rsq[q]]
  # Uses as question start date the first day on which there  was a valid safe mode forecast placed!
  #astart <- min(tat[qit==rsq[q] &mdt==1 &asqt<0])
  astart <- min(tat[qit==rsq[q]])
  w <- which(tat%in%tat[tat>=astart &qit==rsq[q] &asqt%in%c(-1,rsq) &asot==roqat])   # selects which forcasts   roqat is correct option on assumtion question 
  #which selects those time which meet the conditions
  time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
  # time is all the traded_ats and the time quesiotn resolved
  lt <- length(time); nfqu[q] <- lt-1
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
    act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)   # average of scores before the first forecast
    print (acun[q])
    # acun[q] uniform dist for each question

    if (lt>1) {    
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])  #new forcast value
        actt <-rep(0,length(tmp1)-1)
        #print(tmp2)
        for (o in 1:(length(tmp1)-1)) {
          actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
          #print(qiq[qiq==rsq[q]])
          #print(o)
          #print(sum(tmp2[1:o]))
          #print(sum(tmp1[1:o]))
          #print(actt[o])
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
  
  
  # non-ordered questions ("binary" & "unordered multi")
  if (orq[qiq==rsq[q]]==1) {  #orq==1 is non-ordered,  all "binary" are unordered
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2  # uniform before first forecast
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
  
  # SciCast and Uniform distribution BS
  acqum <- mean(acqu)  #sciCast
  acunm <- mean(acun)  #uniform
  #acopm <- mean(acop)   #ULinOP	
  
  pocosm<-mean(pocos,na.rm=T)  # Percentage on correct option
  #pocoopm<-mean(pocoop,na.rm=T)
  pocoum<-mean(pocou,na.rm=T)
  
  hitm <- mean(hit,na.rm=T)      # Also compare to pocoum.  # Percentage of time correct option is forecast as most likely
  #hitopm <- mean(hitop,na.rm=T)
  
  duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
  print ("Weighted Forecast calcuations Complete")
  print(duration)
  