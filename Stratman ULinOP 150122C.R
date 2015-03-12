w# if ordered (opq==2), length(temp1)==2 -> "binary ordered (one Q?) or scaled (usually)"
# if non-ordered (orq==1), length(temp1)==2 -> "binary"; length(temp1)>2 ->  multi 
#
# RPS
# if ordered, calculate cumulative dist score  (works for binary/ordered -> scaled continuous)(for scaled questoins, resoluton is a mixture of %0 and %1)
# brier score for the time before the first forecast
#
# Running separate loops for ULinOP (ULinOP(unweighted Linear Opinoin Pool) is the average of the last forecasts from all users for each question)
# all the same except for w, variables have op suffix

#questionUserFrcstULinOP <- array(0,nrow=nsmfq, ncol=length(days))   ###not yeat defined
qstnDayAvgFrcstULinOP <- matrix(0,nrow=length(rsq),ncol=length(days))
qstnDayBrierULinOP <- matrix(0,nrow=length(rsq),ncol=length(days))
qstnAvgBrierULinOP <- rep(0,length(rsq))
#qstnDayAvgFrcstULinOP[0] <- 0

ra <- rep(tstart,length(rsq))
for (q in 1:length(rsq)) {
#for (q in 1:1) {                    #tetsting only
  print(rsq[q])
  #for (q in 1:2) {     #for testing
  # ra[q] <- saq[qiq==rsq[q]]
  ra[q] <- raq[qiq==rsq[q]]
  # Uses as question start date the first day on which there  was a valid safe mode forecast placed!
  astart <- trunc(min(tat[qit==rsq[q] &mdt==1 &asqt<0]),"days")
  w <- which(tat%in%tat[tat>=astart &qit==rsq[q] &asqt<0 &mdt==1])		# For Steve Stratman, do not include conditional forecasts.
      # mdt==1 -> safemode forcasts
      # asqt<0 -> -1 incicates no contitional assumptions (not a conditioanl trade)
  # w <- which(tat%in%tat[qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat&mdt==1]) #&mdpt==1&fput>1	# Include safe-mode forecasts from users who prefer power mode? Who haven't made many forecasts?
  time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
 
  ## dividing trade_history into days ##
  #days <- seq(1,ceiling(as.double(tstop - astart)),1)
  numTradesULinOP <- rep(0,length(days))
  
  a <- 1
  #for (t in 1:length(tat)) {
  for (t in 1:length(w)) {             #only need to loop through trades specific to each question
    #print(t)
    #print(difftime(tradeDate[a], trunc(tat[t], "days"))) 
    #d2 <- c(tradeDate[a],as.POSIXct(trunc(time[t],"days")),difftime(tradeDate[a], as.POSIXct(trunc(time[t], "days"))))
    #d3 <- c(tradeDate[a],as.POSIXct(trunc(tat[t],"days")),difftime(as.POSIXct(trunc(tat[t])), tradeDate[a], "days"))
    #print(difftime(as.POSIXct(trunc(tat[t], "days")),tradeDate[a]))
    #print(d2)
    #print(d3)
    #if(difftime(tradeDate[a], as.POSIXct(trunc(tat[t], "days"))) > 0) {
    while(difftime(as.POSIXct(trunc(time[t],"days")),tradeDate[a]) > 0 & a<=length(tradeDate)) {   #if all remaining forecasts are later than next tradeDate, increment tradeDate unitil reaches next forecast
      #m1 <- c("no trades",as.Date(tradeDate[a]),t,a)
      #print(m1)
      numTradesULinOP[a] <- 0    #since no forecasts this day, make sure number trade  = 0
      a <- a+1
    }
    if (difftime(as.POSIXct(trunc(time[t], "days")),trunc(tradeDate[a],"days")) < 1) {    #if there is a trade on that tradeDate....
      numTradesULinOP[a] <- numTradesULinOP[a]+1                                          #increment the number trades on that tradeDate
      #m2 <- c("Trades=", numTradesULinOP[a],"difftime=",difftime(as.POSIXct(trunc(time[t], "days")),trunc(tradeDate[a],"days")),t,a)
      #print(m2)
      if (trunc(time[t+1], "days")!=tradeDate[a]) {                                       #if the next forecast in on another day, goto the next tradeDay
        a <- a+1
        #print("new day")
      }
    }
  }
  
  ### loop with daily trades ###
  
  lt <- length(time)
  
  nfqu[q] <- lt-1
  #weight <- rep(1,lt-1)   #weights used to select latest forecasts, weights are 0/1
  weight <- rep(0,lt-1)
    tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
    stv <- acd <- actop <- rep(2,lt); pocotop <- hittop <- rep(0,lt)
     # Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
     # I doubt Stratman includes this one hour, but it makes little difference and helps with our algorithm.
     acd[1] <- time[1]-base -(time[1]-60*60-base)   #acd is the weight
     pocotop[1] <- 1/length(tmp1)
  
  ltStop <- 0
  ltStart <- 1
  print("start")
  
  for (d2 in 1:length(numTradesULinOP)) {          #determining
  #for (d2 in 1:150) {
    if (numTradesULinOP[d2]==0) {
      #print("skip")
      if (d2>1) {
        qstnDayAvgFrcstULinOP[q,d2] <- qstnDayAvgFrcstULinOP[q,d2-1]
        qstnDayBrierULinOP[q,d2] <- qstnDayBrierULinOP[q,d2-1]
      }  else {
          qstnDayAvgFrcstULinOP[q,d2] <- 0
          }
        next
    }
  #print(orq[qiq==rsq[q]])

  # ordered questions - not binary (should not impact ULinOP)                           
  if (orq[qiq==rsq[q]]==2) {                      #if the question is ordered
    acttop <-rep(0,length(tmp1)-1)                 #
    for (o in 1:(length(tmp1)-1)) {
      acttop[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
    }
    # svt uses the binned safe mode forecasts
    # instead if temp2, temp0 is matrix of svt
    actop[1] <- sum(acttop)/(length(tmp1)-1)
    if (lt>1) {
      for (t in 1:ltStop+numTradesULinOP[d2]) {
        tmp0 <- matrix(svt[w,],c(length(w),40))  # pullout rows that match forcasts(w) and max 40 options(columns) for a question
        
        if (t<2) {                              # first forecast
          tmp3 <- tmp0[or[t],1:length(tmp1)]     # excludes excess options   #### or calculated in previous section, same for w  ######
        }
        else {  					 					            # for all later forecasts, ULinOP based on safe mode
          w2 <- which(pit[w]==pit[w][or[t]])     # all forecasts on this question made by user who made this forecast (or[t])
          lw2 <- length(w2)                      # number of forcasts
          if (lw2>1) {                           # person made more than one forecast on this question
            weight[w2] <- 0                      # all weights are 0 for that person's forecasts
            weight[or[t]] <- 1                   # all weight is out on the current forecast (going through by time)
          }		# Removing older forecasts from the same user
          tmp3 <- colSums(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]])/sum(weight[or[1:t]])  # current ULinOP estimate
          # tmp0[or[1:t],1:length(tmp1)] - all safe mode forcasts up to now
          # multiplying by weight[or[1:t]]
          # Here is where there was a mistake earlier.  Ken was using column means, which meant that forecasts without weight (old forecasts) were still contributing to the denominator.  Sorry.
        }
        # Same as previosu section except tep2 repalced by temp3
        acttop <-rep(0,length(tmp1)-1)
        for (o in 1:(length(tmp1)-1)) {
          acttop[o]<-2*(sum(tmp3[1:o])-sum(tmp1[1:o]))^2
        }
        actop[t+1] <- sum(acttop)/(length(tmp1)-1)
        if (length(tmp1)>2) {
          pocotop[t+1] <- mean(tmp3[b])
          if (mean(which(tmp3==max(tmp3)))%in%b) {hittop[t+1] <- 1}
        }
        else {
          pocotop[t+1] <- NA
          hittop[t+1] <- NA
        }
        
      }
    }
  }
  
  
    # non-ordered questions ("binary" & "unordered multi")
    if (orq[qiq==rsq[q]]==1) {
      actop[1] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
     if (lt>1) {
       
       ltStop <- ltStop+numTradesULinOP[d2]
       i2 <- c( ltStart,d2, numTradesULinOP[d2],ltStop)
       #print(i2)
       for (t in 1:ltStop) {                            # t is the current forecast ####### why stp?  Wrong t
       #tmp0 <- matrix(svt[w,],c(length(w),40))          #length(w) rows x 40 columns with new values (there will be excess columns)
       tmp0 <- rst[w] 
#       tmp0[3] <- 0.3
        w2 <- which(pit[w]==pit[w][or[t]])             # all forecasts on this question made by user who made this forecast (or[t])
        lw2 <- length(w2)                              # number of forecasts
        if (t<2) {                              # first forecast
         weight[w2] <- 1
          #tmp3 <- tmp0[or[t],1:length(tmp1)]     # excludes excess options   #### or calculated in previous section, same for w  ######
         tmp3 <- tmp0[or[t]]
         #print(c(t,lw2, tmp0[or[t]],tmp3, weight[ltStart:ltStop]))
       }
       else {
       if (lw2>0) {                                   # if more than one forecast.......
          weight[w2] <- 0                              # Remove all forecasts from the same user(w2)   **other user's weights are nto affected**
          weight[or[t]] <- 1                           # put back in the latest forecast from this user 
#          weight[2] <- 0
        }  
        #tmp3 <- colSums(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]])/sum(weight[or[1:t]]) 
       tmp3 <- sum(tmp0[or[1:t]]*weight[or[1:t]])/sum(weight[or[1:t]])
       #print(c("t=",t,"lw2=",lw2,"rst[t]=", rst[t],"tmp3=",tmp3,sep=""))
       #print(c(t,lw2, tmp0[or[t]],tmp3, weight[ltStart:ltStop]))
        # colMeans(tmp0[or[1:t],1:length(tmp1)])
        # Here is where there was a mistake earlier.  Ken was using column means, which meant that forecasts without weight (old forecasts) 
        # were still contributing to the denominator.  Sorry.
        # tmp0[or[1:t] -> all the forecasts up to time t
        # t:length(tmp1)] -> truncated to length(tmp1)
        # weight[or[1:t]] ->  weight vector for all users (0s) beyond this time
        }
       stv[t+1] <- tmp3[2]
#       actop[t+1]<-sum( (tmp3[t]-tmp1)^2 )  # sum of squares (cut tmp3 to length(temp1))  Why do this for each timestep?
#       actop[t+1]<-2*(tmp3[2]-tmp1[2])^2        # sum of squares (binary only)
       actop[t]<-2*((tmp3-tmp1[2])^2)       # sum of squares (binary ULinOP only)
       pocotop[t+1] <- mean(tmp3[b])
       if (mean(which(tmp3==max(tmp3)))%in%b) {hittop[t+1] <- 1}
      }
      qstnDayAvgFrcstULinOP[q,d2] <- tmp3
      qstnDayBrierULinOP[q,d2] <- actop[ltStop]
      ltStart <- ltStop +1
     }
    }
    numDays <- ceiling(as.double(tstop - astart))-1
    

  }
  #avgNumDays <- as.double(trunc(difftime(raq[qiq==rsq[t]],astart)-1,"days"))
  avgStart <- as.double(trunc(difftime(astart,tstart)+1,"days"))
  #avgEnd <- as.double(trunc(difftime(raq[qiq==rsq[t]],tstart)+1,"days"))
  avgEnd <- ceiling(as.double(raq[qiq==rsq[q]] - tstart))
  avgNumDays <- ceiling(avgEnd-avgStart)+1
  #qstnAvgBrierULinOP[q] <- sum(qstnDayBrierULinOP[q,avgStart:avgEnd])/(avgNumDays-1)
  acop[q] <- sum(qstnDayBrierULinOP[q,avgStart:avgEnd])/(avgNumDays)
  pocoop[q] <-sum(pocotop*acd)/sum(acd)
  hitop[q] <- sum(hittop*acd)/sum(acd)
}

