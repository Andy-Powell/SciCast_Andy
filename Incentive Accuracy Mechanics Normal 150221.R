############################################################
#
#  Generates Brier score for all five types of questions
#  USed only for quesions wiht 2 or more trades
#  needs rsqNorm, tme, qiq, clq, tmp1, ovt, nvt, 
#
#
###########################################################





setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")


for (t in 1:(lt-1)) {                                             # For all trades....
  acd[t+1] <- difftime(time[t+1],time[t],units="days")            # weight going forward (since first weight is aloread known) is the time betseen trades
  b <- which(tmp1==max(tmp1))                                     # b is the max of all the resolution values
}
#  }

#print(c(q,qiq[q],clq[qiq==rsqNorm[q]],lt))
#print(c("tmp1",tmp1))

if (clq[qiq==rsqNorm[q]]=="ordered multinomial") {
  actt <-rep(0,length(tmp1)-1)
  #print("o1")
  
  for (t in 1:(lt-1)) {
    tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
    #print(c("o1",tmp2))
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
      actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
    }
    act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)
    
    if (lt>1) {
      for (t in 1:(lt-1)) {
        tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
        actt <-rep(0,length(tmp1)-1)
        for (o in 1:(length(tmp1)-1)) {
          actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
        }
        act[t+1] <- sum(actt)/(length(tmp1)-1)
        #print(c("o2",tmp2, act[t+1]))
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
}   

if (clq[qiq==rsqNorm[q]]=="unordered multinomial") {
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum((tmp2-tmp1)^2)
  pocot[1] <- mean(tmp2[b])
  #print(c("u1",tmp2,act[1]))
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      pocot[t+1] <- mean(tmp2[b])
      #print(c("u2",tmp2,act[t+1]))
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
  #print (c("unordered",tmp1,"-", act[t]))
}


if (clq[qiq==rsqNorm[q]]=="binary") {
  #act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum( (tmp2-tmp1)^2 )
  pocot[1] <- mean(tmp2[b])
  #print(c("b1",tmp2,act[1]))
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      #print("b1")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      pocot[t+1] <- mean(tmp2[b])
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
 # print (c("binary",tmp1,"-", act[t]))
}

if (clq[qiq==rsqNorm[q]]=="scaled") {
  #act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum((tmp2-tmp1)^2)
  pocot[1] <- mean(tmp2[b])
  #print(c("s1",tmp2,act[1]))
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      #print(c("s2",tmp2,act[t+1]))
      pocot[t+1] <- mean(tmp2[b])
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
  #print (c("scale",tmp1,"-", act[t]))
}

if (clq[qiq==rsqNorm[q]]=="shares") {
  #act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum( (tmp2-tmp1)^2 )
  pocot[1] <- mean(tmp2[b]) 
  #print(c("sh1",tmp2,act[t]))
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      #print("s2")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      #print(c("sh2",tmp2,act[t+1]))
      pocot[t+1] <- mean(tmp2[b])
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
  #print (c("scale",tmp1,"-", act[t]))
}

#lastTrade[m,q] <- tmp2

#print (c("Tmp2",tmp2))
 