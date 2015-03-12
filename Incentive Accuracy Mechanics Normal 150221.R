


for (t in 1:(lt-1)) {                                             # For all trades....
  acd[t+1] <- difftime(time[t+1],time[t],units="days")            # weight going forward (since first weight is aloread known) is the time betseen trades
  b <- which(tmp1==max(tmp1))                                     # b is the max of all the resolution values
}
#  }

print(clq[qiq==rsqNorm[q]])

if (clq[qiq==rsqNorm[q]]=="ordered multinomial") {
  actt <-rep(0,length(tmp1)-1)
  #for (o in 1:(length(tmp1)-1)) {
  #  actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
  #}
  for (t in 1:(lt-1)) {
    print("o1")
    tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
    #print(tmp2)
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
      actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
    }
    act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)
    
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
}   

if (clq[qiq==rsqNorm[q]]=="unordered multinomial") {
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum((tmp2-tmp1)^2)
  pocot[1] <- mean(tmp2[b])
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      print("u1")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      pocot[t+1] <- mean(tmp2[b])
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
  if (lt>1) {
    for (t in 1:(lt-1)) {
      print("b1")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      pocot[t+1] <- mean(tmp2[b])
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
  #print (c("binary",tmp1,"-", act[t]))
}

if (clq[qiq==rsqNorm[q]]=="scaled") {
  #act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(ovt[w])[or[1]],",")[[1]])
  act[1] <- acun[q] <- sum((tmp2-tmp1)^2)
  pocot[1] <- mean(tmp2[b])
  if (lt>1) {
    for (t in 1:(lt-1)) {
      print("s1")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
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
  
  if (lt>1) {
    for (t in 1:(lt-1)) {
      print("s2")
      tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
      act[t+1] <- sum( (tmp2-tmp1)^2 )
      pocot[t+1] <- mean(tmp2[b])
      if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
  }
  #print (c("scale",tmp1,"-", act[t]))
}

#print (c(tmp1, acqu[q]))
 