



if (qnClass[t]=="ordered multinomial") {
  actt <-rep(0,length(tmp1)-1)
    tmp2 <- as.double(strsplit(as.vector(nvtData[t]),",")[[1]])
    #print(tmp2)
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
      actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
    }
    brier[t] <- sum(actt)/(length(tmp1)-1)
    

    #print (c("ordered",tmp1,"-", act[t]))
}   

if (qnClass[t]=="unordered multinomial") {
  tmp2 <- as.double(strsplit(as.vector(nvtData[t]),",")[[1]])
  brier[t] <- sum( (tmp2-tmp1)^2 )

  #print (c("unordered",tmp1,"-", act[t]))
}


if (qnClass[t]=="binary") {
  tmp2 <- as.double(strsplit(as.vector(nvtData[t]),",")[[1]])
  brier[t] <- sum( (tmp2-tmp1)^2 )
  #print (c("binary",tmp1,"-", act[t]))
}

if (qnClass[t]=="scaled") {
  #act[1] <- acun[t] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(nvtData[t]),",")[[1]])
  brier[t] <- sum( (tmp2-tmp1)^2 )

  #print (c("scale",tmp1,"-", act[t]))
}

if (qnClass[t]=="shares") {
  #act[1] <- acun[t] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
  tmp2 <- as.double(strsplit(as.vector(nvtData[t]),",")[[1]])
  brier[t]<- sum( (tmp2-tmp1)^2 )
  #print (c("scale",tmp1,"-", act[t]))
}


#print (c(tmp1, acqu[t]))


 