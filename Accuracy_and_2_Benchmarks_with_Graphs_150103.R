#
# Market Accuracy (Updated on 2014-10-09)
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".

start <- Sys.time()

good <- complete.cases(tat)
sum(!good)     														# How many are not good?
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]; tit<-tit[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

# nowish <- strsplit(as.character(tstop), ' ')[[1]][1]
#asq <- aso <- rep("a",length(tat))
#for (t in 1:length(tat)) {
# asq[t] <- strsplit(as.character(as[t]),':')[[1]][1]
# aso[t] <- strsplit(as.character(as[t]),':')[[1]][2]
#}
#asqt <- as.double(asq)
#asot <- as.double(aso)
#asqt[is.na(asqt)==T] <- -1
#asot[is.na(asot)==T] <- -1

# Find resolved questions.
'%ni%' <- Negate('%in%')
gpq <- matrix(rep("a",length(qiq)*200),c(length(qiq),200)); vldq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
 tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]]); lv <- length(tmp)
 if (lv>0) {  gpq[q,1:lv] <- tmp }
 if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }
}
# How many are invalid?
length(vldq[vldq==0])

ctq <- qn$categories; orq <- qn$is_ordered; orq <- as.double(orq); rvq <- qn$resolution_value_array; svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40)); roqt <- roqat <-rep(-1,length(tat))
#rsq <- levels(factor(qiq[raq<=Sys.time()&caq>tstart]))
# Cleaning up question groups
wtg <- rep(0,length(qiq))
gq <- array(numeric(), c(length(qiq),200))
for (j in 1:length(qiq)) {
 if(grq[j]!="") {
  temp <- levels(factor(strsplit(as.character(grq[j]),",")[[1]]))
  wtg[j]<-1/length(temp); gq[j,1:(length(temp))]<- temp
 }
}

rsq <- levels(factor(qiq[saq<=Sys.time()&caq>tstart&ctq!="Study 2.1"&ctq!="Study 2.1,Study 2.1"&"Public"%in%gq&vldq==1]))	# Restrict to public questions.
 frc <- numeric(); rqb <- length(rsq)
 for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]&pit%in%pip[igrp==0]])}					# Removing questions that have almost no (non-internal) forecasts
 rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 
 rqb-rqa														# Number of low -activity questions
 hist(frc); quantile(frc,1-0.02); length(frc[frc>200])									# Number of high-activity questions

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

#tat <- as.POSIXct(th$traded_at); nvt <- th$new_value_list
# Weight forecasts by how long they endure. Average over questions.
acqu <- acun <- acop <- nfqu <- rep(2,length(rsq)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsq)); ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
 base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
#for (q in 1:30) {
# ra[q] <- saq[qiq==rsq[q]]
 ra[q] <- raq[qiq==rsq[q]]
 w <- which(tat%in%tat[qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat])
 time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
 lt <- length(time); nfqu[q] <- lt-1
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  ac <- acd <- act <- rep(2,lt); pocot <- hitt <- rep(0,lt)
# Pretend the first trade lasted 24 hours because we don't have a record of how long the questions were paused after being published.
   acd[1] <- time[1]-base -(time[1]-24*60*60-base)
   pocot[1] <- pocou[q] <- 1/length(tmp1)
   if (lt>1) {
    for (t in 1:(lt-1)) {
     acd[t+1] <- time[t+1]-base -(time[t]-base)
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     b <- which(tmp1==max(tmp1))
    }
   }

  if (orq[qiq==rsq[q]]==2) {
   actt <-rep(0,length(tmp1)-1)
   for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
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
    if (length(tmp1)>2) {
     pocot[t+1] <- mean(tmp2[b])
     if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
    else {
     pocot[t+1] <- NA
     hitt[t+1] <- NA
    }
   }
   }
  }
  if (orq[qiq==rsq[q]]==1) {
# Fix this because sometimes there are mixture resolutions!
    act[1] <- acun[q] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
   if (lt>1) {
    for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     act[t+1] <- sum( (tmp2-tmp1)^2 )
     pocot[t+1] <- mean(tmp2[b])
     if (mean(which(tmp2==max(tmp2)))%in%b) {hitt[t+1] <- 1}
    }
   }
  }
  acqu[q] <- sum(act*acd)/sum(acd)
  pocos[q] <-sum(pocot*acd)/sum(acd)
  hit[q] <- sum(hitt*acd)/sum(acd)
}

# Running separate loops for ULinOP

#fput <- rep(0,length(pit))
#for (t in 1:length(pit)) {
# fput[t] <- fpu[pip==pit[t]]
#}
ra <- rep(tstart,length(rsq))
for (q in 1:length(rsq)) {
# ra[q] <- saq[qiq==rsq[q]]
 ra[q] <- raq[qiq==rsq[q]]
 w <- which(tat%in%tat[qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat&mdt==1]) #&mdpt==1&fput>1	# Include safe-mode forecasts from users who prefer power mode? Who haven't made many forecasts?
 time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
 lt <- length(time); nfqu[q] <- lt-1; weight <- rep(1,lt-1)
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  acd <- actop <- rep(2,lt); pocotop <- hittop <- rep(0,lt)
# Pretend the first trade lasted 24 hours because we don't have a record of how long the questions were paused after being published.
   acd[1] <- time[1]-base -(time[1]-24*60*60-base)
   pocotop[1] <- 1/length(tmp1)
   if (lt>1) {
    for (t in 1:(lt-1)) {
     acd[t+1] <- time[t+1]-base -(time[t]-base)
     b <- which(tmp1==max(tmp1))
    }
   }

  if (orq[qiq==rsq[q]]==2) {
   acttop <-rep(0,length(tmp1)-1)
   for (o in 1:(length(tmp1)-1)) {
    acttop[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
   }
   actop[1] <- sum(acttop)/(length(tmp1)-1)
   if (lt>1) {
   for (t in 1:(lt-1)) {
     tmp0 <- matrix(svt[w,],c(length(w),40))
      if (t<2) {
       tmp3 <- tmp0[or[t],1:length(tmp1)]
      }
      else {
       w2 <- which(pit[w]==pit[w][or[t]]); lw2 <- length(w2); if (lw2>1) {weight[w2] <- 0; weight[or[t]] <- 1}		# Removing older forecasts from the same user
       tmp3 <- colMeans(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]]) # colMeans(tmp0[or[1:t],1:length(tmp1)])
      }						 					# ULinOP based on safe mode
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
  if (orq[qiq==rsq[q]]==1) {
    actop[1] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
   if (lt>1) {
    for (t in 1:(lt-1)) {
     tmp0 <- matrix(svt[w,],c(length(w),40))
      if (t<2) {
       tmp3 <- tmp0[or[t],1:length(tmp1)]
      }
      else {
       w2 <- which(pit[w]==pit[w][or[t]]); lw2 <- length(w2); if (lw2>1) {weight[w2] <- 0; weight[or[t]] <- 1}		# Removing older forecasts from the same user
       tmp3 <- colMeans(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]]) # colMeans(tmp0[or[1:t],1:length(tmp1)])
      }						 					# ULinOP based on safe mode

     actop[t+1]<-sum( (tmp3[1:length(tmp1)]-tmp1)^2 )
     pocotop[t+1] <- mean(tmp3[b])
     if (mean(which(tmp3==max(tmp3)))%in%b) {hittop[t+1] <- 1}
    }
   }
  }
  acop[q] <- sum(actop*acd)/sum(acd)
  pocoop[q] <-sum(pocotop*acd)/sum(acd)
  hitop[q] <- sum(hittop*acd)/sum(acd)
}

acqum <- mean(acqu); acunm <- mean(acun); acopm <- mean(acop)		# SciCast and Uniform distribution BS
pocosm<-mean(pocos,na.rm=T); pocoopm<-mean(pocoop,na.rm=T); pocoum<-mean(pocou,na.rm=T)	# Percentage on correct option
hitm <- mean(hit,na.rm=T); hitopm <- mean(hitop,na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely

winaU <- round(100*(length(acqu[acqu<acun])/length(acqu)))		# Percentage of time SciCast better than uniform
winaO <- round(100*(length(acqu[acqu<acop])/length(acqu)))		# Percentage of time SciCast better than opinion pool
impoU <- round(100*(acunm-acqum)/acunm)
impoO <- round(100*(acopm-acqum)/acopm)

br <- seq(0,2,0.1)

date <- Sys.Date()                                                                 #Adding Date to title
title <- paste("Accuracy_and_Uniform_Benchmark ",date,".png", collapse="")      #Expanding title name

png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(acqu,breaks=br)
two <- hist(acun,breaks=br)
plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
plot(one,col=rgb(1,0,0,0.5),add=T)
 text(0.5,65,pos=4,paste("Uniform Distribution of Forecasts, mean = ",round(acunm,2),sep=""),col=rgb(0,0,1,0.6))
 text(0.5,60,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
 mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Better on ',winaU,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Overall score improved ',impoU,'%',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

br <- seq(0,2,0.1)

title <- paste("Accuracy_and_Safe_Mode_Benchmark ",date,".png", collapse="")      #Expanding title name

png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(acqu,breaks=br)
two <- hist(acop,breaks=br)
plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
plot(one,col=rgb(1,0,0,0.5),add=T)
 text(0.5,65,pos=4,paste("Safe-Mode Forecasts, mean = ",round(acopm,2),sep=""),col=rgb(0,0,1,0.6))
 text(0.5,60,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
 mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Accuracy-and_2_Benchmarks graphs Competed")
print(duration)