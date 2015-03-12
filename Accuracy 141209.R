## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.

#
# First run Get_Data.R.

start <- Sys.time() 
print("Accuracy and Uniform calculations started")



#lp <- length(pip)

# Market Accuracy
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".
# ONLY binary for Stratman.


ctq <- qn$categories; orq <- qn$is_ordered; orq <- as.double(orq); rvq <- qn$resolution_value_array; svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40)); roqt <- roqat <-rep(-1,length(tat))
#rsq <- levels(factor(qiq[raq<=Sys.time()&caq>tstart]))

# Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
acqu <- acun <- acop <- nfqu <- rep(2,length(rsq)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsq)); ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
 base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
#for (q in 85:85) {
 ra[q] <- raq[qiq==rsq[q]]
# Uses as question start date the first day on which there  was a valid safe mode forecast placed!
 #astart <- min(tat[qit==rsq[q] &mdt==1 &asqt<0])
 astart <- min(tat[qit==rsq[q] &asqt<0])
 w <- which(tat%in%tat[tat>=astart &qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat])
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
     tmp2 <- as.double(strsplit(as.vector(nvt[w])[or[t]],",")[[1]])
     b <- which(tmp1==max(tmp1))
    }
   }

  if (clq[qiq==rsq[q]]=="ordered multinomial") {
     actt <-rep(0,length(tmp1)-1)
   for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
   }
   act[1] <- acun[q] <- sum(actt)/(length(tmp1)-1)
   print(actt)
   print(acun[q])
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
    }
    else {
     pocot[t+1] <- NA
     hitt[t+1] <- NA
    }
   }
   }
   print (c("ordered",tmp1,"-", acun[q]))
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
  print (c("unordered",tmp1, acun[q]))
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
   print (c("binary",tmp1, acun[q]))
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
  print (c("scale",tmp1, acun[q]))
}
  #print (c(tmp1, acqu[q]))
  acqu[q] <- sum(act*acd)/sum(acd)
  pocos[q] <-sum(pocot*acd)/sum(acd)
  hit[q] <- sum(hitt*acd)/sum(acd)
}

duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print ("Accuracy and Uniform calcuations Complete")
print(duration)

acqum <- mean(acqu); acunm <- mean(acun); acopm <- mean(acop)		# SciCast and Uniform distribution BS
pocosm<-mean(pocos,na.rm=T); pocoopm<-mean(pocoop,na.rm=T); pocoum<-mean(pocou,na.rm=T)	# Percentage on correct option
hitm <- mean(hit,na.rm=T); hitopm <- mean(hitop,na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely

winaU <- round(100*(length(acqu[acqu<acun])/length(acqu)))		# Percentage of time SciCast better than uniform
winaO <- round(100*(length(acqu[acqu<acop])/length(acqu)))		# Percentage of time SciCast better than opinion pool
impoU <- round(100*(acunm-acqum)/acunm)
impoO <- round(100*(acopm-acqum)/acopm)

br <- seq(0,2,0.1)
png("AcpQaU.png", width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(acqu,breaks=br)
two <- hist(acun,breaks=br)
plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
plot(one,col=rgb(1,0,0,0.5),add=T)
 text(0.8,14,pos=4,paste("Uniform Distribution of Forecasts, mean = ",round(acunm,2),sep=""),col=rgb(0,0,1,0.6))
 text(0.8,12,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
 mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Better on ',winaU,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Overall score improved ',impoU,'%',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

#br <- seq(0,2,0.1)
#png("AcpQaO.png", width = 3600, height = 3600, pointsize = 18, res = 360)
#one <- hist(acqu,breaks=br)
#two <- hist(acop,breaks=br)
#plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
#plot(one,col=rgb(1,0,0,0.5),add=T)
# text(0.8,14,pos=4,paste("Safe-Mode Forecasts, mean = ",round(acopm,2),sep=""),col=rgb(0,0,1,0.6))
# text(0.8,12,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
# mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
# mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
# mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
#dev.off()

# Outputs table for easier comparison with Stratman's results.
ru <- rf <- rc <- rep(0,length(rsq))
for (q in 1:length(rsq)) {
 ru[q] <- length(unique(pit[qit==rsq[q]]))
 rf[q] <- length(tat[qit==rsq[q]])
 rc[q] <- length(cac[qic==rsq[q]])
}

write.table(data.frame(rsq,ra,acqu,acop,ru,rf,rc),file="Ken's Brier Scores.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)