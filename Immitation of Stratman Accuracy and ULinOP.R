## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.

#
# First run Get_Data.R.


#
# Removing admin accounts and activity
# Match to Steve's list!

pip <- pr$user_id; pus <- as.character(pr$username); cap <- as.POSIXct(pr$created_at); grps <- pr$groups; rip <-pr$referral_id

adu <- c("amsiegel","BAE11","brnlsl","brobins","cedarskye","christinafreyman","ctwardy","daggre_admin","dquere","gbs_tester","Inkling","jessiejury","jlu_bae","kennyth0","klaskey","kmieke","manindune","Naveen Jay","pthomas524","Question_Admin","Question Mark","randazzese","RobinHanson","saqibtq","scicast_admin","slin8","ssmith","tlevitt","wsun")
adi <- numeric()
for (i in 1:length(adu)) {
 adi[i] <- pip[pus==adu[i]]
 cap[pip==adi[i]] <- NA
}

grp <- array(rep("a",length(pip)*20),c(length(pip),20)); igrp <- rep(0,length(pip))
for (i in 1:length(pip)) {
 temp <- as.vector(strsplit(as.character(grps[i]),",")[[1]])
 grp[i,1:length(temp)] <- temp
}
adi <- numeric()
for (i in 1:length(pip)) {
 for (g in 1:20) {
  if (grp[i,g]=="Admin"|grp[i,g]=="SuperAdmin"|grp[i,g]=="UserAdmin"|grp[i,g]=="BadgesAdmin"|grp[i,g]=="RolesAdmin"|grp[i,g]=="QuestionAdmin") {
   cap[i] <- NA
   adi <- c(adi,pip[i])
  }
  if (grp[i,g]=="Internal") {										# Keeping but noting internal accounts!
   igrp[i] <- 1
  }
 }
}
adi <- unique(adi)

good <- complete.cases(cap)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grp<-grp[good,]; rip<-rip[good]

pit <- th$user_id; qit <- th$question_id; tat <- as.POSIXct(th$traded_at); nvt <- th$new_value_list; ovt <- th$old_value_list; as <- th$serialized_assumptions; apot <- th$assets_per_option; tit <- th$trade_id
cit <- th$choice_index; rust <- as.character(th$raw_user_selection)
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\",[0.9545454545454546,1],null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\",[0.9545454545454546,1],null]"
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\", [0.9545454545454546, 1], null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\", [0.9545454545454546,1], null]" 
rust[rust=="[\"\\\"Less than 4.75%\\\"\",[-0.33333333333333326,-0.33333333333333326],null]"] <- "None"
rust[rust=="[\"\\\"Between 4.75% and 5%\\\"\",[-0.33333333333333326,0.33333333333333326],\"Lower\"]"] <- "None"
rust[rust=="[\"\\\"Less than 4.75%\\\"\", [-0.33333333333333326, -0.33333333333333326], null]"] <- "None"
rust[rust=="[\"Down ~12% or more\",[-0.21428571428571427,0.5],null]"] <- "None"
rust[rust=="[\"Up ~12% or more\",[0.842857142857143,1.2142857142857142],null]"] <- "None"

rst <- mdt <- rep(0,length(rust))
#for (t in 1:39) {
for (t in 1:length(rust)) {
 m <- strsplit(rust[t],",")[[1]][1]
 if (m!="None") {
  mdt[t] = 1												# 1 indicates safe-mode forecast.
  tmp1 <- as.double(strsplit(as.vector(ovt[t]),",")[[1]])						# A later selection on SciCast.org like "Higher" will assume the user wants the forecast halfway between the current market estimate and the top of the bin.
  tmp2 <- strsplit(strsplit(rust[t],',')[[1]][4],'"',fixed=T)[[1]][2]
  if (is.na(tmp2)==T) { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
  if (is.na(tmp2)==F) { 
   if (tmp2=="Lower" ) { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(strsplit(rust[t],',')[[1]][2],',')[[1]][1],'[',fixed=T)[[1]][2]) -tmp1[cit[t]+1])/2 }
   if (tmp2=="Higher") { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)) -tmp1[cit[t]+1])/2 }
   if (tmp2=="What they are now") { rst[t] <- tmp1[cit[t]+1]}
   if (tmp2=="null") { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
  }
 }
}

for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA
}
good <- complete.cases(tat)
sum(!good)     												# How many are not good?
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]; tit<-tit[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

pic <- cm$user_id; cac <- as.POSIXct(cm$created_at); qic <- cm$question_id
for (i in 1:length(adi)) {
 cac[pic==adi[i]] <- NA
}
good <- complete.cases(cac)
sum(!good)     												# How many are not good?
cac<-cac[good]; pic<-pic[good]; qic<-qic[good]

lp <- length(pip)

##############
# Setup
##############
tstart <- as.POSIXct("2013-11-25 00:00:00 EST");  base <- tstart-28*24*60*60
# For Steve Stratman
tstop <- as.POSIXct("2014-11-30 00:00:00 EST")
days <- seq(1,ceiling(as.double(tstop - tstart)),1)

qiq <- qn$question_id; caq <- as.POSIXct(qn$created_at); grq <- as.character(qn$groups); saq <- as.character(qn$resolution_at); raq <- as.character(qn$pending_until)
#qn$provisional_settled_at is start of comment period and qn$pending_until will be reused for event resolution (not question resolution/settlement)
saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)
raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)
ls <- qn$relationships_source_question_id; ld <- qn$relationships_destination_question_id
ql <- qn$is_locked; qv <- qn$is_visible; qps <- qn$provisional_settled_at; pq <- qn$type
tpq <- rep(0,length(qiq)); tpq[pq=="binary"] <- 2; tpq[pq=="multi"] <- 3
ct <- qn$categories; orq <- qn$is_ordered; orq <- as.double(orq); rvq <- qn$resolution_value_array

drq <- as.double(raq-caq)
hist(drq) 										# Range of questions' duration

for (t in 1:length(tat)) {								# Removing forecasts that occurred after resolution was known.
 if (tat[t]>raq[qiq==qit[t]]) {
  tat[t] <- NA
 }
}
tat[tat>tstop] <- NA
good <- complete.cases(tat)
sum(!good)
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

 nowish <- strsplit(as.character(tstop), ' ')[[1]][1]
asq <- aso <- rep("a",length(tat))
for (t in 1:length(tat)) {
 asq[t] <- strsplit(as.character(as[t]),':')[[1]][1]
 aso[t] <- strsplit(as.character(as[t]),':')[[1]][2]
}
asqt <- as.double(asq); asot <- as.double(aso)
asqt[is.na(asqt)==T] <- -1; asot[is.na(asot)==T] <- -1

#
# For Steve Stratman, DON'T remove stuttered forecasts. (no "de-stuttering")

# Reordering to simplify other operations later.
ord <- order(qit,tat)
tat<-tat[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; as<-as[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]

#
# Market Accuracy
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".
# ONLY binary for Stratman.

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

# Creating Dummy variable to track binary questions.  Question 206 is misclassified, however.
clq <- qn$classification
cls <- rep(0,length(clq))
cls[clq=="binary"] <- 1

# Creating variables for the number of safe-mode forecasters and the time since the first safe-mode forecast on each question.
nsmfq <- nsmdq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
 nsmfq[q] <- length(unique(pit[qit==qiq[q]&mdt==1&asqt<0]))
 if (nsmfq[q]>0) {
  nsmdq[q] <- (tstop-tstart) - min(floor(tat[qit==qiq[q]&mdt==1&asqt<0]-tstart))
 }
}

# Restrict analysis to public questions and also binary questions with 10 safe-mode forecasters and at least 10 days since the first safe-mode forecast.
# Check that 232 and 648 are included!
rsq <- sort(c(levels(factor(qiq[saq<=tstop&caq>tstart&ctq!="Study 2.1"&ctq!="Study 2.1,Study 2.1"&"Public"%in%gq&vldq==1&cls==1&nsmfq>9&nsmdq>9])),"206"))
rsq[rsq=="5"] <- NA; rsq[rsq=="671"] <- NA;good <- complete.cases(rsq); rsq<-rsq[good]					# Steve doesn't include because of mixture resolution.

 frc <- numeric(); rqb <- length(rsq)
 for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]&pit%in%pip[igrp==0]])}					# Checking the total forecasts on each question included for analysis.
 rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 

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
acqu <- acun <- acop <- nfqu <- rep(2,length(rsq)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsq)); ra <- rep(tstart,length(rsq))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsq))
 base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
 ra[q] <- raq[qiq==rsq[q]]
# Uses as question start date the first day on which there  was a valid safe mode forecast placed!
 astart <- min(tat[qit==rsq[q]&mdt==1&asqt<0])
 w <- which(tat%in%tat[tat>=astart&qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat])
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

ra <- rep(tstart,length(rsq))
for (q in 1:length(rsq)) {
# ra[q] <- saq[qiq==rsq[q]]
 ra[q] <- raq[qiq==rsq[q]]
# Uses as question start date the first day on which there  was a valid safe mode forecast placed!
 astart <- min(tat[qit==rsq[q]&mdt==1&asqt<0])
 w <- which(tat%in%tat[tat>=astart&qit==rsq[q]&asqt<0&mdt==1])		# For Steve Stratman, do not include conditional forecasts.
# w <- which(tat%in%tat[qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat&mdt==1]) #&mdpt==1&fput>1	# Include safe-mode forecasts from users who prefer power mode? Who haven't made many forecasts?
 time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
 lt <- length(time); nfqu[q] <- lt-1; weight <- rep(1,lt-1)
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  stv <- acd <- actop <- rep(2,lt); pocotop <- hittop <- rep(0,lt)
# Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
# I doubt Stratman includes this one hour, but it makes little difference and helps with our algorithm.
   acd[1] <- time[1]-base -(time[1]-60*60-base)
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
      else {						 					# ULinOP based on safe mode
       w2 <- which(pit[w]==pit[w][or[t]]); lw2 <- length(w2); if (lw2>1) {weight[w2] <- 0; weight[or[t]] <- 1}		# Removing older forecasts from the same user
       tmp3 <- colSums(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]])/sum(weight[or[1:t]]) # colMeans(tmp0[or[1:t],1:length(tmp1)])
# Here is where there was a mistake earlier.  Ken was using column means, which meant that forecasts without weight (old forecasts) were still contributing to the denominator.  Sorry.
      }
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
#   for (t in 1:5) {
     tmp0 <- matrix(svt[w,],c(length(w),40))
      if (t<2) {
       tmp3 <- tmp0[or[t],1:length(tmp1)]
      }
      else {						 					# ULinOP based on safe mode
       w2 <- which(pit[w]==pit[w][or[t]]); lw2 <- length(w2); if (lw2>1) {weight[w2] <- 0; weight[or[t]] <- 1}		# Removing older forecasts from the same user
       tmp3 <- colSums(tmp0[or[1:t],1:length(tmp1)]*weight[or[1:t]])/sum(weight[or[1:t]]) # colMeans(tmp0[or[1:t],1:length(tmp1)])
# Here is where there was a mistake earlier.  Ken was using column means, which meant that forecasts without weight (old forecasts) were still contributing to the denominator.  Sorry.
      }
     stv[t+1] <- tmp3[2]
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

br <- seq(0,2,0.1)
png("AcpQaO.png", width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(acqu,breaks=br)
two <- hist(acop,breaks=br)
plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
plot(one,col=rgb(1,0,0,0.5),add=T)
 text(0.8,14,pos=4,paste("Safe-Mode Forecasts, mean = ",round(acopm,2),sep=""),col=rgb(0,0,1,0.6))
 text(0.8,12,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
 mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

# Outputs table for easier comparison with Stratman's results.
ru <- rf <- rc <- rep(0,length(rsq))
for (q in 1:length(rsq)) {
 ru[q] <- length(unique(pit[qit==rsq[q]]))
 rf[q] <- length(tat[qit==rsq[q]])
 rc[q] <- length(cac[qic==rsq[q]])
}

write.table(data.frame(rsq,ra,acqu,acop,ru,rf,rc),file="Ken's Brier Scores.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)