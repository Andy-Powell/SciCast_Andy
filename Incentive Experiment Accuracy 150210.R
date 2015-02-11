## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.

#
# First run Get_Data.R.
source("Get_Data_150203lb.R")
source("Incentive Selection 150210.R")

# Removing admin accounts and activity (Data_cleaning)
# Match to Steve's list!

pip <- pr$user_id; pus <- as.character(pr$username); cap <- as.POSIXct(pr$created_at); grps <- pr$groups; rip <-pr$referral_id

adu <- c("amsiegel","BAE11","brnlsl","brobins","cedarskye","christinafreyman","ctwardy",
         "daggre_admin","dquere","gbs_tester","Inkling","jessiejury","jlu_bae","kennyth0",
         "klaskey","kmieke","manindune","Naveen Jay","pthomas524","Question_Admin",
         "Question Mark","randazzese","RobinHanson","saqibtq","scicast_admin","slin8",
         "ssmith","tlevitt","wsun")

ads <- integer()
ads <- c(15,16,70,82,135,249,855)    #specificly excluded user_ids from MITRE

########  All are coded as internal users, so why is this part needed?  #########

adi <- numeric()                #adi in a list of specifically excluded pr_user_names
for (i in 1:length(adu)) {
  adi[i] <- pip[pus==adu[i]]
  cap[pip==adi[i]] <- NA         #cap is pr_created_at with NAs for Pr_user_ids on Adi list
}

for (i in 1:length(ads)) {
  cap[pip==ads[i]] <- NA                                                                        #puts NA for each pr_created_at for each admin user
  adi <- c(adi,ads[i])   
}

grp <- array(rep("a",length(pip)*20),c(length(pip),20)); igrp <- rep(0,length(pip))    #grp is an array of all groups a user is part of
for (i in 1:length(pip)) {
  temp <- as.vector(strsplit(as.character(grps[i]),",")[[1]])                           #makes a vector of the list of groups
  grp[i,1:length(temp)] <- temp                                                         #puts the vector in the grp array
}

#adi <- numeric()
for (i in 1:length(pip)) {
  for (g in 1:20) {
    if (grp[i,g]=="Admin"|grp[i,g]=="SuperAdmin"|grp[i,g]=="UserAdmin"|grp[i,g]=="BadgesAdmin"|grp[i,g]=="RolesAdmin"|grp[i,g]=="QuestionAdmin") {
      cap[i] <- NA                                                                        #puts NA for each pr_created_at for each admin user
      adi <- c(adi,pip[i])                                                                #adds admion users to adi list
    }
    if (grp[i,g]=="Internal") {  									# igrp is a list (length(pip)) with internal users flagged with 1s 
      igrp[i] <- 1
      adi <- c(adi,pip[i])
    }
  }
}
adi <- unique(adi)


good <- complete.cases(cap)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grp<-grp[good,]; rip<-rip[good]

pit <- th$user_id; qit <- th$question_id
tat <- as.POSIXct(th$traded_at)
nvt <- th$new_value_list
ovt <- th$old_value_list
ast <- th$serialized_assumptions
apot <- th$assets_per_option
tit <- th$trade_id
cit <- th$choice_index
thInterface <- th$interface_type

rust <- as.character(th$raw_user_selection)
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\",[0.9545454545454546,1],null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\",[0.9545454545454546,1],null]"
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\", [0.9545454545454546, 1], null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\", [0.9545454545454546,1], null]" 
rust[rust=="[\"\\\"Less than 4.75%\\\"\",[-0.33333333333333326,-0.33333333333333326],null]"] <- "None"
rust[rust=="[\"\\\"Between 4.75% and 5%\\\"\",[-0.33333333333333326,0.33333333333333326],\"Lower\"]"] <- "None"
rust[rust=="[\"\\\"Less than 4.75%\\\"\", [-0.33333333333333326, -0.33333333333333326], null]"] <- "None"
rust[rust=="[\"Down ~12% or more\",[-0.21428571428571427,0.5],null]"] <- "None"
rust[rust=="[\"Up ~12% or more\",[0.842857142857143,1.2142857142857142],null]"] <- "None"

rst <- rep(0,length(rust))
mdt <- rep(1,length(rust))      ##orinially rst <- med <- rep(0,length(rust))
#for (t in 1:39) {

for (t in 1:length(rust)) {
 #if (grepl("None",rust[t])) {
  if (thInterface[t]==2)  {
  mdt[t] = 0												# 1 indicates safe-mode forecast.
  tmp1 <- as.double(strsplit(as.vector(nvt[t]),",")[[1]])
  rst[t] <- tmp1[cit[t]+1]
 } else {
 
  # A later selection on SciCast.org like "Higher" will assume the user wants the forecast halfway between the current market estimate and the top of the bin.
  tmp1 <- as.double(strsplit(as.vector(ovt[t]),",")[[1]])						

  
        #rust -> ["<str1>",[<num1>,<num2>],"<str2>"] ["<str1>",[<num1>,<num2>],null]
        if (grepl("Lower", rust[t])) { rst[t] <- (tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][2],'[',fixed=T)[[1]][2])))/2}
        ##strsplit(rust[t],',')[[1]][2] -> [<num1>
        ##strsplit([<num1>,']',fixed=T)[[1]][1]) -> <num1> (bin min)
        ##tmp2 -> x - (x- binmax)/2
        if (grepl("Higher", rust[t])) { rst[t] <- (tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)[[1]][1])))/2 }
        ##strsplit(rust[t],',')[[1]][3] -> <num2>]
        ##strsplit(<num2>],']',fixed=T)[[1]][1]) -> <num2> (bin max)
        ##tmp2 -> x + (binmax - x)/2
        #if (tmp2=="Higher") { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)[[1]][1]) -tmp1[cit[t]+1])/2 }
        ##strsplit(rust[t],',')[[1]][3] -> <num2>]
        ##strsplit(<num2>],']',fixed=T)[[1]][1]) -> <num2>
        ##tmp2 -> tmp1[cit[t]+1] + <num2> -tmp1[cit[t]+1])/2 -> <num2> + tmp1[cit[t]+1])/2
        if (grepl("What they are now",rust[t])) { rst[t] <- tmp1[cit[t]+1]}
        if (grepl("null",rust[t])) { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
        #tmp2 -> mean(<num1. <num2>)
    }
 }


for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA
}
good <- complete.cases(tat)
sum(!good)     												# How many are not good?
tat<-tat[good]
pit<-pit[good]
qit<-qit[good]
nvt<-nvt[good]
ovt<-ovt[good]
ast<-ast[good]
apot<-apot[good]
tit<-tit[good]
cit<-cit[good]
rst<-rst[good]
mdt<-mdt[good]

pic <- cm$user_id
cac <- as.POSIXct(cm$created_at)
qic <- cm$question_id
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
tstart <- as.POSIXct("2013-11-25 00:00:00 EST")
base <- tstart-28*24*60*60
tstop <- Sys.time()

days <- seq(1,ceiling(as.double(tstop - tstart)),1)

#qn$provisional_settled_at is start of comment period and qn$pending_until will be reused for event resolution (not question resolution/settlement)(Analysis_Setup)
qiq <- qn$question_id
caq <- as.POSIXct(qn$created_at)
grq <- as.character(qn$groups)
saq <- as.character(qn$resolution_at)
raq <- as.character(qn$pending_until)


saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)
raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)
ls <- qn$relationships_source_question_id
ld <- qn$relationships_destination_question_id
ql <- qn$is_locked; qv <- qn$is_visible
qps <- qn$provisional_settled_at
pq <- qn$type
tpq <- rep(0,length(qiq))
tpq[pq=="binary"] <- 2
tpq[pq=="multi"] <- 3
ctq <- qn$categories
orq <- qn$is_ordered
orq <- as.double(orq)
rvq <- qn$resolution_value_array
clq <- qn$classification

drq <- as.double(raq-caq)
hist(drq) 										# Range of questions' duration

########## Removing forecasts that occurred after resolution was known (Analysis_setup).
for (t in 1:length(tat)) {           # for all tardes....
 if (tat[t]>raq[qiq==qit[t]]) {      # if traded_at > pending_until...   #####is pending_until correct vice resolved_at?
  tat[t] <- NA                       # NA subistuted for traded_at 
 }
}
tat[tat>tstop] <- NA                 # For all traded_at > than tstop (Sys.time()) substitute Na for traded_at
good <- complete.cases(tat)          # remove all trades with trades later than tstop or pending_until
sum(!good)
tat<-tat[good]; tit<-tit[good];pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; ast<-ast[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]


########## generates list of condional assumption questions (asqt) and options (asot), -1 indicates in either indicades non-conditional trade (Analysis_setup)
 nowish <- strsplit(as.character(tstop), ' ')[[1]][1]   # tstop date only
asq <- aso <- rep("a",length(tat))
for (t in 1:length(tat)) {                               # for all remaining forecasts...
 asq[t] <- strsplit(as.character(ast[t]),':')[[1]][1]    # assumptin question as str
 aso[t] <- strsplit(as.character(ast[t]),':')[[1]][2]    # assumtion option as str
}
asqt <- as.double(asq)                                   # assumptin question as double
asot <- as.double(aso)                                   # assumptin option as double
asqt[is.na(asqt)==T] <- -1                               # if no seirialized _assumtions question replace NAs wiht -1
asot[is.na(asot)==T] <- -1                               # if no seirialized _assumtions option replace NAs wiht -1     => for non-conditional trades  asqt & asot = -1

###########


# For Steve Stratman, DON'T remove stuttered forecasts. (no "de-stuttering")

# Reordering to simplify other operations later (Analysis_Setup).
ord <- order(qit,tat)
tat<-tat[ord]; tit<-tit[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; ast<-ast[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]

#
# Market Accuracy
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".
# ONLY binary for Stratman.

# Find resolved questions (Anaysis_Setup)     ##### Not used for input to anything else? ####
'%ni%' <- Negate('%in%')
gpq <- matrix(rep("a",length(qiq)*200),c(length(qiq),200))
vldq <- rep(0,length(qiq))                                  # initialize 

for (q in 1:length(qiq)) {
 tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]])        # as vector groups associated with each question 
 lv <- length(tmp)  
 if (lv>0) {  gpq[q,1:lv] <- tmp }                          # If there is a group (and there always is), vector of groups put into gpq
 if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }           # If "Invalid Questions" in list of groups, valid questions = -1
}
# How many are invalid?
length(vldq[vldq==0])

svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40))
roqt <- roqat <-rep(-1,length(tat))
#rsq <- levels(factor(qiq[raq<=Sys.time()&caq>tstart]))
rsq <- levels(factor(qiq[raq<=tstop &caq>tstart]))  ### This uses pending_until (raq) vice Resolved_at

######## 
# Cleaning up question groups - creating vectors of groups for each user (Data_Cleaning)
wtg <- rep(0,length(qiq))
gq <- array(numeric(), c(length(qiq),200))                         #### WHy not a matrix  ####
for (j in 1:length(qiq)) {                                         # For all questions
 if(grq[j]!="") {                                                  # If there are any groups.....
  temp <- levels(factor(strsplit(as.character(grq[j]),",")[[1]]))  # generate a vector of groups for each user  #### Same as above ###
  wtg[j]<- 1/length(temp)                                          # weight is inversly proportinal to number fo groups
  gq[j,1:(length(temp))] <- temp                                   # putting the vector into array gq           #### WHy is gq different from gpq                   
 }
}

# Creating Dummy variable to track binary questions.  Question 206 is misclassified, however(Data_Selection inlcuding 206).
### OBE  by using classification (clq)
#cls <- rep(0,length(clq))
#cls[clq=="binary"] <- 1
#cls[qiq="206"] <- 1

# Creating variables for the number of safe-mode forecasters and the time since the first safe-mode forecast on each question (Data_Selection).
###  only for ULinOP ###
### move and use rsq? ###
#nsmfq <- nsmdq <- rep(0,length(qiq))
#for (q in 1:length(qiq)) {
# nsmfq[q] <- length(unique(pit[qit==qiq[q] &mdt==1 &asqt<0]))  # number of unique th_user_ids for each qr_question_id which are same (mdt==1) and non-conditional (asqt<0)
# if (nsmfq[q]>0) {
#  nsmdq[q] <- (tstop-tstart) - min(floor(tat[qit==qiq[q] &mdt==1 &asqt<0]-tstart))  # floor -> first trade for each question that is safe (mdt==1) and non-conditional (asqt<0) (min???) days?
# } ###### tstop or resolved_at (raq)  ########
#}

frc <- numeric(); rqb <- length(rsq)
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]&pit%in%pip[igrp==0]])}  				# Checking the total forecasts on each question included for analysis.
rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 

# for multi questions   # possibly normalize remaining probablity using previous forcasts on other options - how close are other edits (Ulinop)
for (t in 1:length(tat)) {
  temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  if (is.na(temp1[1])==F) {
    rvqt[t,1:length(temp1)] <- temp1
    if (mdt[t]>0) {                                           # if safe-mode forecast   ### get rid fof mdt use inerface_value ###
      dflt <- (1-rst[t])/(length(temp1)-1)
      svt[t,1:length(temp1)] <- rep(dflt,length(temp1))				# Assume non-attended options have uniform distribution.
      svt[t,(cit[t]+1)] <- rst[t]
      #print(svt[t,cit[t]+1])
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

# Restrict analysis to public questions.
# Check that 232 and 648 are included!qn$
#rsq <- sort(c(levels(factor(qiq[saq<=tstop &caq>tstart &ctq!="Study 2.1" &ctq!="Study 2.1,Study 2.1" &"Public"%in%gq &vldq==1 &cls==1 &nsmfq>9 &nsmdq>9])),"206")) 
#rsq <- sort(c(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1 &cls==1 &nsmfq>9 &nsmdq>9])),"196","206","232","648")) 
#rsq <- sort(c(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1 &cls==1 &nsmfq>9 &nsmdq>9])),"196","232","648"))
#rsq <- sort(c(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1 &cls==1 &nsmfq>9 &nsmdq>9])),"232","648"))
#rsq <- sort(c(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1] &(qit%in%setAQstns | qit%in%setBQstns))),"196","232","648")) 
rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expStart &"Public"%in%gq &vldq==1])))   #non-ULinOP
#rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1 &(qiq%in%incentiveSet || qiq%in%controlSet)])))
# selects and sorts qr_user_id for
# resolution before tstop & question created after tstart
# catagories not "Study 2.1" or &ctq!="Study 2.1,Study 2.1   ###note - study 2.1 only appears in group - cqt is qn$catagories -> not used
# public is in gq
# question not designated invalid questions (vldq==1)
# is bianary (cls==1)
# has 10+ users executing trades (nsmfq>9)
####### time since first safe-mode trade > 9 days ?????  This is not the same as 10+ day with trades. ########

#rsq[rsq=="5"] <- NA                    # Steve doesn't include because of mixture resolution.
#rsq[rsq=="671"] <- NA                  # Steve doesn't include because of mixture resolution.
#good <- complete.cases(rsq); rsq<-rsq[good]					

#### eliminating questions with less than 3 forecasts (WEighted_Forecasts)####
frc <- numeric(); rqb <- length(rsq)
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q] &pit%in%pip[igrp==0]])}  				# Checking the total forecasts on each question included for analysis.
# th_traded_at() for th_user_id that are in the list of "appropriate" questions
# pit%in%pip[igrp==0] -> th_user_id is in list of pr_user_ids that are not internal users 
##### what happened to no admin or specifically exlcuded users? #######
rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 



# Restrict analysis to public questions and also binary questions with 10 safe-mode forecasters and at least 10 days since the first safe-mode forecast.
# Check that 232 and 648 are included!




tradeDate <- rep(tstart, length(days))
#tradeDate[0] <- as.POSIXct(tstart)$date
tradeDate[0] <- trunc(tstart, "days")
for (date in 1:length(days)) {
  tradeDate[date] <- trunc(tradeDate[date-1]+60*60*25, "days")
}


source("Incentive Accuracy Active 141209.R")
source("Incentive Accuracy Control 141209.R")
#source("ULinOP_150122C.R")

# SciCast and Uniform distribution BS
acqum <- mean(acquAct)  #Active group mean
#acunm <- mean(acun)  #uniform
acopm <- mean(acquCon)	 #Control group mean	

pocosm<-mean(pocosAct,na.rm=T)
pocoopm<-mean(pocosCon,na.rm=T)
#pocoum<-mean(pocou,na.rm=T)	# Percentage on correct option
hitm <- mean(hitAct,na.rm=T)
hitopm <- mean(hitCon,na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely

#winaU <- round(100*(length(acquAct[acquAct<acun])/length(acquAct)))		# Percentage of time SciCast better than uniform
#winaO <- round(100*(length(acquAct[acquAct<acquCon])/length(acquAct)))		# Percentage of time SciCast better than ULinOP pool
#impoU <- round(100*(acunm-acqum)/acunm)                       # Percentage improvement over uniform
impoO <- round(100*(acopm-acqum)/acopm)                       # Percentage improvement over UNinOP

br <- seq(0,2,0.1)
title <- paste("Incentive Accruacy  ", expStart,"--",expStop,sep="",".png")
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(acquAct,breaks=br)
two <- hist(acquCon,breaks=br)
plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",expStop,sep=""))
plot(one,col=rgb(1,0,0,0.5),add=T)
 text(0.8,4,pos=4,paste("Non-Active Forecasts, mean = ",round(acopm,3),sep=""),col=rgb(0,0,1,0.6))
 text(0.8,6,pos=4,paste("Active Forecasts, mean = ",round(acqum,3),sep=""),col=rgb(1,0,0,0.6))
 mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 #mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
 mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      number of Act ',impoO,'%',sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

#br <- seq(0,2,0.1)
#png("ULinOP Question Comparison.png", width = 3600, height = 3600, pointsize = 18, res = 360)
#one <- hist(acqu,breaks=br)
#two <- hist(acop,breaks=br)
#plot(two,col=rgb(0,0,1,0.5),xlim=c(0,2),xlab="Brier Score",ylim=c(0,floor(max(one$counts)*1.1)),ylab="Number of Questions",cex.main=1,main=paste("Accuracy through ",nowish,sep=""))
#plot(one,col=rgb(1,0,0,0.5),add=T)
# text(0.8,14,pos=4,paste("ULinOP Forecasts, mean = ",round(acopm,2),sep=""),col=rgb(0,0,1,0.6))
# text(0.8,10,pos=4,paste("SciCast Forecasts, mean = ",round(acqum,2),sep=""),col=rgb(1,0,0,0.6))
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

write.table(data.frame(rsq,ra,acqu,acop,ru,rf,rc),file="Incentive Brier Scores.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)