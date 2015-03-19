
###########################################################
## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.
##############################################################


# setwd("C:/Users/Walter/Documents/GMU/SciCast/Analysis")
# First run Get_Data.R.
source("Get_Data_150203lb.R")
source("Incentive Selection 150312.R")

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

        if (grepl("Lower", rust[t])) { rst[t] <- (tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][2],'[',fixed=T)[[1]][2])))/2}
        if (grepl("Higher", rust[t])) { rst[t] <- (tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)[[1]][1])))/2 }
        if (grepl("What they are now",rust[t])) { rst[t] <- tmp1[cit[t]+1]}
        if (grepl("null",rust[t])) { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
    }
 }


for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA
}
good <- complete.cases(tat)
sum(!good)     												# How many are not good?
tat<-tat[good]; tit<-tit[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; ast<-ast[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]; #asqt<-asqt[good]; asot<-asot[good]

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

#saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)
#raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)
qps <- qn$provisional_settled_at
pq <- qn$type
tpq <- rep(0,length(qiq))
tpq[pq=="binary"] <- 2
tpq[pq=="multi"] <- 3
ctq <- qn$categories
clq <- qn$classification

drq <- as.double(raq-caq)
hist(drq) 										# Range of questions' duration

########## Removing forecasts that occurred after resolution was known (Analysis_setup).
start1 <- Sys.time()
print("tat removal started")

for (t in 1:length(tat)) {           # for all tardes....
  if (tat[t]>raq[qiq==qit[t]]) {      # if traded_at > pending_until...   #####is pending_until correct vice resolved_at?
    tat[t] <- NA                       # NA subistuted for traded_at 
  } 
}


#duration <- as.double(difftime(Sys.time(),start1,units="sec"))   #reports time to retrieve files
#print(duration)

#tat[tat>=expStop] <- NA
#tat[tat<expStart] <- NA
tat[tat>tstop] <- NA  

duration <- as.double(difftime(Sys.time(),start1,units="sec"))   #reports time to retrieve files
print("tat removal complete")
print(duration)



# For all traded_at > than tstop (Sys.time()) substitute Na for traded_at
good <- complete.cases(tat)          # remove all trades with trades later than tstop or pending_until
sum(!good)
tat<-tat[good]; tit<-tit[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; ast<-ast[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]; #asqt<-asqt[good]; asot<-asot[good]



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


source("Incentive Overall Trade Selection 150212.R")

tat<-tatAAct; tit<-titAAct; pit<-pitAAct; qit<-qitAAct; nvt<-nvtAAct; ovt<-ovtAAct; ast<-astAAct
apot<-apotAAct; cit<-citAAct; rst<-rstAAct; mdt<-mdtAAct; asqt<-asqtAAct; asot<-asotAAct

source("De-Stuttering A.R")

tatAAct<-tat; titAAct<-tit; pitAAct<-pit; qitAAct<-qit; nvtAAct<-nvt; ovtAAct<-ovt; astAAct<-ast
apotAAct<-apot; citAAct<-cit; rstAAct<-rst; mdtAAct<-mdt; asqtAAct<-asqt; asotAAct<-asot

tat<-tatACon; tit<-titACon; pit<-pitACon; qit<-qitACon; nvt<-nvtACon; ovt<-ovtACon; ast<-astACon
apot<-apotACon; cit<-citACon; rst<-rstACon; mdt<-mdtACon; asqt<-asqtACon; asot<-asotACon

source("De-Stuttering A.R")

tatACon<-tat; titACon<-tit; pitACon<-pit; qitACon<-qit; nvtACon<-nvt; ovtACon<-ovt; astACon<-ast
apotACon<-apot; citACon<-cit; rstACon<-rst; mdtACon<-mdt; asqtACon<-asqt; asotACon<-asot

# Reordering to simplify other operations later (Analysis_Setup).
ord <- order(qit,tat)
tat<-tat[ord]; tit<-tit[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; ast<-ast[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]

#
# Market Accuracy
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".

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

svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40))
roqt <- roqat <-rep(-1,length(tat))
rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expStart &"Public"%in%gq &vldq==1])))

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

#### eliminating questions with less than 3 forecasts (WEighted_Forecasts)####
frc <- numeric()
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q] &pit%in%pip[igrp==0]])}  				# Checking the total forecasts on each question included for analysis.
# th_traded_at() for th_user_id that are in the list of "appropriate" questions
# pit%in%pip[igrp==0] -> th_user_id is in list of pr_user_ids that are not internal users 
##### what happened to no admin or specifically exlcuded users? #######
rsq <- rsq[frc>2] 

tradeDate <- rep(tstart, length(days))
#tradeDate[0] <- as.POSIXct(tstart)$date
tradeDate[0] <- trunc(tstart, "days")
for (date in 1:length(days)) {
  tradeDate[date] <- trunc(tradeDate[date-1]+60*60*25, "days")
}

###### Accruacy of Set A trades #####
## Accuracy of Set A active trades
#Act trades are Nov,Jan trades
tat<-tatAAct; tit<-titAAct; pit<-pitAAct; qit<-qitAAct; nvt<-nvtAAct; ovt<-ovtAAct; ast<-astAAct
apot<-apotAAct; cit<-citAAct; rst<-rstAAct; mdt<-mdtAAct; asqt<-asqtAAct; asot<-asotAAct

source("Incentive Accuracy Active Overall 141209.R")

rsqAct <- rsqExpA

tatActA <- tatAAct[qit%in%rsqExpA]
titActA <- titAAct[qit%in%rsqExpA]
pitActA <- pitAAct[qit%in%rsqExpA]
qitActA <- qitAAct[qit%in%rsqExpA]
nvtActA <- nvtAAct[qit%in%rsqExpA]
ovtActA <- ovtAAct[qit%in%rsqExpA]
astActA <- astAAct[qit%in%rsqExpA]
apotActA <- apotAAct[qit%in%rsqExpA]
citActA <- citAAct[qit%in%rsqExpA]
rstActA <- rstAAct[qit%in%rsqExpA]
mdtActA <- mdtAAct[qit%in%rsqExpA]
asqtActA <- asqtAAct[qit%in%rsqExpA]
asotActA <- asotAAct[qit%in%rsqExpA]

statusActA <- rep("Act",length(tatActA))
setActA <- rep("A", length(tatActA))

qActA <- rep(1,length(tatActA))      # default is first quarter
qActA[tatActA>= expChange2 ] <- 3    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qActA[tatActA>= expChange1 & tatActA<expChange2] <- 2   
qActA[tatActA>= expChange3] <- 4

accSetAAct <- acquAct
perSetAAct <- pocosAct
hitSetAAct <- hitAct

########################## Needs to be another instance of "Incentive Accuracy Active Overall 141209.R"  ## &qit%in%incentiveSet => in set A
## Accuracy of Set A non-active trades
#Con trades are Dec,Feb trades
tat<-tatACon; tit<-titACon; pit<-pitACon; qit<-qitACon; nvt<-nvtACon; ovt<-ovtACon; ast<-astACon
apot<-apotACon; cit<-citACon; rst<-rstACon; mdt<-mdtACon; asqt<-asqtACon; asot<-asotACon

source("Incentive Accuracy Active Overall 141209.R")

tatConA <- tatACon[qit%in%rsqExpA]
titConA <- titACon[qit%in%rsqExpA]
pitConA <- pitACon[qit%in%rsqExpA]
qitConA <- qitACon[qit%in%rsqExpA]
nvtConA <- nvtACon[qit%in%rsqExpA]
ovtConA <- ovtACon[qit%in%rsqExpA]
astConA <- astACon[qit%in%rsqExpA]
apotConA <- apotACon[qit%in%rsqExpA]
citConA <- citACon[qit%in%rsqExpA]
rstConA <- rstACon[qit%in%rsqExpA]
mdtConA <- mdtACon[qit%in%rsqExpA]
asqtConA <- asqtACon[qit%in%rsqExpA]
asotConA <- asotACon[qit%in%rsqExpA]

statusConA <- rep("Con",length(tatConA))
setConA <- rep("A", length(tatConA))

qConA <- rep(2,length(tatConA))      # default is first quarter
qConA[tatConA>=expChange3 ] <- 4    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qConA[tatConA>=expChange2 & tatConA<expChange3] <- 3   
qConA[tatConA<expChange1] <- 1

accSetACon <- acquAct
perSetACon <- pocosAct
hitSetACon <- hitAct



##### Print Routine  ############
acqum <- mean(accSetAAct)  #Active group mean
acopm <- mean(accSetACon)   #Control group mean	

pocosm<-mean(perSetAAct,na.rm=T)    # Percentage on correct option
pocoopm<-mean(perSetACon,na.rm=T)   # Percentage on correct option

hitm <- mean(hitSetAAct,na.rm=T)
hitopm <- mean(hitSetACon,na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely


winaO <- round(100*(length(accSetAAct[accSetAAct<accSetACon])/length(accSetAAct)))		# Percentage of time active betterh than non-active
impoO <- round(100*(acopm-acqum)/acopm)                       # Percentage improvement over non-active

br <- seq(0,2,0.1)
title <- paste("Set A Accruacy  ", expStart,"--",expStop,sep="",".png")
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(accSetAAct,breaks=br)
two <- hist(accSetACon,breaks=br)
count <- matrix(c(one$counts,two$counts), nrow=2,byrow=T)
colnames(count) <- one$mids
rownames(count) <- c("Active", "Non-Active")
count <- as.table(count)
#count <- table(one$counts,two$counts)
barplot(count,beside=T,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),xlab="Brier Score",
        ylab="Number of Questions",cex.main=1,main=paste("Accuracy ",expStart," to ",expStop,sep=""))
text(10,12,pos=4,paste("Set A Brier Scores (Non-Active) mean = ",round(acopm,3),sep=""),col=rgb(0,0,1,0.6))
text(10,15,pos=4,paste("Set A Brier Scores (Active) mean = ",round(acqum,3),sep=""),col=rgb(1,0,0,0.6))
#mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-6.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Set A questions:  ',length(rsqAct),sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

################################

###### Accruacy of Set B trades #####
## Accuracy of Set B active trades
# ACon trades = BAct trades -- Dec,Feb trades
tat<-tatACon; tit<-titACon; pit<-pitACon; qit<-qitACon; nvt<-nvtACon; ovt<-ovtACon; ast<-astACon
apot<-apotACon; cit<-citACon; rst<-rstACon; mdt<-mdtACon; asqt<-asqtACon; asot<-asotACon

source("Incentive Accuracy Control Overall 141209.R")

rsqCon <- rsqExpB

tatActB <- tatACon[qit%in%rsqExpB]
titActB <- titACon[qit%in%rsqExpB]
pitActB <- pitACon[qit%in%rsqExpB]
qitActB <- qitACon[qit%in%rsqExpB]
nvtActB <- nvtACon[qit%in%rsqExpB]
ovtActB <- ovtACon[qit%in%rsqExpB]
astActB <- astACon[qit%in%rsqExpB]
apotActB <- apotACon[qit%in%rsqExpB]
citActB <- citACon[qit%in%rsqExpB]
rstActB <- rstACon[qit%in%rsqExpB]
mdtActB <- mdtACon[qit%in%rsqExpB]
asqtActB <- asqt[qit%in%rsqExpB]
asotActB <- asot[qit%in%rsqExpB]

statusActB <- rep("Act",length(tatActB))
setActB <- rep("B", length(tatActB))

qActB <- rep(2,length(tatActB))      # default is first quarter
qActB[tatActB>=expChange3 ] <- 4    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qActB[tatActB>=expChange2 & tatActB<expChange3] <- 3   
qActB[tatActB<expChange1] <- 1

accSetBAct <- acquCon
perSetBAct <- pocosCon
hitSetBAct <- hitCon

## Accuracy of Set B non-active trades
#AAct trades = BCon trades -- Nov,Jan trades
tat<-tatAAct; tit<-titAAct; pit<-pitAAct; qit<-qitAAct; nvt<-nvtAAct; ovt<-ovtAAct; ast<-astAAct
apot<-apotAAct; cit<-citAAct; rst<-rstAAct; mdt<-mdtAAct; asqt<-asqtAAct; asot<-asotAAct

source("Incentive Accuracy Control Overall 141209.R")

tatConB <- tatAAct[qit%in%rsqExpB]
titConB <- titAAct[qit%in%rsqExpB]
pitConB <- pitAAct[qit%in%rsqExpB]
qitConB <- qitAAct[qit%in%rsqExpB]
nvtConB <- nvtAAct[qit%in%rsqExpB]
ovtConB <- ovtAAct[qit%in%rsqExpB]
astConB <- astAAct[qit%in%rsqExpB]
apotConB <- apotAAct[qit%in%rsqExpB]
citConB <- citAAct[qit%in%rsqExpB]
rstConB <- rstAAct[qit%in%rsqExpB]
mdtConB <- mdtAAct[qit%in%rsqExpB]
asqtConB <- asqtAAct[qit%in%rsqExpB]
asotConB <- asotAAct[qit%in%rsqExpB]

statusConB <- rep("Con",length(tatConB))
setConB <- rep("B", length(tatConB))

qConB <- rep(1,length(tatConB))      # default is first quarter
qConB[tatConB>= expChange2 ] <- 3    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qConB[tatConB>= expChange1 & tatConB<expChange2] <- 2   
qConB[tatConB>= expChange3] <- 4

accSetBCon <- acquCon
perSetBCon <- pocosCon
hitSetBCon <- hitCon

acqum <- mean(accSetBCon)  #Active group mean
acopm <- mean(accSetBAct)   #Control group mean  

pocosm<-mean(perSetBCon,na.rm=T)    # Percentage on correct option
pocoopm<-mean(perSetBAct,na.rm=T)   # Percentage on correct option

hitm <- mean(hitSetBAct,na.rm=T)
hitopm <- mean(hitSetBCon,na.rm=T) # Also compare to pocoum.  # Percentage of time correct option is forecast as most likely

winaO <- round(100*(length(accSetBAct[accSetBAct<accSetBCon])/length(accSetBAct)))  	# Percentage of time active betterh than non-active
impoO <- round(100*(acqum-acopm)/acqum)                       # Percentage improvement over non-active

br <- seq(0,2,0.1)
title <- paste("Set B Accruacy  ", expStart,"--",expStop,sep="",".png")
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(accSetBAct,breaks=br)
two <- hist(accSetBCon,breaks=br)
count <- matrix(c(one$counts,two$counts), nrow=2,byrow=T)
colnames(count) <- one$mids
rownames(count) <- c("Active", "Non-Active")
count <- as.table(count)
barplot(count,beside=T,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),xlab="Brier Score",
        ylab="Number of Questions",cex.main=1,main=paste("Set B Accuracy ",expStart," to ",expStop,sep=""))
text(10,12,pos=4,paste("Set B Brier score (Non-Active) mean = ",round(acqum,3),sep=""),col=rgb(0,0,1,0.6))
text(10,15,pos=4,paste("Set B Brier score (Active) mean = ",round(acopm,3),sep=""),col=rgb(1,0,0,0.6))
#mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-6.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Set B questions:  ',length(rsqCon),sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()



#########  special run for B Non-Active without first non-active set in order to compare
#          with Set A whose 1st non-active quarter was after an active quarter
#########
tat<-tatAAct; tit<-titAAct; pit<-pitAAct; qit<-qitAAct; nvt<-nvtAAct; ovt<-ovtAAct; ast<-astAAct
apot<-apotAAct; cit<-citAAct; rst<-rstAAct; mdt<-mdtAAct; asqt<-asqtAAct; asot<-asotAAct

source("Incentive Accuracy Control Overall  wo 1st Q B 141209.R")

accSetBCon3q <- acquCon
perSetBCon3q <- pocosCon
hitSetBCon3q <- hitCon

##### Print Routine  ############
acqum <- mean(accSetBCon3q)  #Active group mean
acopm <- mean(accSetBAct)   #Control group mean  

pocosm<-mean(perSetBCon3q,na.rm=T)    # Percentage on correct option
pocoopm<-mean(perSetBAct,na.rm=T)   # Percentage on correct option

hitm <- mean(hitSetBCon3q,na.rm=T)
hitopm <- mean(hitSetBAct,na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely

winaO <- round(100*(length(accSetBAct[accSetBAct<accSetBCon])/length(accSetBAct)))  	# Percentage of time active betterh than non-active
impoO <- round(100*(acqum-acopm)/acqum)                       # Percentage improvement over non-active

br <- seq(0,2,0.1)
title <- paste("Set B Accruacy wo 1st non-active qtr  ", expStart,"--",expStop,sep="",".png")
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(accSetBAct,breaks=br)
two <- hist(accSetBCon3q,breaks=br)
count <- matrix(c(one$counts,two$counts), nrow=2,byrow=T)
colnames(count) <- one$mids
rownames(count) <- c("Active", "Non-Active")
count <- as.table(count)
barplot(count,beside=T,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),xlab="Brier Score",
        ylab="Number of Questions",cex.main=1,main=paste("Set B Accuracy wo 1st Non-Active Qtr ",expChange1," to ",expStop,sep=""))
text(10,12,pos=4,paste("Set B Brier score (Non-Active) mean = ",round(acqum,3),sep=""),col=rgb(0,0,1,0.6))
text(10,15,pos=4,paste("Set B Brier score (Active) mean = ",round(acopm,3),sep=""),col=rgb(1,0,0,0.6))
#mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-6.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Set B questions:  ',length(rsqCon),sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()
################################

print(rsqAct)
print(rsqCon)

#################################
## Overall active & non-active means

accActAll <- c(accSetAAct,accSetBAct)
accConAll <- c(accSetACon,accSetBCon)

acqum <- mean(accActAll)  #Active group mean
acopm <- mean(accConAll)	 #Control group mean	

pocosm<-mean(c(perSetAAct,perSetBAct),na.rm=T)    # Percentage on correct option
pocoopm<-mean(c(perSetACon,perSetBCon),na.rm=T)      # Percentage on correct option

hitm <- mean(c(hitSetAAct,hitSetBAct),na.rm=T)
hitopm <- mean(c(hitSetACon,hitSetBCon),na.rm=T) # Also compare to pocoum.	# Percentage of time correct option is forecast as most likely

impoO <- round(100*(acopm-acqum)/acopm)                       # Percentage improvement over UNinOP

br <- seq(0,2,0.1)
title <- paste("Overall Experiment Accruacy  ", expStart,"--",expStop,sep="",".png")
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
one <- hist(accActAll,breaks=br)
two <- hist(accConAll,breaks=br)
count <- matrix(c(one$counts,two$counts), nrow=2,byrow=T)
colnames(count) <- one$mids
rownames(count) <- c("Active", "Non-Active")
count <- as.table(count)
barplot(count,beside=T,col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),xlab="Brier Score",
        ylab="Number of Questions",cex.main=1,main=paste("Overall Accuracy ",expStart," to ",expStop,sep=""))
text(10,12,pos=4,paste("Brier score (Non-Active) mean = ",round(acopm,3),sep=""),col=rgb(0,0,1,0.6))
text(10,15,pos=4,paste("Brier score (Active) mean = ",round(acqum,3),sep=""),col=rgb(1,0,0,0.6))
#mtext('        based on "de-stuttered" forecasts weighted by how long they last', outer=T,side=3,line=-3.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Better on ',winaO,'% of questions',sep=''), outer=T,side=3,line=-5.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Overall score improved ',impoO,'%',sep=''), outer=T,side=3,line=-6.5,cex=0.75,font=1,col=rgb(0,0,0,1))
mtext(paste('      Resolved Questions:  ',length(c(rsqAct,rsqCon)),sep=''), outer=T,side=3,line=-4.5,cex=0.75,font=1,col=rgb(0,0,0,1))
dev.off()

# Outputs table for easier comparison with Stratman's results.
#ru <- rf <- rc <- rep(0,length(rsq))
#for (q in 1:length(rsq)) {
# ru[q] <- length(unique(pit[qit==rsq[q]]))
# rf[q] <- length(tat[qit==rsq[q]])
# rc[q] <- length(cac[qic==rsq[q]])
#}


write.table (data.frame(setActA,statusActA,qActA,tatActA,titActA,pitActA,qitActA,nvtActA,ovtActA,astActA,apotActA,citActA,rstActA,mdtActA,asqtActA,asotActA),file="Incentive Exp Set A Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setConA,statusConA,qConA,tatConA,titConA,pitConA,qitConA,nvtConA,ovtConA,astConA,apotConA,citConA,rstConA,mdtConA,asqtConA,asotConA),file="Incentive Exp Set A Non-Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setActB,statusActB,qActB,tatActB,titActB,pitActB,qitActB,nvtActB,ovtActB,astActB,apotActB,citActB,rstActB,mdtActB,asqtActB,asotActB),file="Incentive Exp Set B Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setConB,statusConB,qConB,tatConB,titConB,pitConB,qitConB,nvtConB,ovtConB,astConB,apotConB,citConB,rstConB,mdtConB,asqtConB,asotConB),file="Incentive Exp Set B Non-Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)

tatAct <- c(tatActA,tatActB)
titAct <- c(titActA,titActB)
pitAct <- c(pitActA,pitActB)
qitAct <- c(qitActA,qitActB)
nvtAct <- c(nvtActA,nvtActB)
ovtAct <- c(ovtActA,ovtActB)
astAct <- c(astActA,astActB)
apotAct <- c(apotActA,apotActB)
citAct <- c(citActA,citActB)
rstAct <- c(rstActA,rstActB)
mdtAct <- c(mdtActA,mdtActB)
asqtAct <- c(asqtActA,asqtActB)
asotAct <- c(asotActA,asotActB)

setAct <- c(setActA,setActB)
statusAct <- c(statusActA,statusActB)
qAct <- c(qActA,qActB)

########
tatCon <- c(tatConA,tatConB)
titCon <- c(titConA,titConB)
pitCon <- c(pitConA,pitConB)
qitCon <- c(qitConA,qitConB)
nvtCon <- c(nvtConA,nvtConB)
ovtCon <- c(ovtConA,ovtConB)
astCon <- c(astConA,astConB)
apotCon <- c(apotConA,apotConB)
citCon <- c(citConA,citConB)
rstCon <- c(rstConA,rstConB)
mdtCon <- c(mdtConA,mdtConB)
asqtCon <- c(asqtConA,asqtConB)
asotCon <- c(asotConA,asotConB)

setCon <- c(setConA,setConB)
statusCon <- c(statusConA,statusConB)
qCon <- c(qConA,qConB)

write.table (data.frame(setAct,statusAct,qAct,tatAct,titAct,pitAct,qitAct,nvtAct,ovtAct,astAct,apotAct,citAct,rstAct,mdtAct,asqtAct,asotAct),file="Incentive Exp Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setCon,statusCon,qCon,tatCon,titCon,pitCon,qitCon,nvtCon,ovtCon,astCon,apotCon,citCon,rstCon,mdtCon,asqtCon,asotCon),file="Incentive Exp non_active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)

#write.table(data.frame(rsq,ra,acqu,acop,ru,rf,rc),file="Incentive Brier Scores.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)