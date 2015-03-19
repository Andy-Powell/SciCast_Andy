######################################################
#
# Genrates a table with 
## Based on "incentive Experiment Accuracy Overall"
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.

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


goodp <- complete.cases(cap)
sum(!goodp)     												# How many are not good?
cap<-cap[goodp]; pus<-pus[goodp]; pip<-pip[goodp]; grp<-grp[goodp,]; rip<-rip[goodp]

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
        #tmp2 -> mean(<num1. <num2>)
    }
 }


for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA
}
goodt1 <- complete.cases(tat)
sum(!goodt1)     												# How many are not good?
tat1<-tat[goodt1]; tit1<-tit[goodt1]; pit1<-pit[goodt1]; qit1<-qit[goodt1]; nvt1<-nvt[goodt1]; ovt1<-ovt[goodt1]; ast1<-ast[goodt1]; apot1<-apot[goodt1]
cit1<-cit[goodt1]; rst1<-rst[goodt1]; mdt1<-mdt[goodt1]; #asqt1<-asqt[goodt1]; asot1<-asot[goodt1]

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

grq <- as.character(qn$groups)
qps <- qn$provisional_settled_at
pq <- qn$type
#tpq <- rep(0,length(qiq))
#tpq[pq=="binary"] <- 2
#tpq[pq=="multi"] <- 3
ctq <- qn$categories
clq <- qn$classification
drq <- as.double(raq-caq)
hist(drq) 										# Range of questions' duration


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


########## Removing forecasts that occurred after resolution was known (Analysis_setup).
start1 <- Sys.time()
print("tat removal started")

for (t in 1:length(tat1)) {           # for all tardes....
  if (tat1[t]>raq[qiq==qit1[t]]) {      # if traded_at > pending_until...   #####is pending_until correct vice resolved_at?
    tat1[t] <- NA                       # NA subistuted for traded_at 
  } 
}

tat1[tat1>=expStop] <- NA
tat1[tat1<expStart] <- NA
tat1[tat1>tstop] <- NA  

# For all traded_at > than tstop (Sys.time()) substitute Na for traded_at
goodt2 <- complete.cases(tat1)          # remove all trades with trades later than tstop or pending_until
sum(!goodt2)
tat2<-tat1[goodt2]; tit2<-tit1[goodt2]; pit2<-pit1[goodt2]; qit2<-qit1[goodt2]; nvt2<-nvt1[goodt2]; ovt2<-ovt1[goodt2]; ast2<-ast1[goodt2]; apot2<-apot1[goodt2]
cit2<-cit1[goodt2]; rst2<-rst1[goodt2]; mdt2<-mdt1[goodt2]; #asqt2<-asqt1[goodt2]; asot2<-asot1[goodt2]

duration <- as.double(difftime(Sys.time(),start1,units="sec"))   #reports time to retrieve files
print("tat removal complete")
print(duration)

#source("Incentive Overall Trade Selection 150212.R")

#rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expFirst &"Public"%in%gq &vldq==1])))
rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expFirst &"Public"%in%gq &vldq==1])))

# Checking the total forecasts on each question included for analysis.
frc <- numeric()
for (q in 1:length(rsq)) {frc[q] <- length(tat2[qit2==rsq[q]&pit2%in%pip[igrp==0]])}
rsq <- rsq[frc>2]

tat2<-tat1; tit2<-tit1; pit2<-pit1; qit2<-qit1; nvt2<-nvt1; ovt2<-ovt1; ast2<-ast1; apot2<-apot1
cit2<-cit1; rst2<-rst1; mdt2<-mdt1; #asqt2<-asqt1; asot2<-asot1

tat2[qit2%ni%rsq] <- NA
tat2[qit2%ni%incentiveSet &qit2%ni%controlSet] <- NA
goodt3 <- complete.cases(tat2)
sum(!goodt3)
tat3<-tat2[goodt3]; tit3<-tit2[goodt3]; pit3<-pit2[goodt3]; qit3<-qit2[goodt3]; nvt3<-nvt2[goodt3]; ovt3<-ovt2[goodt3]; ast3<-ast2[goodt3]; apot3<-apot2[goodt3]
cit3<-cit2[goodt3]; rst3<-rst2[goodt3]; mdt3<-mdt2[goodt3]

########## generates list of condional assumption questions (asqt) and options (asot), -1 indicates in either indicades non-conditional trade (Analysis_setup)
nowish <- strsplit(as.character(tstop), ' ')[[1]][1]   # tstop date only
asq <- aso <- rep("a",length(tat3))
for (t in 1:length(tat3)) {                               # for all remaining forecasts...
  asq[t] <- strsplit(as.character(ast3[t]),':')[[1]][1]    # assumptin question as str
  aso[t] <- strsplit(as.character(ast3[t]),':')[[1]][2]    # assumtion option as str
}
asqt <- as.double(asq)                                   # assumptin question as double
asot <- as.double(aso)                                   # assumptin option as double
asqt[is.na(asqt)==T] <- -1                               # if no seirialized _assumtions question replace NAs wiht -1
asot[is.na(asot)==T] <- -1                               # if no seirialized _assumtions option replace NAs wiht -1     => for non-conditional trades  asqt & asot = -1

###########

tat<-tat3; tit<-tit3; pit<-pit3; qit<-qit3; nvt<-nvt3; ovt<-ovt3; ast<-ast3; apot<-apot3
cit<-cit3; rst<-rst3; mdt<-mdt3; #asqt<-asqt3; asot<-asot3

source("De-Stuttering A.R")

tat4<-tat; tit4<-tit; pit4<-pit; qit4<-qit; nvt4<-nvt; ovt4<-ovt; ast4<-ast; apot4<-apot
cit4<-cit; rst4<-rst; mdt4<-mdt; #asqt4<-asqt; asot4<-asot


source("Incentive Experiment Brier Data 150225.R")    # Generates data.frame, tradeData w/ Brier Scores for each trade

#source("Brier Score vs Choices graph 150304.R")

roqt <- roqat <-rep(-1,length(tat))



#write.table(data.frame(rsq,ra,acqu,acop,ru,rf,rc),file="Incentive Brier Scores.csv",sep=",",append=F,col.names=c("Question_Number","Resolution_Date","SciCast_Brier_Score","ULinop_Brier_Score","Number_of_Users","Number_of_Forecasts","Number_of_Comments"),row.names=F)