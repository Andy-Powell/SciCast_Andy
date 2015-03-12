## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.

# setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#
# First run Get_Data.R.

tstart <- as.POSIXct("2013-11-25 00:00:00 EST")
base <- tstart-28*24*60*60
# For Steve Stratman
#tstop <- as.POSIXct("2014-11-30 00:00:00 EST")
tstop <- Sys.time()
days <- seq(1,ceiling(as.double(tstop - tstart)),1)

source("Incentive Selection.R")
#
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
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; ast<-ast[good]; apot<-apot[good]
tit<-tit[good]; cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

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


#qn$provisional_settled_at is start of comment period and qn$pending_until will be reused for event resolution (not question resolution/settlement)(Analysis_Setup)

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
 tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]])        # as vector groups associated wiht each question 
 lv <- length(tmp)  
 if (lv>0) {  gpq[q,1:lv] <- tmp }                          # If there is a group (and there always is), vector of groups put into gpq
 if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }           # If "Invlid Questions" in list of groups, valid questions = -1
}
# How many are invalid?
length(vldq[vldq==0])

svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40))
roqt <- roqat <-rep(-1,length(tat))



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