######################################################
#
# Genrates a table with 
# Based on "Incentive Experiment Brier Data 150225.R"
#
#
#####################################################

# setwd("C:/Users/Walter/Documents/GMU/SciCast/Analysis")
# First run Get_Data.R.
source("Get_Data_150203lb.R")
source("Incentive Selection 150312.R")

source("General Data Prep 150304B.R")

#source("Incentive Overall Trade Selection 150212.R")

#rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expFirst &"Public"%in%gq &vldq==1])))
rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &raq>=expFirst &"Public"%in%gq &vldq==1])))

#### Checking the total forecasts on each question included for analysis.
frc <- numeric()
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]&pit%in%pip[igrp==0]])}
rsq <- rsq[frc>2]

tat2<-tat; tit2<-tit; pit2<-pit; qit2<-qit; nvt2<-nvt; ovt2<-ovt; ast2<-ast; apot2<-apot
cit2<-cit; rst2<-rst; mdt2<-mdt; #asqt2<-asqt; asot2<-asot


#### removing trades not in rsq and not in experimetnal sets.
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



write.table(tradeData,file=" Incentive Experiment tradeData.csv",sep=",",append=F,col.names=c("questionId","tradeId","userId","tradedAt","newValue","resolvedAt","resolveValue","Brier","set","active", "timeToRes","timeSinceActive"),row.names=F)