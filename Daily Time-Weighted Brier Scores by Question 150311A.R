###################################################
#
#  Generating time-weighted Brier score by month
#
#
###################################################
#source("Daily Time-Weighted Brier Scores by Question 150311A.R")
#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")

startData <- Sys.time() 
print("Data Generation started")

source("Get_Data_150203lb.R")

day <- as.POSIXct(rep("2013-11-25",length(days)))
day[1] <- trunc(as.POSIXct("2013-11-25"), units="days")
for (d in 2:length(days)){
  day[d] <- trunc(as.POSIXct(day[d-1]+24*60*70), units="days")
}

month <- day

source("General Data Prep 150304B.R")

rsqNorm <- sort(qiq[saq< as.POSIXct(Sys.time())])

for (t in 1:length(tat)) {
  temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  if (is.na(temp1[1])==F) {
    rvqt[t,1:length(temp1)] <- temp1
    if (mdt[t]>0) {                                           # if safe-mode forecast
      dflt <- (1-rst[t])/(length(temp1)-1)
      svt[t,1:length(temp1)] <- rep(dflt,length(temp1))  			# Assume non-attended options have uniform distribution.
      svt[t,(cit[t]+1)] <- rst[t]
      #print(svt[t,cit[t]+1])
    }
    if (sum(temp1%%1)==0) {													# Not mixture resolutions
      roqt[t] <- which(rvqt[t,]==1)-1
    }
  }
  if (asqt[t]%in%rsqNorm) {
    temp2 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==asqt[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
    if (is.na(temp2[1])==F) {
      rvqat[t,1:length(temp2)] <- temp2
      if (sum(temp2%%1)==0) {
        roqat[t] <- which(rvqat[t,]==1)-1
      }
    }
  }
}

###  *Save varibles used to reset varibales to original state
tatSave<-tat; titSave<-tit; pitSave<-pit; qitSave<-qit; nvtSave<-nvt; ovtSave<-ovt; astSave<-ast; apotSave<-apot
citSave<-cit; rstSave<-rst; mdtSave<-mdt; asqtSave<-asqt; asotSave<-asot; roqatSave<-roqat


monthlyBrier <- matrix(rep(2,length(rsqNorm)*(length(month)+1)), nrow=length(rsqNorm),ncol=length(month)+1)
lastNvt <- matrix(rep(2,length(rsqNorm)*31),nrow=length(rsqNorm),ncol=31)

for (m in 2:length(month)) {
#for (m in 2:8) {
  ## reseting variables
  tatMonth<-tatSave; titMonth<-titSave; pitMonth<-pitSave; qitMonth<-qitSave; nvtMonth<-nvtSave; ovtMonth<-ovtSave; astMonth<-astSave; apotMonth<-apotSave;
  citMonth<-citSave; rstMonth<-rstSave; mdtMonth<-mdtSave; asqtMonth<-asqtSave; asotMonth<-asotSave; roqatMonth<-roqatSave
  
 
  
  # Removing trades for other months
  tatMonth[tatMonth<month[m-1]] <- NA
  tatMonth[tatMonth>=month[m]] <- NA
  goodTat <- complete.cases(tatMonth)
  tatMonth<-tatMonth[goodTat]; titMonth<-titMonth[goodTat]; pitMonth<-pitMonth[goodTat]; qitMonth<-qitMonth[goodTat]; nvtMonth<-nvtMonth[goodTat]; ovtMonth<-ovtMonth[goodTat]; astMonth<-astMonth[goodTat]; apotMonth<-apotMonth[goodTat];
  citMonth<-citMonth[goodTat]; rstMonth<-rstMonth[goodTat]; mdtMonth<-mdtMonth[goodTat]; asqtMonth<-asqtMonth[goodTat]; asotMonth<-asotMonth[goodTat]; roqatMonth<-roqatMonth[goodTat]
  
  #tat<-tatMonth; 
  tit<-titMonth; pit<-pitMonth; qit<-qitMonth; nvt<-nvtMonth; ovt<-ovtMonth; ast<-astMonth; apot<-apotMonth;
  cit<-citMonth; rst<-rstMonth; mdt<-mdtMonth; asqt<-asqtMonth; asot<-asotMonth; roqat<-roqatMonth
  
    start <- Sys.time() 
    print("Active Accuracy calculations started")
    
    # Market Accuracy
    # Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
    acquAct <- acun <- acop <- nfqu <- acqu <- rep(2,length(rsqNorm)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsqNorm))
    ra <- rep(tstart,length(rsqNorm))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsqNorm))
    print("1")
    base <- tstart-28*24*60*60
    print("2")
    for (q in 1:length(rsqNorm)) {
    #for (q in 1:3) {
      tmp2 <- NA
      #ra[q] <- raqNorm[qiq==rsqNorm[q]]
      #ra[q] <- min(raqNorm[qiq==rsqNorm[q]],month[m])
      ra[q] <- min(saq[qiq==rsqNorm[q]],month[m])     ## nor raq, saq is resoluton date
      #raqExp[q] <- raq[qiq==rsqNorm[q]]
      # Uses as question start date the first day on which there  was a valid safe mode forecast placed!
      astart <- min(tatMonth[qitMonth==rsqNorm[q] &asqtMonth<0])
      print("3")
      #w <- which(tatMonth%in%tatMonth[tatMonth>=expStart &tatMonth<expStop &qitMonth==rsqNorm[q] &asqtMonth%in%c(-1,rsqNorm) &asotMonth==roqat])
      w <- which(tatMonth%in%tatMonth[tatMonth>=expStart &tatMonth<expStop &qitMonth==rsqNorm[q] &asqtMonth%in%c(-1,rsqNorm) &asotMonth==roqatMonth])
      print("4")
      time <- c(tatMonth[w],ra[q]); or <- order(time); time <- time[or]
      print("5")
      lt <- length(time); nfqu[q] <- lt-1
      tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsqNorm[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
#      lastNvt[q] <- rep(2,length(tmp1))
      ac <- acd <- act <- rep(2,lt); pocot <- hitt <- rep(0,lt)
      # Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
      #acd[1] <- time[1]-base -(month[m]-base)
      print("6")
      acd[1] <- difftime(time[1], month[m-1],units="days")    ##### changed
      print("7")
      pocot[1] <- pocou[q] <- 1/length(tmp1)
      
      print(c(m,q,rsqNorm[q],clq[qiq==rsqNorm[q]],lt))
      #print(c("diff-",difftime(month[m],raq[qiq==rsqNorm[q]])))
      #print(c("lastNvt1[q]-",lastNvt[q,1:length(tmp1)]))
      #print(is.na(lastNvt[q,][1]))
      if (lt<2) {
          if (difftime(month[m],raq[qiq==rsqNorm[q]])>0) {            # if pending_until is later than end of month....
            #print("1")
            lastNvt[q,1] <- NA
            monthlyBrier[q,m] <- NA                                                 # act <- NA  => acqu <- NA
            } else if (lastNvt[q,][1]==2) {
                  #print("3")
                  lastNvt[q,1] <- 2
                  monthlyBrier[q,m] <- NA
                } else {
                  monthlyBrier[q,m] <- acqu[q] <- monthlyBrier[q,m-1]
#                  act <- lastNvt[q,1:(length(tmp1))]                                      # and no data for current month => forecast = old forecast
#                  acd <- length(tmp1)
                  #print(c("2-",lastNvt[q,1:(length(tmp))]))           
                }
        }

      print(c("lastNvt2[q]-",lastNvt[q,1:length(tmp1)]))

      if (lt>1) {
       source("Incentive Accuracy Mechanics Normal 150221.R")
       for (l in 1:length(tmp2)){
         lastNvt[q,l] <- tmp2[l]
         monthlyBrier[q,m] <- acqu[q] <- sum(act*acd)/sum(acd)
       }
      }
      
      monthlyBrier[q,1] <- rsqNorm[q]
#      monthlyBrier[q,m] <- acqu[q] <- sum(act*acd)/sum(acd)


      #pocos[q] <-sum(pocot*acd)/sum(acd)
      #hit[q] <- sum(hitt*acd)/sum(acd)

      #print(c("lastNvt3[q]-",lastNvt[q,1:length(tmp1)]))
      #print(c("act- ",act))
      #print(c("acqu[q]- ",acqu[q]))
    
    }
}



  write.table(monthlyBrier,file="Daily Brier Data.csv",sep=",",append=F,col.names=c("questionId",c(as.character(day))),row.names=F)

  #write.table(lastNvt,file="Daily lastNvt.csv",sep=",",append=F,col.names=c("1","2","3","4","5","6","7","8","9",10,"11","12","13","14","15","16","17","18","19",20,"21","22","23","24","25","26","27","28","29",30,"31"),row.names=F)

duration <- as.double(difftime(Sys.time(),startData,units="sec"))   #reports time to retrieve files
print ("Data Generation Complete")
print(duration)


