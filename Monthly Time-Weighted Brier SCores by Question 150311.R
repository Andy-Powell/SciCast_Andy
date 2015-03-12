###################################################
#
#  Generating time-weighted Brier score by month
#
#
###################################################

source("Get_Data_150203lb.R")

monthData <- read.csv("SciCast Months.csv")
month <- as.POSIXct(monthData$month)


source("General Data Prep 150304B.R")
monthlyBrier <- matrix(rep(2,length(qiq)*length(month)), nrow=length(qiq),ncol=length(month))

tatSave <- tat

### needs data prep - qiq
#for (m in 2:length(month)) {
for (m in 2:2) {
  tatMonth <- tatSave
  #print(c(m, month))
  tatMonth[tatMonth<month[m-1]] <- NA
  tatMonth[tatMonth>=month[m]] <- NA
  goodTat <- complete.cases(tatMonth)
  tatMonth <- tatMonth[goodTat]
  
    
    start <- Sys.time() 
    print("Active Accuracy calculations started")
    
    #lp <- length(pip)
    rsqNorm <- qiq
    raqNorm <- raq
    
    # Market Accuracy
    # Weight forecasts by how long they endure. Average over questions.  THIS IS NOT WHAT STEVE STRATMAN DOES, but it's close.
    acquAct <- acun <- acop <- nfqu <- acqu <- rep(2,length(rsqNorm)); pocos <- pocou <- pocoop <- hit <- hitop <- rep(0,length(rsqNorm))
    ra <- rep(tstart,length(rsqNorm))#; ra <- rep(as.POSIXct("2013-11-25 00:00:00 EST"),length(rsqNorm))
    base <- tstart-28*24*60*60
    for (q in 1:length(rsqNorm)) {
      #for (q in 36:36) {
      ra[q] <- raq[qiq==rsqNorm[q]]
      raqExp[q] <- raq[qiq==rsqNorm[q]]
      # Uses as question start date the first day on which there  was a valid safe mode forecast placed!
      astart <- min(tat[qit==rsqNorm[q] &asqt<0])
      w <- which(tat%in%tat[tat>=expStart &tat<expStop &qit==rsqNorm[q] &asqt%in%c(-1,rsqNorm) &asot==roqat])
      time <- c(tat[w],ra[q]); or <- order(time); time <- time[or]
      lt <- length(time); nfqu[q] <- lt-1
      tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsqNorm[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
      ac <- acd <- act <- rep(2,lt); pocot <- hitt <- rep(0,lt)
      # Pretend the first trade came after 1 hour because we don't have a record of how long the questions were paused after being published.
      acd[1] <- time[1]-base -(time[1]-60*60-base)
      pocot[1] <- pocou[q] <- 1/length(tmp1)
    
      source("Incentive Accuracy Mechanics Normal 150221.R")
      
      monthlyBrier[q,m] <- acqu[q] <- sum(act*acd)/sum(acd)
      #pocos[q] <-sum(pocot*acd)/sum(acd)
      #hit[q] <- sum(hitt*acd)/sum(acd)
    
  }
  
}



source("Accuracy Overall 141209")