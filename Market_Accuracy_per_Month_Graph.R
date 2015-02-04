open <- ceiling(as.double(as.POSIXlt("2014-09-30 00:00:00 EDT")-tstart)/30.5)
#weeks <- seq(1,ceiling(as.double(Sys.time()-60*60*24 - tstart)/7),1)
 mnths <- seq(1,open,1)
## Weight forecasts by how long they endure.
oq <- rep(0,length(mnths))
acqu <- array(rep(numeric(),length(mnths)*length(rsq)),c(length(mnths),length(rsq)))
 base <- tstart-28*24*60*60
for (w in 1:length(mnths)) {
 nfqu <- rep(2,length(rsq)); ra <- rep(tstart,length(rsq))
 base <- tstart-28*24*60*60
for (q in 1:length(rsq)) {
 ra[q] <- raq[qiq==rsq[q]]
 wh <- which(tat%in%tat[qit==rsq[q]&asqt%in%c(-1,rsq)&asot==roqat])
 time <- c(tat[wh],ra[q]); or <- order(time); time <- time[or]
 lt <- length(time); nfqu[q] <- lt-1
  tmp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[q]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  ac <- acd <- act <- rep(2,lt)
# Pretend the first trade lasted 24 hours because we don't have a record of how long the questions were paused after being published.
   acd[1] <- time[1]-base -(time[1]-24*60*60-base)
   if (lt>1) {
    for (t in 1:(lt-1)) {
     acd[t+1] <- time[t+1]-base -(time[t]-base)
     tmp2 <- as.double(strsplit(as.vector(nvt[wh])[or[t]],",")[[1]])
    }
   }

  if (orq[qiq==rsq[q]]==2) {
   actt <-rep(0,length(tmp1)-1)
   for (o in 1:(length(tmp1)-1)) {
    actt[o] <- 2*(o/length(tmp1)-sum(tmp1[1:o]))^2
   }
   act[1] <- sum(actt)/(length(tmp1)-1)
   if (lt>1) {
   for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[wh])[or[t]],",")[[1]])
    actt <-rep(0,length(tmp1)-1)
    for (o in 1:(length(tmp1)-1)) {
     actt[o] <- 2*(sum(tmp2[1:o])-sum(tmp1[1:o]))^2
    }
    act[t+1] <- sum(actt)/(length(tmp1)-1)
   }
   }
  }
  if (orq[qiq==rsq[q]]==1) {
    act[1] <- (length(tmp1)-1)*(1/length(tmp1))^2+(1-1/length(tmp1))^2
   if (lt>1) {
    for (t in 1:(lt-1)) {
     tmp2 <- as.double(strsplit(as.vector(nvt[wh])[or[t]],",")[[1]])
     act[t+1] <- sum( (tmp2-tmp1)^2 )
    }
   }
  }

  dex <- which(time[1:(lt-1)]>=(tstart+(w-1)*7*24*60*60)&time[1:(lt-1)]<(tstart+w*7*24*60*60))
  if (length(dex)>0) {
   acqu[w,q] <- sum(act[dex]*acd[dex])/sum(acd[dex])
  }
  if (length(dex)<=0&time[1]<(tstart+(w-1)*7*24*60*60)&time[lt]>=(tstart+(w-1)*7*24*60*60)) {
   acqu[w,q] <- sum(act[dex]*acd[dex])/sum(acd[dex])
  }
}
oq[w] <- length(acqu[w,][is.na(acqu[w,])==F])
}

smuth <- function(x) {y<-x; for (i in 2:(length(x)-1)) { y[i] <- x[i-1]*0.25+x[i]*0.5+x[i+1]*0.25}; return(y)}		# Different smoothing function
png("AcpQpM.png", width = 7200, height = 3600, pointsize = 18, res = 360)
plot(mnths,rep(1,length(mnths)),col=rgb(1,1,1),ylim=c(0,2.05),ylab="Brier Score over Questions",main=paste("Accuracy over ",length(rsq)," Active, Public Questions",sep=""),xlim=c(1,length(mnths)),xlab="Month")
for (q in 1:length(rsq)) {
 points(mnths,acqu[,q],pch="-",cex=2,col=rgb(0.8,0.8,0.8))
}
 axis(4,at=seq(0,2,0.5))
bxplt <- function(x) {quantile(x,c(0.25,0.75,0.50),na.rm=T)}
for (p in 1:3) {
 points(2,quantile(acqu[2,],c(0.25,0.75,0.50),na.rm=T)[p],pch="-",cex=3*ceiling(p/2))
}
for (p in 1:3) {
 points(mnths[2:(length(mnths)-1)],smuth(apply(acqu[1:length(mnths),],1,bxplt)[p,])[2:(length(mnths)-1)],pch="-",cex=3*ceiling(p/2))
}
for (p in 1:3) {
 points(open,quantile(acqu[open,],c(0.25,0.75,0.50),na.rm=T)[p],pch="-",cex=3*ceiling(p/2))
}
 text(0,2.05,0,col=rgb(0.8,0.8,0.8))
for (w in 1:length(mnths)) {
 text(mnths[w],2.05,oq[w],col=rgb(0.8,0.8,0.8))
}
text(1,0.15,"25%"); text(1,0.40,"50%"); text(1,0.65,"75%")
dev.off()