start <- Sys.time()

# Creating new variables
sit <- nit <- rep(0,length(qit))
# Finding "no-impact" forecasts
for (t in 2:length(tat)) {
 if (qit[t]==qit[t-1]) {
  sit[t] <- sum(abs(as.double(strsplit(as.vector(nvt[t]),",")[[1]]) - as.double(strsplit(as.vector(nvt[t-1]),",")[[1]])))	# Difference in edits
  if (sit[t]<0.01) {
   nit[t] <- 1															# Small edits have nit value 1.
  }
 }
}

# Calculating number of trades and no-impact trades for each day
nt <- nzt <- numeric()
for (d in 1:max(days)) {
 nt[d] <- length(tat[tat>=tstart+(d-1)*60*60*24&tat<tstart+d*60*60*24])
 nzt[d] <- length(tat[tat>=tstart+(d-1)*60*60*24&tat<tstart+d*60*60*24&nit==1])
}

ts <- length(days)-2

date <- Sys.Date()                                           #Adding Date to title
title <- paste("Trades_per_Day ",date,".png", collapse="")      #Expanding title name

png(title, width = 7200, height = 3600, pointsize = 18, res = 360)
par(mar=c(5,4,4,4))
plot(days,nt^(1/2),type="l",lty=2,lwd=1,col=rgb(0.95,0,0),xaxt="n",yaxt="n",ylim=c(-4^(1/2),ceiling(max(nt)*1.05)^(1/2)),ylab="Forecasts per Day",xlab="Date")
 lines(days,nzt^(1/2),lty=2,lwd=1,col=rgb(0,0,1))
 par(las=2)
 axis(1,at=days,lab=label[1:length(days)])
 axis(2,at=seq(0,ceiling(max(nt)*1.05)^(1/2),4),lab=seq(0,ceiling(max(nt)*1.05)^(1/2),4)^2); axis(4,at=seq(0,ceiling(max(nt)*1.05)^(1/2),4),lab=seq(0,ceiling(max(nt)*1.05)^(1/2),4)^2)
 lines(smooth.spline(days[1:ts],nzt[1:ts]^(1/2),df=ceiling(length(nt)/6),nknots=ts),col=rgb(0,0,1),lwd=3)
 lines(smooth.spline(days[1:ts],nt[1:ts]^(1/2),df=ceiling(length(nt)/6),nknots=ts),col=rgb(0.95,0,0),lwd=3)
 text(160,300^(1/2),"All forecasts",col=rgb(0.95,0,0))
 text(160,-4^(1/2),"No-Impact forecasts",col=rgb(0,0,1))
 par(las=1)
 mtext('"De-Stuttered" Forecasts', outer=T,side=3,line=-2,font=1,col=rgb(0,0,0))
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Trades_per_Day graph completed")
print(duration)
