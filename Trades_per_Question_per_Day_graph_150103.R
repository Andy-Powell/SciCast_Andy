start <- Sys.time()

# Calculating number of questions per day and average number of trades per question per day
nt.q <- nc.q <- qu <- numeric()
for (d in 1:max(days)) {
 qu[d] <- length(levels(factor(c(qit[tat>=tstart+(d-1)*60*60*24 &tat<tstart+d*60*60*24],qic[cac>=tstart+(d-1)*60*60*24 &cac<tstart+d*60*60*24]))))
 if (qu[d] > 0) {
  nt.q[d] <- nt[d]/qu[d]; nc.q[d] <- nc[d]/qu[d]
 }
 else {nt.q[d] <- nc.q[d] <- 0}
}

nq <- numeric()
for (d in 1:max(days)) {
 nq[d] <- length(levels(factor(qiq[raq>=tstart+(d-1)*60*60*24&caq<tstart+d*60*60*24])))
} 

date <- Sys.Date()                                                        #Adding Date to title
title <- paste("Trades_per_Question_per_Day ",date,".png", collapse="")      #Expanding title name

# Graphing over time
png(title, width = 7200, height = 3600, pointsize = 18, res = 360)
par(mar=c(5,4,4,4))
plot(days,nt.q,type="l",lwd=1,col=rgb(0.99,0.6,0.6),xaxt="n",ylim=c(0,ceiling(max(nq)/24)),ylab="",xlab="Date")
  lines(smooth.spline(days[1:ts],smth(nt.q[1:ts]),df=ceiling(ts/5),all.knots=T),lwd=3,col=rgb(0.95,0,0))   # smoothed Forcasts/ active question
 lines(days,nc.q,lty=2,lwd=1,col=rgb(0.99,0.6,0.6))   #what does this plot?
  lines(smooth.spline(days[1:ts],smth(nc.q[1:ts]),df=ceiling(ts/3),all.knots=T),lty=2,lwd=3,col=rgb(0.95,0,0)) #Comments / Active question
 lines(days,qu^(1/2),lwd=1,col=rgb(0.6,0.6,1))   #raw trades / active question
  lines(smooth.spline(days[1:ts],smth(qu[1:ts]^(1/2)),df=ceiling(ts/5),all.knots=T),lwd=3,col=rgb(0,0,1))   #smoothed trades / active question
 #lines(days,nq^(1/2),lty=2,lwd=1,col=rgb(0.6,0.6,1))  #raw open questions?
 lines(days,nq/24,lty=2,lwd=1,col=rgb(0.6,0.6,1))
  #lines(smooth.spline(days[1:ts],smth(nq[1:ts]^(1/2)),df=ceiling(ts/3),all.knots=T),lty=2,lwd=3,col=rgb(0,0,1))  #smoothed open questions
 lines(smooth.spline(days[1:ts],smth(nq[1:ts]/24),df=ceiling(ts/3),all.knots=T),lty=2,lwd=3,col=rgb(0,0,1))
 mtext("Activity per Day", outer=T,side=2,line=-1.5,font=1,col=rgb(0.95,0,0))
 par(las=2)
 axis(1,at=days,lab=label[1:length(days)])
 #axis(4,at=seq(0,ceiling(max(nq)^(1/2)),2),lab=seq(0,ceiling(max(nq)^(1/2)),2)^2)     #Axis for open questions
 axis(4,at=seq(0,ceiling(max(nq)/24),2.5),lab=seq(0,ceiling(max(nq)/24),2.5)*24)
 par(las=0)
 mtext("Questions per Day", outer=T,side=4,line=-1.5,font=1,col=rgb(0,0,1))
 mtext('based on "de-stuttered" forecasts', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
 text(65,10.5,"Open Questions",col=rgb(0,0,1),cex=0.9)
 text(65,6.5,"Active Questions",col=rgb(0,0,1),cex=0.9)
 text(65,0.75,"Forecasts per Active Question",col=rgb(0.95,0,0),cex=0.9)
 text(65,-0.40,"Comments per Active Question",col=rgb(0.95,0,0),cex=0.9)
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Trades_per_Questsion_per_Day graph completed")
print(duration)