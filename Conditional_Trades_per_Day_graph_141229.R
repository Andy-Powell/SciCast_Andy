start <- Sys.time()

rq <- qiq[is.na(ld)==F]										# Linked questions
#ptc <- length(qit[as!="None"])/length(qit[qit%in%rq])						# Portion of Conditional trades on eligible questions

nq <- nrq <- numeric(); ntc <- rep(0,length(days))
for (d in 1:max(days)) {
 nq[d] <- length(levels(factor(qiq[raq>=tstart+(d-1)*60*60*24&caq<tstart+d*60*60*24])))
 nrq[d] <-length(levels(factor(qiq[qiq%in%rq&raq>=tstart+(d-1)*60*60*24&caq<tstart+d*60*60*24])))	# Number of linked questions
 ntc[d] <-length(qit[as!="None"&tat>=tstart+(d-1)*60*60*24&tat<tstart+d*60*60*24])		# Number of conditional trades on eligible questions
# if (nrq[d]>0) {
#  ntc[d] <-length(qit[as!="None"&tat>=tstart+(d-1)*60*60*24&tat<tstart+d*60*60*24])/nrq[d]	# Portion of conditional trades on eligible questions
# }										
}

date <- Sys.Date()                                                        #Adding Date to title
title <- paste("Conditional_Trades_per_Day ",date,".png", collapse="")       #Expanding title name

png(title, width = 7200, height = 3600, pointsize = 18, res = 360)
par(mar=c(5,4,4,4))
plot(days,ntc,type="l",lwd=1,col=rgb(0.99,0.6,0.6),xaxt="n",ylim=c(0,ceiling(max(nq)/6)),ylab="",xlab="Date")
  lines(smooth.spline(days[1:ts],smth(ntc[1:ts]),df=ceiling(ts/2),all.knots=T),lwd=3,col=rgb(0.95,0,0))
 lines(days,nrq/6,lwd=1,col=rgb(0.6,0.6,1))
  lines(smooth.spline(days[1:ts],smth(nrq[1:ts]/6),df=ceiling(ts/3),all.knots=T),lwd=3,col=rgb(0,0,1))
 lines(days,nq/6,lty=2,lwd=1,col=rgb(0.6,0.6,1))
  lines(smooth.spline(days[1:ts],smth(nq[1:ts]/6),df=ceiling(ts/3),all.knots=T),lty=2,lwd=3,col=rgb(0,0,1))
 mtext("Forecasts per Day", outer=T,side=2,line=-1.5,font=1,col=rgb(0.95,0,0))
 par(las=2)
 axis(1,at=days,lab=label[1:length(days)])
 axis(4,at=seq(0,ceiling(max(nq)/6),10),lab=seq(0,ceiling(max(nq)/6),10)*6)
 par(las=0)
 mtext("Questions per Day", outer=T,side=4,line=-1.5,font=1,col=rgb(0,0,1))
 mtext('based on "de-stuttered" forecasts', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
 text(40,30,"Open Questions",col=rgb(0,0,1),cex=0.9)
 text(40,12,"Linked Questions",col=rgb(0,0,1),cex=0.9)
 text(40,5.5,"Conditional Forecasts on Linked Questions",col=rgb(0.95,0,0),cex=0.9)
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Conditional_Trades_per_Day graph completed")
print(duration)