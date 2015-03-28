start <- Sys.time()

np <- nt.p <- pu <- numeric()
for (d in 1:max(days)) {
 pu[d] <- length(levels(factor(pit[tat>=tstart+(d-1)*60*60*24&tat<tstart+d*60*60*24])))   # pu -> number of users who traded on a given day
 np[d] <- length(pip[cap<tstart+d*60*60*24])                                              # np -> number of users created  before current day +1
 if (pu[d]>0) {
  nt.p[d] <- nt[d]/pu[d]                                                                  # nt.p -> ratio between tardes per day/number of users per day (forecasts/user?)
 }                                                                                        # nt -> number trades per day from tardes-per_Day_Graph
 else {nt.p[d] <- 0}
} 

date <- Sys.Date()                                                        #Adding Date to title
title <- paste("Trades_per_Person_per_Day ",date,".png", collapse="")        #Expanding title name

png(title, width = 7200, height = 4800, pointsize = 18, res = 360)
par(mar=c(5,4,4,4.5))
#top <- max(ceiling(max(pu)^(1/2)),ceiling(max(nt.p)))
top <- max(ceiling(max(pu)^(1/2)),ceiling(max(nt.p)))
#plot(days,nt.p,type="l",lwd=1,col=rgb(0.99,0.6,0.6),xaxt="n",yaxt="n",ylim=c(0,top),ylab="",xlab="Date")
plot(days,nt.p,type="l",lwd=1,col=rgb(0.99,0.6,0.6),xaxt="n",yaxt="n",ylim=c(0,20),ylab="",xlab="Date")
  lines(smooth.spline(days[1:ts],smth(nt.p[1:ts]),df=ceiling(ts/6),all.knots=T),lwd=3,col=rgb(0.95,0,0))
 lines(days,pu^(1/2),lwd=1,col=rgb(0.6,0.6,1))
#  lines(days,pu,lwd=1,col=rgb(0.6,0.6,1))
  lines(smooth.spline(days[1:ts],smth(pu[1:ts]^(1/2)),df=ceiling(ts/6),all.knots=T),lwd=3,col=rgb(0,0,1))
#lines(smooth.spline(days[1:ts],smth(pu[1:ts]),df=ceiling(ts/6),all.knots=T),lwd=3,col=rgb(0,0,1))
# lines(days,np^(1/2),lty=2,lwd=1,col=rgb(0.6,0.6,1))
#  lines(smooth.spline(days[1:ts],smth(np[1:ts]^(1/2)),df=ceiling(ts/2),all.knots=T),lty=2,lwd=3,col=rgb(0,0,1))
 mtext("Forecasts per Day", outer=T,side=2,line=-1.5,font=1,col=rgb(0.95,0,0))
 mtext(paste('Trades per Person per Day ', expStop, sep=""), outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
 par(las=2)
 axis(1,at=days,lab=label[1:length(days)])
 axis(2,at=c(0,2,seq(5,top,5)),lab=c(0,2,seq(5,top,5)))
# axis(4,at=c(0,2,seq(5,top,5)),lab=c(0,2,seq(5,top,5))^2)
axis(4,at=c(0,2,seq(5,400,5)),lab=c(0,2,seq(5,400,5))^2)
 par(las=0)
 mtext("Users per Day", outer=T,side=4,line=-1.5,font=1,col=rgb(0,0,1))
 text(85,0,"Forecasts per Active User",col=rgb(0.95,0,0))
 text(130,12,"Active Users",col=rgb(0,0,1))
# text(85,31,"Registered Users",col=rgb(0,0,1))
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Trades_per_Person_per_Day graph completed")
print(duration)