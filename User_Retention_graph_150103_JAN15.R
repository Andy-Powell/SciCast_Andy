start <- Sys.time()

rstart <-as.POSIXlt("2013-12-01 00:00:00 EST")
#open <- ceiling(as.double(as.POSIXlt("2014-12-31 00:00:00 EDT")-rstart)/30.5)
open <- ceiling(as.double(as.POSIXlt("2015-02-28 00:00:00 EDT")-rstart)/30.5)              #need to automate
month <-c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb")   #need to automate
user <- levels(factor(pit)); first <-last <-rep(0,length(user))
for (i in 1:length(user)) {
 first[i] <-as.double(min(tat[pit==user[i]])-rstart)/30.5; last[i] <-as.double(max(tat[pit==user[i]])-rstart)/30.5
}

date <- Sys.Date()                                                        #Adding Date to title
title <- paste("User_Retention ",date,".png", collapse="")                #Expanding title name

png(title, width = 3600, height = 3600*5/16*open, pointsize = 18, res = 360)					# High resolution image
par(oma=c(4.5,2.25,0.5,0),mar=c(1.5,4,0.25,2.25),mfrow=c(open,1),cex.main=1.5,cex.axis=0.5); bukts<-seq(0,open,1)		# Outer margins set on a plot with 9 subplots.
  histdata <- hist(last[first>=(open-1)&first<open],breaks=bukts,plot=FALSE)
  barplot((histdata$count)^(1/2),space=0,ylab=month[open], xlab = "",main="",ylim=c(0,3500^(1/2)),xaxt="n",yaxt="n",col="black")
  axis(2, at=c(0,27,103,300,1000,3000)^(1/2), lab=c(0,30,100,300,1000,3000),tick=T)
  axis(4, at=c(0,27,103,300,1000,3000)^(1/2), lab=c(0,30,100,300,1000,3000),tick=T)
for (m in (open-1):1) {
  histdata <- hist(last[first>= (m-1) & first< m ],breaks=bukts,plot=FALSE)
  barplot((histdata$count)^(1/2),space=0,ylab=month[m], xlab = "",main="",ylim=c(0,3500^(1/2)),xaxt="n",yaxt="n",col="black")	# Shows duration of participation by month.
  axis(2, at=c(0,27,103,300,1000,3000)^(1/2), lab=c(0,30,100,300,1000,3000),tick=T)
  axis(4, at=c(0,27,103,300,1000,3000)^(1/2), lab=c(0,30,100,300,1000,3000),tick=T)
}
par(cex.axis=1)
  axis(1, at=seq(0.5,(open-1+0.5),1), lab=month[1:open],tick=F)								# Final plot retains x-axis/scale.
 mtext("Month of Last Forecast", outer=T,side=1,line=1.25,font=2);								# Main axis labels
 mtext('based on "de-stuttered" forecasts', outer=T,side=1,line=2.5,cex=0.5,font=1,col=rgb(0,0,0))
 mtext("Month of First Forecast", outer=T,side=2,line=0.25,font=2); mtext("       Number of Participants", outer=T,side=2,line=-2,font=2, cex=0.5)
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("User_Retention graph completed")
print(duration)