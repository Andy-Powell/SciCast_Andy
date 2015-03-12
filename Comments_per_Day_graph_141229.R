start <- Sys.time()


## dynamically creating the y asix labels  ###
mnthly <- as.character(c(rep("",6),8,rep("",6),15,rep("",6),22,rep("",6)))    # sets day number for each week starting on the first day of each month
#label <- as.character(c(25,rep("",5),"Dec 1",mnthly,rep("",3),"Jan 1",mnthly,rep("",3),"Feb 1",mnthly,"Mar 1",mnthly,rep("",3),"Apr 1",mnthly,rep("",2),"May 1",mnthly,rep("",3),"Jun 1",mnthly,rep("",2),"Jul 1",mnthly,rep("",3),"Aug 1",mnthly,rep("",3),"Sep 1",mnthly,rep("",2),"Oct 1",mnthly,rep("",3),"Nov 1",mnthly,rep("",2),"DEC 1",mnthly,rep("",3)))

labelMnthStr <- c("Dec 1","Jan 1","Feb 1","Mar 1","Apr 1","May 1","Jun 1","Jul 1","Aug 1","Sep 1","Oct 1","Nov 1")   # begining day of each month
labelRemDays <- c(3,3,0,3,2,3,2,3,3,2,3,2)                                                                           # labels each of the first 4 weeks, these are the left over day
#labelDf <- data.frame(labelMnthInt,labelMnthStr,labelRemDays)



numMnths <- function(end_date, start_date) {             # fucntion to calculate the number of months between dates
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
diffMnths <- numMnths(Sys.Date(),"2013-11-25")           # number of months since the marketr started

label <- c(25,rep("",5))
yrs <- 0

for(mon in 1:diffMnths) {                                # number of months the market has been open 
  mnth <- mon - yrs*12                                   # month counter minus years
  label <- c(label,labelMnthStr[mnth],mnthly,rep("",labelRemDays[mnth]))  # adds the first day lable and weekly labels
  if (mnth==12) {                                        # resets month counter to 1 each 12 months
    yrs <- yrs + 1
  }
}



nc <- numeric()
for (d in 1:max(days)) {
 nc[d] <- length(cac[cac>=tstart+(d-1)*60*60*24&cac<tstart+d*60*60*24])
}

date <- Sys.Date()                                              #Adding Date to title
title <- paste("Comments_per_Day ",date,".png", collapse="")    #Expanding title name

png(title, width = 7200, height = 3600, pointsize = 18, res = 360)
par(mar=c(5,4,4,4))
plot(days,nc,type="l",lwd=3,xaxt="n",ylim=c(0,max(nc)*1.25),ylab="Comments per Day",xlab="Date")
 par(las=2)
 axis(1,at=days,lab=label[1:length(days)])
mtext('Comments per Day', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
dev.off()

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Comments_per_Day graph completed")
print(duration)


