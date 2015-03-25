start <- Sys.time()
  
pit <- th$user_id
qit <- th$question_id
pic <- cm$user_id



# Questions traded per person
qpu <- rep(0,length(pip))
for (i in 1:length(pip)) {
 qpu[i] <- length(unique(qit[pit==pip[i]]))
}
date <- Sys.Date()                                                        #Adding Date to title
title <- paste("Activity_per_Person ",date,".png", collapse="")        #Expanding title name

br <- 2^seq(0,11,1)
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
#activity <- hist(qpu[qpu>2],breaks=br, freq=T)
activity <- hist(qpu[qpu>0],freq=T, breaks=br, plot=F)
#plot(activity$count, log="y", type='h', lwd=10, lend=2)
plot(activity$mids,activity$counts, type='h', lwd=10, lend=2,col=rgb(1,0,0,0.5),log="xy", xaxt='n',xlab="Number of Users", ylab="Number of Questions",main=paste("Questions Forecast per Active User, ",expStart," to ",trunc(expStop-24*60*60,"days"),sep=""))               
axis(1, at=c(activity$breaks[2:12]),labels=c(activity$breaks[2:12]))
#plot(activity$counts, type='h', lwd=10, col=rgb(1,0,0,0.5),log="y", xlab="Questions Forecast per Active User", ylab="Number of Questions",main=paste("Questions Forecast per Active User, ",expStart," to ",trunc(expStop-24*60*60,"days"),sep=""))
#hist(qpu[qpu>2],freq=T,breaks=breaks,xlim=c(3,2^10),xlab="Questions Forecast per Active User",main="")
 ##hist(qpu[qpu>0],freq=T,breaks=breaks,xlim=c(1,2^10),xlab="Questions Forecast per Active User",main="")
 ##hist(qpu[qpu>0],freq=T,breaks=breaks,xlim=c(0,2^10),xlab="Questions Forecast per Active User",main="")
 #mtext('Activity per Person', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
dev.off()

#Forecasts per person
fpu <- rep(0,length(pip))
for (i in 1:length(pip)) {
 fpu[i] <- length(qit[pit==pip[i]])
}

#
# Activities per person
#lp <- length(pip)
atpu <- rep(0,length(pip))
for (i in 1:length(pip)) {
 atpu[i] <- length(tat[pit==pip[i]])+length(cac[pic==pip[i]])
}

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Actiities_per_Person graph completed")
print(duration)