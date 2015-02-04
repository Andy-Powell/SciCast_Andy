start <- Sys.time()

# Questions traded per person
qpu <- rep(0,length(pip))
for (i in 1:length(pip)) {
 qpu[i] <- length(unique(qit[pit==pip[i]]))
}
date <- Sys.Date()                                                        #Adding Date to title
title <- paste("Activity_per_Person (3 or more trades) ",date,".png", collapse="")        #Expanding title name

breaks <- 2^seq(0,10,1)
png(title, width = 3600, height = 3600, pointsize = 18, res = 360)
 hist(qpu[qpu>2],freq=T,breaks=breaks,xlim=c(3,2^10),xlab="Questions Forecast per Active User",main="")
 #hist(qpu[qpu>0],freq=T,breaks=breaks,xlim=c(1,2^10),xlab="Questions Forecast per Active User",main="")
 #hist(qpu[qpu>0],freq=T,breaks=breaks,xlim=c(0,2^10),xlab="Questions Forecast per Active User",main="")
 mtext('based on "de-stuttered" forecasts', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
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