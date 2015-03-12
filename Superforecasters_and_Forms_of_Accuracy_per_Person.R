

pil <- lb$user_id; msl <- lb$max_score
pio <- lmi <-msi<-aci<-acdi <-ati <- sort(unique(pito)); uni <- rep("a",length(pio))
for (i in 1:(length(pio)-1)) {
 aci[i] <- mean(BSo[pito==pio[i]],na.rm=T)
 acdi[i]<-mean(BSdo[pito==pio[i]],na.rm=T)
 msi[i] <- tp[pip==pio[i]]
 lmi[i] <- msl[pil==pio[i]]
 uni[i] <- pus[pip==pio[i]]
 ati[i] <- atpu[pip==pio[i]]
}

# hist(aci); hist(acdi)

# What's the correlation between activity and points/score?
cor(lmi,ati)
cor(aci,ati)

# Does score on destuttered edits correlate with BS?

cor(lmi,aci,use="complete.obs")							# Main correlation

fpi <- rep(0,length(aci))
for (i in 1:length(aci)) {
 fpi[i] <- fpu[pip==pio[i]]
}

# Find Superforecasters!
or <- order(aci[aci<2&fpi>20])
#nspr <- floor(0.02*length(pio))
nspr <- 70
suprB <- (pio[aci<2&fpi>20][or])[1:nspr]									# Super Forecasters based on Brier score
suprBn <- rep("a",nspr); suprBr <- suprBa <- suprBp <- rep(0,nspr)
for (i in 1:nspr) {
 suprBn[i] <- uni[pio==suprB[i]]
 suprBr[i] <- aci[pio==suprB[i]]
 suprBa[i] <- fpi[pio==suprB[i]]
 suprBp[i] <- lmi[pio==suprB[i]]
}

# write.table(data.frame(suprB,suprBn,suprBp,suprBr,suprBa),file=paste("Brier_score_super_forecasters",tstop,".csv",sep=""),sep=",",append=F,col.names=c("user_id","username","market_score","Brier_score","edits"),row.names=F)

or <- order(msi[fpi>20],decreasing=TRUE)
suprM <- (pio[fpi>20][or])[1:nspr]
suprMn <- rep("a",nspr); suprMr <- suprMa <- suprMp <- rep(0,nspr)						# Super Forecasters based on market score (sum of points change from de-stuttered trades on resolved qeustions)
for (i in 1:nspr) {
 suprMn[i] <- uni[pio==suprM[i]]
 suprMr[i] <- aci[pio==suprM[i]]
 suprMa[i] <- fpi[pio==suprM[i]]
 suprMp[i] <- msi[pio==suprM[i]]
}

or <- order(lmi[fpi>20],decreasing=TRUE)
suprM <- (pio[fpi>20][or])[1:nspr]
suprMn <- rep("a",nspr); suprMr <- suprMa <- suprMp <- rep(0,nspr)						# Super Forecasters based on market score (leaderboard max score for today)
for (i in 1:nspr) {
 suprMn[i] <- uni[pio==suprM[i]]
 suprMr[i] <- aci[pio==suprM[i]]
 suprMa[i] <- fpi[pio==suprM[i]]
 suprMp[i] <- lmi[pio==suprM[i]]
}