pid <- rep(0,length(un))
for (i in 1:length(un)) {
 if (length(pus[pus==un[i]])>0) {
  pid[i] <- pip[pus==un[i]]
 }
# else { pid[i] <- NA }
}
#pid <- pid[complete.cases(pid)==TRUE]

acis <- acdis <- acisr <- acdisr <- rep(-1,length(pid))
for (i in 1:length(pid)) {
 if (length(aci[pio==pid[i]])==0) {
  acis[i] <- acdis[i] <- acisr[i] <- acdisr[i] <- NA
 }
 else {
  acis[i] <- aci[pio==pid[i]]; acdis[i] <- acdi[pio==pid[i]]
  acisr[i] <- aci[pio==pid[i]]^(1/4); acdisr[i] <- acdi[pio==pid[i]]^(1/8)					# Transformation of variables to achieve normality
 }
}

# hist(acis); hist(acdis)
acdisr[acdis>2] <- NA; acdis[acdis>2] <- NA

#g <-lm(acisr ~ age+gndr+edu+see*crt+aott)
#summary(g)
#anova(g)

#g <-lm(acisr ~ age+aott+see)
#summary(g)
#anova(g)

# Only aot seems to help at all.
g <-lm(acisr ~ aott)
summary(g)
anova(g)
 
par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="Log response")
plot(g$fitted,acisr[is.na(acisr)==F&is.na(aott)==F],xlab="Fitted",ylab="Residuals",main="Log response")

cor(aott,acisr,use="complete.obs")
plot(aott,acisr)
abline(g$coefficients[1],g$coefficients[2],lwd=2)

g <-lm(acis ~ aott)
plot(aott,acis)
abline(g$coefficients[1],g$coefficients[2],lwd=2)

#png("demoBS.png", width = 1728, height = 2160, pointsize = 10, res = 360)
#plot(crt[ge==1],BSid[ge==1],col="red",pch=16, xlab="Cognitive Reflection Score",ylab="Mean Change in Market Brier Score per Trade",ylim=c(-0.30,0.25))
#points(crt[ge==0],BSid[ge==0],pch=1)
#abline(g$coefficients[1],g$coefficients[3],lwd=2)
#abline(g$coefficients[1]+g$coefficients[2],g$coefficients[3]+g$coefficients[4],col="red",lwd=3,lty=3)
# legend(0.25,-0.15,c("No Graduate Degree","Graduate Degree"),lwd=c(2,3),lty=c(1,3),pch=c(1,16),col=c("black","red"))
#dev.off()
