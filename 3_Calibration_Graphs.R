# Broken down by question type, but ordered, multiple-choice questions should probably be treated differently!

# supr is the list of super forecaster IDs (B is when based on Brier score.).

# Calibration of all Edits
estqb <- estqm <- estqs <- oqb <- oqm <- oqs <- numeric()
for (t in 1:length(qit)) {
# if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]) {			# Switch which of these two lines is "commented" in order to control whether all edits or superforecaster edits are used.
 if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]&pit[t]%in%suprB) {	# Switch which of these two lines is "commented" in order to control whether all edits or superforecaster edits are used.
  oq <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]]); lo <- length(oq)
  es <- as.double(strsplit(as.vector(nvt[t]),",")[[1]])
# No concern about duration of edit
  if (orq[qiq==qit[t]]==1& lo==2) { # binary questions
   oqb <- c(oqb,oq[2])
   estqb <- c(estqb,es[2])
  }
  if (orq[qiq==qit[t]]==2& lo==2) { # scaled, continuous questions (calculated the same as binary questions)
   oqs <- c(oqs,oq[2])
 if (es[2]>0.95) {print(t)}
   estqs <- c(estqs,es[2])
  }
  if (lo>2) { # multiple-choice questions
   oqm <- c(oqm,oq)
   estqm <- c(estqm,es)
  }
 }
}

bin <- seq(0,1,0.10); bin[1] <- -0.0001; lnb <- length(bin)-1
corlb <- confbm <- confbe <- calibm <- calibe <- corls <- confsm <- confse <- calism <- calise <- corlm <- confmm <- confme <- calimm <- calime <- rep(0.5,lnb)
for (b in 1:lnb) {
 confbm[b] <- mean(estqb[estqb>bin[b]&estqb<=bin[b+1]]); confbe[b] <- sd(estqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(estqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 calibm[b] <- mean(oqb[estqb>bin[b]&estqb<=bin[b+1]]); calibe[b] <- sd(oqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(oqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 corlb[b] <- cor(estqb[estqb>bin[b]&estqb<=bin[b+1]],oqb[estqb>bin[b]&estqb<=bin[b+1]])
 confsm[b] <- mean(estqs[estqs>bin[b]&estqs<=bin[b+1]]); confse[b] <- sd(estqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(estqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 calism[b] <- mean(oqs[estqs>bin[b]&estqs<=bin[b+1]]); calise[b] <- sd(oqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(oqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 corls[b] <- cor(estqs[estqs>bin[b]&estqs<=bin[b+1]],oqs[estqs>bin[b]&estqs<=bin[b+1]])
 confmm[b] <- mean(estqm[estqm>bin[b]&estqm<=bin[b+1]]); confme[b] <- sd(estqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(estqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 calimm[b] <- mean(oqm[estqm>bin[b]&estqm<=bin[b+1]]); calime[b] <- sd(oqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(oqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 corlm[b] <- cor(estqm[estqm>bin[b]&estqm<=bin[b+1]],oqm[estqm>bin[b]&estqm<=bin[b+1]])
}

plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event")
 lines(confbm,calibm,col=rgb(0.95,0,0))
points(confsm,calism,pch=17,col=rgb(0,0.9,0))
 lines(confsm,calism,col=rgb(0,0.9,0))
points(confmm,calimm,pch=16,col=rgb(0,0,1))
 lines(confmm,calimm,col=rgb(0,0,1))
abline(0,1)
 legend(0.75,0.25,c("Binary","Continuous","Multi-Choice"),pch=c(15,17,16),col=c(rgb(0.95,0,0),rgb(0,0.9,0),rgb(0,0,1)))

 library(ellipse)
#png("CaliEdits.png", width = 4800, height = 2400, pointsize = 18, res = 360)
png("CaliEditsSuprB.png", width = 4800, height = 2400, pointsize = 18, res = 360)
par (mfrow=c(1,3))
plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Binary Questions")
 lines(confbm,calibm,col=rgb(0.95,0,0))
# arrows(x,ll,x,ul, length = 0.25, angle = 90, code=3)
 for (b in 1:lnb) {
  lines(ellipse(corlb[b],centre =c(confbm[b],calibm[b]),scale =c(confbe[b],calibe[b]),level = 0.90),col=rgb(0.95,0,0),lty=3)
 }
abline(0,1)
plot(confsm,calism,pch=17,col=rgb(0,0.9,0),xlim=c(0,1),xlab="Market Estimate of Quantity",ylim=c(0,1),ylab="Observed Quantity",main="Scaled, Continuous Questions")
 lines(confsm,calism,col=rgb(0,0.9,0))
 for (b in 1:lnb) {
  lines(ellipse(corls[b],centre =c(confsm[b],calism[b]),scale =c(confse[b],calise[b]),level = 0.90),col=rgb(0,0.9,0),lty=3)
 }
abline(0,1)
plot(confmm,calimm,pch=16,col=rgb(0,0,1),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Multiple-Choice Questions")
 lines(confmm,calimm,col=rgb(0,0,1))
 for (b in 1:lnb) {
  lines(ellipse(corlm[b],centre =c(confmm[b],calimm[b]),scale =c(confme[b],calime[b]),level = 0.90),col=rgb(0,0,1),lty=3)
 }
abline(0,1)
dev.off()

# Calibration of Safe-Mode (ULinOP)
estqb <- estqm <- estqs <- oqb <- oqm <- oqs <- numeric()
for (t in 1:length(qit)) {
#for (t in 1:1000) {
# if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]&mdt[t]==1) { #&pit[t]%in%supr
 if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]&mdt[t]==1) {
  oq <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]]); lo <- length(oq)
  es <- frcst[t,1:lo]
# No concern about duration of edit
  if (orq[qiq==qit[t]]==1& lo==2) { # binary questions
   oqb <- c(oqb,oq[2])
   estqb <- c(estqb,es[2])
  }
  if (orq[qiq==qit[t]]==2& lo==2) { # scaled, continuous questions (calculated the same as binary questions)
   oqs <- c(oqs,oq[2])
   estqs <- c(estqs,es[2])
  }
  if (lo>2) { # multiple-choice questions
   oqm <- c(oqm,oq)
   estqm <- c(estqm,es)
  }
 }
}

bin <- seq(0,1,0.10); bin[1] <- -0.0001; lnb <- length(bin)-1
corlb <- confbm <- confbe <- calibm <- calibe <- corls <- confsm <- confse <- calism <- calise <- corlm <- confmm <- confme <- calimm <- calime <- rep(0.5,lnb)
for (b in 1:lnb) {
 confbm[b] <- mean(estqb[estqb>bin[b]&estqb<=bin[b+1]]); confbe[b] <- sd(estqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(estqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 calibm[b] <- mean(oqb[estqb>bin[b]&estqb<=bin[b+1]]); calibe[b] <- sd(oqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(oqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 corlb[b] <- cor(estqb[estqb>bin[b]&estqb<=bin[b+1]],oqb[estqb>bin[b]&estqb<=bin[b+1]])
 confsm[b] <- mean(estqs[estqs>bin[b]&estqs<=bin[b+1]]); confse[b] <- sd(estqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(estqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 calism[b] <- mean(oqs[estqs>bin[b]&estqs<=bin[b+1]]); calise[b] <- sd(oqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(oqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 corls[b] <- cor(estqs[estqs>bin[b]&estqs<=bin[b+1]],oqs[estqs>bin[b]&estqs<=bin[b+1]])
 confmm[b] <- mean(estqm[estqm>bin[b]&estqm<=bin[b+1]]); confme[b] <- sd(estqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(estqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 calimm[b] <- mean(oqm[estqm>bin[b]&estqm<=bin[b+1]]); calime[b] <- sd(oqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(oqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 corlm[b] <- cor(estqm[estqm>bin[b]&estqm<=bin[b+1]],oqm[estqm>bin[b]&estqm<=bin[b+1]])
}

plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event")
 lines(confbm,calibm,col=rgb(0.95,0,0))
points(confsm,calism,pch=17,col=rgb(0,0.9,0))
 lines(confsm,calism,col=rgb(0,0.9,0))
points(confmm,calimm,pch=16,col=rgb(0,0,1))
 lines(confmm,calimm,col=rgb(0,0,1))
abline(0,1)
 legend(0.75,0.25,c("Binary","Continuous","Multi-Choice"),pch=c(15,17,16),col=c(rgb(0.95,0,0),rgb(0,0.9,0),rgb(0,0,1)))

 library(ellipse)
png("CaliSafeMode.png", width = 4800, height = 2400, pointsize = 18, res = 360)
par (mfrow=c(1,3))
plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Binary Questions")
 lines(confbm,calibm,col=rgb(0.95,0,0))
# arrows(x,ll,x,ul, length = 0.25, angle = 90, code=3)
 for (b in 1:lnb) {
  lines(ellipse(corlb[b],centre =c(confbm[b],calibm[b]),scale =c(confbe[b],calibe[b]),level = 0.90),col=rgb(0.95,0,0),lty=3)
 }
abline(0,1)
plot(confsm,calism,pch=17,col=rgb(0,0.9,0),xlim=c(0,1),xlab="Market Estimate of Quantity",ylim=c(0,1),ylab="Observed Quantity",main="Scaled, Continuous Questions")
 lines(confsm,calism,col=rgb(0,0.9,0))
 for (b in 1:lnb) {
  lines(ellipse(corls[b],centre =c(confsm[b],calism[b]),scale =c(confse[b],calise[b]),level = 0.90),col=rgb(0,0.9,0),lty=3)
 }
abline(0,1)
plot(confmm,calimm,pch=16,col=rgb(0,0,1),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Multiple-Choice Questions")
 lines(confmm,calimm,col=rgb(0,0,1))
 for (b in 1:lnb) {
  lines(ellipse(corlm[b],centre =c(confmm[b],calimm[b]),scale =c(confme[b],calime[b]),level = 0.90),col=rgb(0,0,1),lty=3)
 }
abline(0,1)
dev.off()


# Calibration with safe-mode substitution (all forecasts)
estqb <- estqm <- estqs <- oqb <- oqm <- oqs <- numeric()
for (t in 1:length(qit)) {
# if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]) { #&pit[t]%in%supr
 if (qit[t]%in%rsq&asqt[t]%in%c(-1,rsq)&asot[t]==roqat[t]) {
  oq <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]]); lo <- length(oq)
  es <- frcst[t,1:lo]
# No concern about duration of edit
  if (orq[qiq==qit[t]]==1& lo==2) { # binary questions
   oqb <- c(oqb,oq[2])
   estqb <- c(estqb,es[2])
  }
  if (orq[qiq==qit[t]]==2& lo==2) { # scaled, continuous questions (calculated the same as binary questions)
   oqs <- c(oqs,oq[2])
   estqs <- c(estqs,es[2])
  }
  if (lo>2) { # multiple-choice questions
   oqm <- c(oqm,oq)
   estqm <- c(estqm,es)
  }
 }
}

bin <- seq(0,1,0.10); bin[1] <- -0.0001; lnb <- length(bin)-1
corlb <- confbm <- confbe <- calibm <- calibe <- corls <- confsm <- confse <- calism <- calise <- corlm <- confmm <- confme <- calimm <- calime <- rep(0.5,lnb)
for (b in 1:lnb) {
 confbm[b] <- mean(estqb[estqb>bin[b]&estqb<=bin[b+1]]); confbe[b] <- sd(estqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(estqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 calibm[b] <- mean(oqb[estqb>bin[b]&estqb<=bin[b+1]]); calibe[b] <- sd(oqb[estqb>bin[b]&estqb<=bin[b+1]]) / length(oqb[estqb>bin[b]&estqb<=bin[b+1]])^(1/2)
 corlb[b] <- cor(estqb[estqb>bin[b]&estqb<=bin[b+1]],oqb[estqb>bin[b]&estqb<=bin[b+1]])
 confsm[b] <- mean(estqs[estqs>bin[b]&estqs<=bin[b+1]]); confse[b] <- sd(estqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(estqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 calism[b] <- mean(oqs[estqs>bin[b]&estqs<=bin[b+1]]); calise[b] <- sd(oqs[estqs>bin[b]&estqs<=bin[b+1]]) / length(oqs[estqs>bin[b]&estqs<=bin[b+1]])^(1/2)
 corls[b] <- cor(estqs[estqs>bin[b]&estqs<=bin[b+1]],oqs[estqs>bin[b]&estqs<=bin[b+1]])
 confmm[b] <- mean(estqm[estqm>bin[b]&estqm<=bin[b+1]]); confme[b] <- sd(estqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(estqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 calimm[b] <- mean(oqm[estqm>bin[b]&estqm<=bin[b+1]]); calime[b] <- sd(oqm[estqm>bin[b]&estqm<=bin[b+1]]) / length(oqm[estqm>bin[b]&estqm<=bin[b+1]])^(1/2)
 corlm[b] <- cor(estqm[estqm>bin[b]&estqm<=bin[b+1]],oqm[estqm>bin[b]&estqm<=bin[b+1]])
}

plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event")
 lines(confbm,calibm,col=rgb(0.95,0,0))
points(confsm,calism,pch=17,col=rgb(0,0.9,0))
 lines(confsm,calism,col=rgb(0,0.9,0))
points(confmm,calimm,pch=16,col=rgb(0,0,1))
 lines(confmm,calimm,col=rgb(0,0,1))
abline(0,1)
 legend(0.75,0.25,c("Binary","Continuous","Multi-Choice"),pch=c(15,17,16),col=c(rgb(0.95,0,0),rgb(0,0.9,0),rgb(0,0,1)))

 library(ellipse)
png("CaliForecasts.png", width = 4800, height = 2400, pointsize = 18, res = 360)
par (mfrow=c(1,3))
plot(confbm,calibm,pch=15,col=rgb(0.95,0,0),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Binary Questions")
 lines(confbm,calibm,col=rgb(0.95,0,0))
# arrows(x,ll,x,ul, length = 0.25, angle = 90, code=3)
 for (b in 1:lnb) {
  lines(ellipse(corlb[b],centre =c(confbm[b],calibm[b]),scale =c(confbe[b],calibe[b]),level = 0.90),col=rgb(0.95,0,0),lty=3)
 }
abline(0,1)
plot(confsm,calism,pch=17,col=rgb(0,0.9,0),xlim=c(0,1),xlab="Market Estimate of Quantity",ylim=c(0,1),ylab="Observed Quantity",main="Scaled, Continuous Questions")
 lines(confsm,calism,col=rgb(0,0.9,0))
 for (b in 1:lnb) {
  lines(ellipse(corls[b],centre =c(confsm[b],calism[b]),scale =c(confse[b],calise[b]),level = 0.90),col=rgb(0,0.9,0),lty=3)
 }
abline(0,1)
plot(confmm,calimm,pch=16,col=rgb(0,0,1),xlim=c(0,1),xlab="Market Estimate of Event Probability",ylim=c(0,1),ylab="Observed Probability of Event",main="Multiple-Choice Questions")
 lines(confmm,calimm,col=rgb(0,0,1))
 for (b in 1:lnb) {
  lines(ellipse(corlm[b],centre =c(confmm[b],calimm[b]),scale =c(confme[b],calime[b]),level = 0.90),col=rgb(0,0,1),lty=3)
 }
abline(0,1)
dev.off()