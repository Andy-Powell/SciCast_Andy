# Cleaning up question groups

ctq <- qn$categories
orq <- qn$is_ordered
orq <- as.double(orq)
rvq <- qn$resolution_value_array
svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40))
roqt <- roqat <-rep(-1,length(tat))

wtg <- rep(0,length(qiq))
gq <- array(numeric(), c(length(qiq),200))
for (j in 1:length(qiq)) {
  if(grq[j]!="") {
    temp <- levels(factor(strsplit(as.character(grq[j]),",")[[1]]))
    wtg[j]<-1/length(temp); gq[j,1:(length(temp))]<- temp
  }
}

# Find resolved questions.
'%ni%' <- Negate('%in%')
gpq <- matrix(rep("a",length(qiq)*200),c(length(qiq),200)); vldq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
  tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]]); lv <- length(tmp)
  if (lv>0) {  gpq[q,1:lv] <- tmp }
  if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }
  
  

rsq <- levels(factor(qiq[saq<=Sys.time()&caq>tstart&ctq!="Study 2.1"&ctq!="Study 2.1,Study 2.1"&"Public"%in%gq&vldq==1]))  # Restrict to public questions.
frc <- numeric(); rqb <- length(rsq)
for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]&pit%in%pip[igrp==0]])}					# Removing questions that have almost no (non-internal) forecasts
rsq <- rsq[frc>2]; rqa <- length(rsq)											# Unused HPV cluster question: rsq <- c(rsq,546); 
rqb-rqa														# Number of low -activity questions
hist(frc); quantile(frc,1-0.02); length(frc[frc>200])									# Number of high-activity questions

for (t in 1:length(tat)) {
  temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==qit[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  if (is.na(temp1[1])==F) {
    rvqt[t,1:length(temp1)] <- temp1
    if (mdt[t]>0) {
      dflt <- (1-rst[t])/(length(temp1)-1); svt[t,1:length(temp1)] <- rep(dflt,length(temp1))				# Assume non-attended options have uniform distribution.
      svt[t,(cit[t]+1)] <- rst[t]
    }
    if (sum(temp1%%1)==0) {													# Not mixture resolutions
      roqt[t] <- which(rvqt[t,]==1)-1
    }
  }
  if (asqt[t]%in%rsq) {
    temp2 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==asqt[t]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
    if (is.na(temp2[1])==F) {
      rvqat[t,1:length(temp2)] <- temp2
      if (sum(temp2%%1)==0) {
        roqat[t] <- which(rvqat[t,]==1)-1
      }
    }
  }
}


qito <- qit[qit %in% rsq]
tato <- tat[qit %in% rsq]
nvto <- nvt[qit %in% rsq]
pito <- pit[qit %in% rsq]
asqto <- asqt[qit %in% rsq]
asoto <- asot[qit %in% rsq]
roqato <- roqat[qit %in% rsq]
svto <- svt[qit %in% rsq,]
mdto <- mdt[qit %in% rsq]

or <-order(qito,tato)
qito <-qito[or]
tato <-tato[or]
nvto <-nvto[or]
asqto <-asqto[or]
asoto <-asoto[or]
roqato <-roqato[or]
svto <- svto[or,]
mdto <- mdto[or]

rvto <- fvto <- array(rep(-1,length(qito)*40),c(length(qito),40))

rvqa <- array(rep(-1,length(rsq)*40),c(length(rsq),40))
for (j in 1:length(rsq)) {
 temp <- as.double(strsplit(strsplit(strsplit(as.character(rvq[qiq==rsq[j]]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
 rvqa[j,1:length(temp)] <- temp
}
 orqo <- rep(0,length(qito))
for (j in 1:length(qito)) {
 rvto[j,] <- rvqa[rsq==qito[j]]
 if (mdto[j]==0) {
  temp <- as.double(strsplit(as.vector(nvto[j]),",")[[1]])
 }
 if (mdto[j]==1) {
  temp <- svto[j,]										# Substitute raw response of safe mode.
 }
 fvto[j,1:length(temp)] <- temp
 orqo[j] <- orq[qiq==qito[j]]
}

BSo <- rep(2,length(qito))										# Score of forecast
for (j in 1:length(qito)) {
 l <- length(rvto[j,][rvto[j,]>0])
 if (asqto[j]%in%c(-1,rsq)&(asoto[j]==roqato[j]|asoto[j]==-1)) {
  if (orqo[j]==2) {
   ac <-rep(0,l-1)
   for (o in 1:(l-1)) {
    ac[o] <- 2*(sum(fvto[j,1:o])-sum(rvto[j,1:o]))^2
   }
   BSo[j] <- mean(ac)
  }
  if (orqo[j]==1) {
   BSo[j] <- sum( (fvto[j,1:l]-rvto[j,1:l])^2 )
  }
 }
 else { BSo[j] <- NA }
}

duro <- rep(0,length(qito))										# Duration of forecast
for (j in 1:length(qito)){
 if (qito[j]!=qito[j+1]|j==length(qito)) {
  duro[j] <- (raq[qiq==qito[j]]-base)-(tato[j]-base)
 }
 else {
  duro[j] <- (tato[j+1]-base)-(tato[j]-base)
 }
}

BSdo <- BSo*duro

###########################################
# Expected points
###########################################

library(rjson)
#library(RJSONIO)

#scicast <- as.POSIXct("2013-11-25 00:00:00 EST")
#fd <- floor(Sys.time()-scicast)+scicast-1*60*60
fd <- Sys.Date()
fl <- paste("expected_value_",fd,".json",sep="")
download.file("https://s3.amazonaws.com/daggre_datamart_production/trade_involvement.json",destfile=fl)
#jti <- fromJSON(readLines(fl))
jti <- fromJSON("https://s3.amazonaws.com/daggre_datamart_production/trade_involvement.json", method = "R", unexpected.escape = "error")             #this might work
close(fl)                                                                   #this might work
ti <- unlist(jti)
id <- ev <- rep(-1,length(ti))
for (t in 1:length(ti)) {
 id[t] <- as.double(names(ti[t]))
 ev[t] <- ti[t][[1]]
}

print("0")

ep <- rep(0,length(tat))
for (t in 1:length(tat)) {
  print("x")
 w <- which(id==tit[t])
 if (length(w)>0) {
  ep[t] <- ev[w]
 }
}

print("1")

lp <- length(pip)
etp <- rep(0,lp)
for (i in 1:lp) {
 etp[i] <- sum(ep[pit==pip[i]])
}

print("2")

# How many people are broke?
hist(etp,1000)
length(etp[etp<0])
po <- ep[qit%in%rsq]
tp <- rep(0,lp)
for (i in 1:lp) {
 tp[i] <- sum(po[pito==pip[i]])
}
length(tp[tp<(-4000)])

print("3")

# What's the correlation between activity and points on destuttered edits?
cor(etp,atpu)
cor(tp,atpu)

# Does score on destuttered edits correlate with BS?

cor(po,BSo,use="complete.obs")

for (q in 1:length(rsq)) {
 w <- which(qito==rsq[q]); l <- length(w)						# Data already sorted
 if (l>0) {
  BSo[w][1] <- NA; BSo[w][l] <- NA						# Removing first and last trades on question
 }
 if (l>3) {
  BSo[w][1:2] <- NA; BSo[w][(l-1):l] <- NA						# Removing early and late trades on question
 }
 if (l>5) {
  BSo[w][1:3] <- NA; BSo[w][(l-2):l] <- NA						# Removing early and late trades on question
 }
}

cor(po,BSo,use="complete.obs")

print("4")

###########################################
# Expected Brier score
###########################################
# For now, expectation is based on t-1 marginal probability forecasts. We shouldn't need this kludge.
# There's a ticket to engineers to add the latest marginal and conditional probabilities to either:
#    parts of data mart
# OR the trade_involvement file.

 library(Matrix)
eBS <- rep(0,length(tat))
# Until the necessary probabilities are added to the datamart, I assume that the most recent prob entered by a user is the current market prob.
for (t in 1:length(tat)) {
 tmp2 <- as.double(strsplit(as.vector(nvt[t]),",")[[1]])
 temp1 <- as.character(rvq[qiq==qit[t]])
 if (temp1[1]!="None") {
  if (asqt[t]>-1) {
   temp2 <- as.character(rvq[qiq==asqt[t]])
   if (temp2[1]!="None") {
    thing1 <- strsplit(strsplit(strsplit(temp1,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],","); thing <- strsplit(thing1[[1]][2],' ',fixed=T); if (length(thing[[1]])>1) {thing1 <- as.double(strsplit(strsplit(strsplit(temp1,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],", ")[[1]])}
    thing2 <- strsplit(strsplit(strsplit(temp2,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],","); thing <- strsplit(thing2[[1]][2],' ',fixed=T); if (length(thing[[1]])>1) {thing2 <- as.double(strsplit(strsplit(strsplit(temp2,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],", ")[[1]])}
    tmp1 <- thing1*thing2[asot[t]+1] # See "if (sum(tmp1)==0)" below
   }
   if (temp2[1]=="None") {
    wa <- which(tat==max(tat[qit==asqt[t]&asqt==-1]))
    tmp1 <- as.double(strsplit(strsplit(strsplit(temp1,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])*(as.double(strsplit(as.vector(nvt[wa]),",")[[1]])[asot[t]+1]) # See "if (sum(tmp1)==0)" below
   }
  }
  if (asqt[t]==-1) {
    tmp1 <- as.double(strsplit(strsplit(strsplit(temp1,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
  }
 }

 if (temp1[1]=="None") {
  wm <- which(tat==max(tat[qit==qit[t]&asqt==-1]))
  if (asqt[t]>-1) {
   temp2[1] <- as.character(rvq[qiq==asqt[t]])
   if (temp2[1]!="None") {
    thing1 <- as.double(strsplit(as.vector(nvt[wm]),",")[[1]]);
    thing2 <- strsplit(strsplit(strsplit(temp2,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],","); thing <- strsplit(thing2[[1]][2],' ',fixed=T); if (length(thing[[1]])>1) {thing2 <- as.double(strsplit(strsplit(strsplit(temp2,"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],", ")[[1]])}
    tmp1 <- thing1*thing2[asot[t]+1] # See "if (sum(tmp1)==0)" below
   }
   if (temp2[1]=="None") {
    wa <- which(tat==max(tat[qit==qit[t]&asqt==asqt[t]]))
    tmp1 <- as.double(strsplit(as.vector(nvt[wa]),",")[[1]])
   }
  }
  if (asqt[t]==-1) {
   tmp1 <- as.double(strsplit(as.vector(nvt[wm]),",")[[1]])
  }
 }

 if (sum(tmp1)==0) {
  eBS[t] <- NA
 }
 if (sum(tmp1)>0) {
  lt <- length(tmp1); l <- 1:lt
  if (orq[qiq==qit[t]]==1) {						# for unordered questions
   eBS[t] <- sum((tmp2-1)^2*tmp1+tmp2^2*(1-tmp1))
  }
  if (orq[qiq==qit[t]]==2& lt>2) {					# for ordered, multiple-choice questions
   eBS[t] <- 2*sum((cumsum(tmp2)-1)^2*cumsum(tmp1)+cumsum(tmp2)^2*(1-cumsum(tmp1)))/(lt-1)			# Multiply by two rather than sum in both directions.
  }
  if (orq[qiq==qit[t]]==2& lt==2 &temp1[1]!="None"&temp2[1]!="None") {	# for scaled, continuous (ordered, single-choice) questions
   eBS[t] <- sum((tmp2-tmp1)^2)										# easy when resolution is known
  }
  if (orq[qiq==qit[t]]==2& lt==2 &(temp1[1]=="None"|temp2[1]=="None")) {	# for scaled, continuous (ordered, single-choice) questions
   tmp1v <- seq(0.025,0.975,0.05); tmp1d <- rep(0,10)
   for (v in 1:length(tmp1v)) {											# Assume triangular distribution with mode at tmp1 when resolution is unknown.
    if (tmp1v[v]<=tmp1[2]) {
     tmp1d[v] <- tmp1v[v]*2/tmp1[2]
    }
    if (tmp1v[v]>tmp1[2]) {
     tmp1d[v] <- (tmp1v[v]-1)*2/(tmp1[2]-1)
    }    
   }
   tmp1d <- tmp1d/sum(tmp1d)		# normalized
   eBS[t] <- 2*sum(tmp1d*(tmp2[2]-tmp1v)^2)
  }
 }
}

cor(ep,eBS,use="complete.obs")

good <- complete.cases(eBS)
sum(!good)
