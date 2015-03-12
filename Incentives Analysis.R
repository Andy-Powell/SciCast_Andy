# !Analyze market per day rather than people per day!
# !In new experiment, check that people didn't make their scores worse for few days before the contest!
# ! Monitor number active users per day and week, number of forecasts per day, and portions of forecasts on two groups of questions!

#############################################
#Third study of incentives aimed at accuracy
#############################################

# This uses a two-stage model:
# In stage 1, number of forecasts per question per day is modeled, and in stage 2, number of forecasts per question per day is a predictor.
# In stage 2, accuracy per trade is modeled.  Accuracy should be assessed with Brier score and the change in log score, which is equivalent to the points a person gained or lost.

# Stage 1 predictors of forecasts per question per day:
# b1 day in or out of study
# b2&3 day for question set A or B
# b4 days until start of question eligibility (people freeing up points to spend or making room for improvement)
# b5 days until end of question elibibility (liquidity issue)
# b6&7 days since question set A or B revealed
# b8 days since study announced
# b9 number of new registrations
# b10 days since SciCast launched
# b? week of year (nominal variable)
# b11&12 question in set A or B
# b13 number of comments on question that day
# b14 days until question must close
# b15 days before question resolved
# (IMPOSSIBLE) how often question was recommended
# b16 = b2*b11 (maybe replaced with whether question was eligible that day)
# b17 = b3*b12 (maybe replaced with whether question was eligible that day)
# b18 = b6*b11 (maybe replaced with days since question eligibility revealed)
# b19 = b7*b12 (maybe replaced with days since question eligibility revealed)

# Stage 2 predictors of accuracy per forecast:
# b1 Stage 1 predictions of number of forecasts on the same question and day
# b2 day in or out of study
# b3&4 day for question set A or B
# b5 days until start of question eligibility (people freeing up points to spend or making room for improvement)
# b6 days until end of question elibibility (liquidity issue)
# b7&8 days since question set A or B revealed
# b9 days since study announced
# b10 days since SciCast launched
# b? week of year (nominal variable)
# b11&12 question in set A or B
# b13 number of comments on question that day
# b14 days until question expected to resolve
# b15 days before question resolved
# b16 = b3*b11 (maybe replaced with whether question was eligible that day)
# b17 = b4*b12 (maybe replaced with whether question was eligible that day)
# b18 = b7*b11 (maybe replaced with days since question eligibility revealed)
# b19 = b8*b12 (maybe replaced with days since question eligibility revealed)
# b20 size of trade
# b? time of day (four-hour blocks?)
# b21 user's previous activity on question
# (IMPROBABLE) user's assets available before trade
# b22 user's days since registration
# b23 user's previous number of forecasts on question
# b24 user's previous number of comments on question
# b25 user's previous average forecasts per day since registered
# b26 conditional or marginal forecast
# b? user's completion of tutorial (up to 10 tutorials)
# b27 = b3*b11*b23
# b28 = b4*b12*b23
# b29 = b3*b11*b25
# b30 = b4*b12*b25

# Putting all the predictor variables into the model is bad form.  First explore the data to see whether the variables seem worthwhile.
# in the unlike case that they all are promising, the models would look like the below.
# g <- lm(BSpf ~ b0 +b1 +b2+b3 +b4 +b5 +b6 +b7 +b8 +b9 +b10 +b11 +b12 +b13 +b14 +b15 +b2:b11 +b3:b12 +b6:b11 +b7:b12)
# summary(g)
# Check the p-values, sizes of the coefficients, and r^2.  Hierarchical regression should isolate the explanatory power of each variable.

#############################################


#############################################
#First and Second studies of incentives
#############################################

#
# Pull csv files from data mart.
# Read  files pulled from Data Mart.

#setwd("<favorite directory>")

setInternet2(use = TRUE)

#download.file("http://datamart.scicast.org/trade_history/?format=csv&api_key=<APIKEY>",destfile="trade_history_report.csv")
 th<-read.csv("trade_history_report.csv")											# 3 empty variables
#download.file("http://datamart.scicast.org/question_history/?format=csv&api_key=<APIKEY>",destfile="question_history_report.csv")
 qh<-read.csv("question_history_report.csv")
#download.file("http://datamart.scicast.org/question/?format=csv&api_key=<APIKEY>",destfile="question_report.csv")
 qn<-read.csv("question_report.csv")
#download.file("http://datamart.scicast.org/comment/?format=csv&api_key=<APIKEY>",destfile="comment_report.csv")
 cm<-read.csv("comment_report.csv")											# 2 empty variables
#download.file("http://datamart.scicast.org/person/?format=csv&api_key=<APIKEY>",destfile="person_report.csv")
 pr<-read.csv("person_report.csv")


# Read file of winning activities in Experiment 1
 wa<-read.csv("winning_activities.csv")

# Read file of winnings in Experiment 2
 ws<-read.csv("Daily_Wins.csv")

#
# Removing admin and *internal* accounts and activity
pip <- pr$user_id; pus <- as.character(pr$username); cap <- as.POSIXct(pr$created_at); grps <- pr$groups
adu <- c("amsiegel","BAE11","brnlsl","brobins","cedarskye","christinafreyman","ctwardy","daggre_admin","dquere","gbs_tester","Inkling","jessiejury","jlu_bae","kennyth0","klaskey","kmieke","manindune","Naveen Jay","pthomas524","Question_Admin","Question Mark","randazzese","RobinHanson","saqibtq","scicast_admin","slin8","ssmith","tlevitt","wsun")
adi <- numeric()
for (i in 1:length(adu)) {
 adi[i] <- pip[pus==adu[i]]
 cap[pip==adi[i]] <- NA
}

lp <- length(pip)

grp <- array(rep("a",lp*20),c(lp,20))
for (i in 1:lp) {
 temp <- as.vector(strsplit(as.character(grps[i]),",")[[1]])
 grp[i,1:length(temp)] <- temp
}
adi <- numeric()
for (i in 1:lp) {
 for (g in 1:20) {
  if (grp[i,g]=="Internal"|grp[i,g]=="Admin"|grp[i,g]=="SuperAdmin"|grp[i,g]=="UserAdmin"|grp[i,g]=="BadgesAdmin"|grp[i,g]=="RolesAdmin"|grp[i,g]=="QuestionAdmin") {
   cap[i] <- NA
   adi <- c(adi,pip[i])
  }
 }
}
adi <- unique(adi)

good <- complete.cases(cap)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grps<-grps[good]

pit <- th$user_id; qit <- th$question_id; tat <- as.POSIXct(th$traded_at); nvt <- th$new_value_list; ovt <- th$old_value_list; as <- th$serialized_assumptions; apot <- th$assets_per_option; tit <- th$trade_id
cit <- th$choice_index; rust <- as.character(th$raw_user_selection)
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\",[0.9545454545454546,1],null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\",[0.9545454545454546,1],null]"
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\", [0.9545454545454546, 1], null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\", [0.9545454545454546,1], null]" 
rust[rust=="[\"\\\"Less than 4.75%\\\"\",[-0.33333333333333326,-0.33333333333333326],null]"] <- "None"
rust[rust=="[\"\\\"Between 4.75% and 5%\\\"\",[-0.33333333333333326,0.33333333333333326],\"Lower\"]"] <- "None"
rust[rust=="[\"\\\"Less than 4.75%\\\"\", [-0.33333333333333326, -0.33333333333333326], null]"] <- "None"
rust[rust=="[\"Down ~12% or more\",[-0.21428571428571427,0.5],null]"] <- "None"
rust[rust=="[\"Up ~12% or more\",[0.842857142857143,1.2142857142857142],null]"] <- "None"

rst <- mdt <- rep(0,length(rust))
for (t in 1:length(rust)) {
 m <- strsplit(rust[t],",")[[1]][1]
 if (m!="None") {
  mdt[t] = 1												# 1 indicates safe-mode forecast.
  tmp1 <- as.double(strsplit(as.vector(ovt[t]),",")[[1]])						# A later selection on SciCast.org like "Higher" will assume the user wants the forecast halfway between the current market estimate and the top of the bin.
  tmp2 <- strsplit(strsplit(rust[t],',')[[1]][4],'"',fixed=T)[[1]][2]
  if (is.na(tmp2)==T) { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
  if (is.na(tmp2)==F) { 
   if (tmp2=="Lower" ) { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(strsplit(rust[t],',')[[1]][2],',')[[1]][1],'[',fixed=T)[[1]][2]) -tmp1[cit[t]+1])/2 }
   if (tmp2=="Higher") { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)) -tmp1[cit[t]+1])/2 }
   if (tmp2=="What they are now") { rst[t] <- tmp1[cit[t]+1]}
   if (tmp2=="null") { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
  }
 }
}

for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA
}
good <- complete.cases(tat)
sum(!good)     												# How many are not good?
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]; tit<-tit[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

pic <- cm$user_id; cac <- as.POSIXct(cm$created_at); qic <- cm$question_id
for (i in 1:length(adi)) {
 cac[pic==adi[i]] <- NA
}
good <- complete.cases(cac)
sum(!good)     												# How many are not good?
cac<-cac[good]; pic<-pic[good]; qic<-qic[good]

pip[pip%in%unique(c(pit,pic))==F] <- NA									# Removing users who never had activity

good <- complete.cases(pip)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grps<-grps[good]

lp <- length(pip)

#
# Removing new forecasts

start <- as.POSIXct("2013-11-25 00:00:00 EST"); stop <- as.POSIXct("2014-09-22 00:00:00 EST") 		# Stop on a Monday.
tat[tat>stop] <- NA
good <- complete.cases(tat)
sum(!good)
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; as<-as[good]; tit<-tit[good]

cac[cac>stop] <- NA
good <- complete.cases(cac)
sum(!good)
cac<-cac[good]; pic<-pic[good]; qic<-qic[good]

#
# NOT Removing stuttered forecasts

daysopen <- seq(0,ceiling(as.double(stop - start))-1,1)
days <- max(daysopen)+1
 base <- start-28*24*60*60
ord <- order(qit,tat)
tat<-tat[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; as<-as[ord]; tit<-tit[ord]

#
# Activities per person
atpu <- rep(0,lp)
for (i in 1:lp) {
 atpu[i] <- length(tat[pit==pip[i]])+length(cac[pic==pip[i]])
}

breaks <- 2^seq(0,13,1)
png("ATpAP.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 hist(atpu[atpu>0],freq=T,breaks=breaks,xlim=c(0,5000),xlab="Questions Forecast per Active User",main="")
 mtext('based on "de-stuttered" forecasts', outer=T,side=3,line=-2,cex=0.75,font=1,col=rgb(0,0,0))
dev.off()

pip[atpu<4] <- NA											# Removing users who had fewer than X activities seems to make no difference.
good <- complete.cases(pip)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grps<-grps[good]

lp <- length(pip)

papu <- apu <- smpu <- mcpu <- ampu <- typu <- tcpu <- acpu <- rep(0,lp*days)				# Placeholders for values of variables on every day and every active user
# previous day's activities, activities per user, surpise merits, merits with gift cards, announced merits, surprise thank-yous, thank-yous with gift cards, all cards per user
for (i in 1:lp) {
 temp1 <- which(pit==pip[i]); temp2 <- which(pic==pip[i])						# Matching up users across data files for person, trades, and comments
 if ((length(temp1)+length(temp2))>0) {
  dd <- ceiling(c(tat[temp1],cac[temp2])-start); l <- length(dd)					# Activity includes both forecasts and comments.
  for (d in 1:l) {
   apu[(i-1)*days+dd[d]] <- apu[(i-1)*days+dd[d]] +1
  }
 }
}

for (i in 1:lp) {
 for (d in 2:days) {
  papu[(i-1)*days+d] <- apu[(i-1)*days+d-1]								# Activity one day ago
 }
}

# Setup													# These are mostly dummy variables.
day <- daysopen%%7+1
 mon <- rep(c(1,0,0,0,0,0,0),days/7); tue <- rep(c(0,1,0,0,0,0,0),days/7); wed <- rep(c(0,0,1,0,0,0,0),days/7); thu <- rep(c(0,0,0,1,0,0,0),days/7);
 fri <- rep(c(0,0,0,0,1,0,0),days/7); sat <- rep(c(0,0,0,0,0,1,0),days/7); sun <- rep(c(0,0,0,0,0,0,1),days/7)
 monp <- rep(mon,lp); tuep <- rep(tue,lp); wedp <- rep(wed,lp); thup <- rep(thu,lp);
 frip <- rep(fri,lp); satp <- rep(sat,lp); sunp <- rep(sun,lp)
Date <- rep(start,length(daysopen)); DST <- as.POSIXct("2014-03-09 00:00:00 EST")
for (t in 2:length(Date)) {
 Date[t] <- Date[t-1]+24*60*60
 if (Date[t-1]==DST) {
  Date[t] <- Date[t]-1*60*60
 }
}
announcement <- as.POSIXct("2014-05-20 17:00:00 EDT"); announcement2 <- as.POSIXct("2014-07-17 17:00:00 EDT")	# Dates when studies 1 and 2 were announced
la <- ceiling(stop-announcement); ann <- c( rep(0,floor(announcement-start)),(la-(1:la)) ); annp <- rep(ann,lp); la2 <- ceiling(stop-announcement2); ann2 <- c( rep(0,floor(announcement2-start)),(la2-(1:la2)) ); ann2p <- rep(ann2,lp)
# Assuming the effect of the announcement weakens over time

advertisement1 <- as.POSIXct("2014-06-14 08:00:00 EDT"); advertisement2 <- as.POSIXct("2014-06-24 08:00:00 EDT"); advertisement3 <- as.POSIXct("2014-06-29 08:00:00 EDT"); advertisement4 <- as.POSIXct("2014-07-11 08:00:00 EDT")
ads <- c( rep(0,floor(advertisement1-start)),rep(1,floor(advertisement2-advertisement1)),rep(0.25,floor(advertisement3-advertisement2)),rep(1,floor(advertisement4-advertisement3)),rep(0.25,ceiling(stop-advertisement4)) ); adsp <- rep(ads,lp)
recommender <- as.POSIXct("2014-06-17 08:00:00 EDT")
rec <- c( rep(0,floor(recommender-start)),rep(1,ceiling(stop-recommender)) ); recp <- rep(rec,lp)

# Incentives dummy variables
istart <- as.POSIXct("2014-05-26 00:00:00 EDT"); istop <- as.POSIXct("2014-06-23 00:00:00 EDT")
incntv <- c( rep(0,ceiling(istart-start)),rep(1,ceiling(istop-istart)),rep(0,ceiling(stop-istop)) ); incntvp <- rep(incntv,lp)

i2start <- as.POSIXct("2014-07-21 00:00:00 EDT"); i2stop <- as.POSIXct("2014-08-18 00:00:00 EDT")
incntv2 <- c( rep(0,ceiling(i2start-start)),rep(1,ceiling(i2stop-i2start)),rep(0,ceiling(stop-i2stop)) ); incntv2p <- rep(incntv2,lp)
i2td <- c( rep(0,ceiling(i2start-start)),rep(4,7),rep(3,7),rep(2,7),rep(1,7),rep(0,ceiling(stop-i2stop)) ); i2tdp <- rep(i2td,lp)

# Number of questions per day
qiq <- qn$question_id; caq <- as.POSIXct(qn$created_at); grq <- as.character(qn$groups); saq <- as.character(qn$resolution_at); raq <- as.character(qn$pending_until)
#qn$provisional_settled_at is start of comment period and qn$pending_until will be reused for event resolution (not question resolution/settlement)
saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)
raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)

nq <- qd <- numeric()
for (d in 1:max(days)) {
 tmp <- levels(factor(qiq[raq<=stop&raq>=start+(d-1)*60*60*24&caq<start+d*60*60*24]))
 nq[d] <- length(levels(factor(qiq[raq>=start+(d-1)*60*60*24&caq<start+d*60*60*24])))
 qd[d] <- mean(as.double(raq[qiq%in%tmp]-start))				# Average question duration on open questions in a day
}
qd[is.na(qd)==TRUE] <- 200; qdp <- rep(qd,lp)
nqp <- rep(nq,lp)

# Number of days since registration per user
drpu <- rep(0,lp*days);
base <- start-28*24*60*60; dat <- Date-base; rap <- cap-base
for (i in 1:lp) {
# for (d in 1:days) {
#  drpu[(i-1)*days+d] <- as.double(ceiling(dat[d]-rap[i]))
# }
 drpu[((i-1)*days+1):(i*days)] <- as.double(ceiling(dat-rap[i]))
}
drpu[drpu<0] <- 0

daysopenp <- rep(daysopen,lp); Datep <- rep(Date,lp)

#
# Winning activities per user
# The file that "wa" draws from was set up by Scott Smith and Ken Olson.  It's not ideal; hence all the lines of code below to calculate who won what at which time.
piw <- wa$user_id; waw <- as.POSIXct(wa$win_date); prw <- wa$prize

ord <- order(piw,prw,waw)
piw <- piw[ord]; prw <- prw[ord]; waw <- waw[ord]
for (i in 1:lp) {
 temp1 <- which(piw[prw==1]==pip[i]); temp2 <- which(piw[prw==2]==pip[i]); temp3 <- which(piw[prw==3]==pip[i]); temp4 <- which(piw[prw==4]==pip[i]); temp5 <- which(piw[prw==5]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw[prw==1][temp1]-start),days)+1; l <- length(dd)
  for (d in 1:(l-1)) {
   smpu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d				# Allows overlap in time to account for multiple prizes to user in a day.
  }
 }
 if (length(temp2)>0) {
  dd <- c(ceiling(waw[prw==2][temp2]-start),days)+1; l <- length(dd)
  for (d in 1:(l-1)) {
   mcpu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d
  }
 }
 if (length(temp3)>0) {
  dd <- c(ceiling(waw[prw==3][temp3]-start),days)+1; l <- length(dd)
  for (d in 1:(l-1)) {
   ampu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d
  }
 }
 if (length(temp4)>0) {
  dd <- c(ceiling(waw[prw==4][temp4]-start),days)+1; l <- length(dd)
  for (d in 1:(l-1)) {
   typu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d
  }
 }
 if (length(temp5)>0) {
  dd <- c(ceiling(waw[prw==5][temp5]-start),days)+1; l <- length(dd)
  for (d in 1:(l-1)) {
   tcpu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d
  }
 }
}
#hist(smpu[smpu>0],100,xlim=c(0,100))

ty2pu <- ac2pu <- am2pu <- rc2pu <- rm2pu <- rep(0,lp*days)

piw2 <- ws$user_id; waw2 <- as.POSIXct(ws$award_date)
ty <- ws$new_accuracy_thank_yous; ac <- ws$new_activity_gift_cards; am <- ws$new_activity_merits;
rc <- ws$new_accuracy_gift_cards; rm <- ws$new_accuracy_merits

for (i in 1:lp) {
 temp1 <- which(piw2[ty>0]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw2[ty>0][temp1]-start),days); l <- length(dd)
  for (d in 1:(l-1)) {
   ty2pu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1]-1)] <- sum(ty[ty>0][temp1[1:d]])
  }
 }
}

for (i in 1:lp) {
 temp1 <- which(piw2[ac>0]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw2[ac>0][temp1]-start),days); l <- length(dd)
  for (d in 1:(l-1)) {
   ac2pu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1]-1)] <- sum(ac[ac>0][temp1[1:d]])
  }
 }
}

for (i in 1:lp) {
 temp1 <- which(piw2[am>0]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw2[am>0][temp1]-start),days); l <- length(dd)
  for (d in 1:(l-1)) {
   am2pu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1]-1)] <- sum(am[am>0][temp1[1:d]])
  }
 }
}

for (i in 1:lp) {
 temp1 <- which(piw2[rc>0]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw2[rc>0][temp1]-start),days); l <- length(dd)
  for (d in 1:(l-1)) {
   rc2pu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1]-1)] <- sum(rc[rc>0][temp1[1:d]])
  }
 }
}

for (i in 1:lp) {
 temp1 <- which(piw2[rm>0]==pip[i])
 if (length(temp1)>0) {
  dd <- c(ceiling(waw2[rm>0][temp1]-start),days); l <- length(dd)
  for (d in 1:(l-1)) {
   rm2pu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1]-1)] <- sum(rm[rm>0][temp1[1:d]])
  }
 }
}

# All merits per user up to date
amtpu <- smpu+mcpu+ampu+am2pu

for (i in 1:lp) {
 temp1 <- which(piw[prw==1]==pip[i]); temp2 <- which(piw[prw==2]==pip[i]); temp3 <- which(piw[prw==3]==pip[i]); temp4 <- which(piw[prw==4]==pip[i]); temp5 <- which(piw[prw==5]==pip[i])
 if ((length(temp2)+length(temp5))>0) {
  dd <- ceiling(c(waw[prw==2][temp2],waw[prw==5][temp5])-start)+1; ud <- unique(dd)	# Cards are received the day after activity, and the personal total increments the day after receipt.
  for (d in 1:length(ud)) {
   lcd <- length(dd[dd==ud[d]])
   if (lcd>4) {dd[which(dd==ud[d])[5:lcd]] <- NA}				# 4-card daily limit
  }
  dd <- sort(dd[complete.cases(dd)]); ld <- min(c(23,length(dd)))
  dd <- c(dd[1:ld],days)							# 23-card total limit
  for (d in 1:(length(dd)-1)) {
   acpu[((i-1)*days+dd[d]):((i-1)*days+dd[d+1])] <- d				# Allows overlap in time to account for multiple prizes to user in a day.
  }
 }
}

ctpu <- acpu +ac2pu +rc2pu							# Ken confirmed the output in these variables with Scott.

#hist(ctpu[ctpu>0],100,xlim=c(0,100))
#unique(pip[ceiling(which(ctpu>14)/days)])
#max(ctpu[pip[ceiling(1:length(ctpu)/days)]==unique(pip[ceiling(which(ctpu>14)/days)])[3]])

tnp <- np <- rep(0,days)							# Total number of people registered each day
for (d in 2:days) {
 tnp[d] <- length(pip[cap<start+d*60*60*24])
 np[d] <- tnp[d] - tnp[d-1]
} 

###########################################
# Model of Activity per User (per day)
###########################################

# Looking for nonlinear relations by squaring some variables.
drpu2 <- drpu^2; daysopenp2 <- daysopenp^2; papu2 <- papu^2; acpu2 <- acpu^2; mcpu2 <- mcpu^2; tcpu2 <- tcpu^2

# First look at correlations and scatterplots of each variable against activity to see which variables are promising.
# If you want them all in a linear model, it looks something like this:
#g <-lm(apu ~ papu+drpu+drpu2+ctpu+mtpu+smpu+mcpu+ampu+typu+tcpu+daysopenp+nqp+monp+tuep+wedp+thup+frip+satp+sunp+annp+ann2p+adsp+recp+incntvp)

# There are possible interactions, of course.
#g <-lm(apu ~ acpu+mtpu+mcpu+ampu+typu+tcpu+incntvp+incntvp:tuep+incntvp:frip+incntvp:recp+ctpu2)


# Dealing with days of the week and incentives periods as interactions is a bit silly.  Instead, create new variables.
acdp <- (tuep+frip)*incntvp; acd2p <- tuep*incntv2p; rcd2p <- thup*incntv2p; amdp <- wedp*incntvp; amd2p <- wedp*incntv2p; rmd2p <- frip*incntv2p

# Here's a model with sensible variables.
#g <-lm(apu ~ acpu:acdp +ac2pu:acd2p +ty2pu +ac2pu +am2pu +amtpu +rc2pu +rm2pu +rc2pu:rcd2p +ampu:amdp +rm2pu:rmd2p)
# Here's the model with the variables that have a real impact.
g <-lm(apu ~ acpu:acdp +ac2pu:acd2p)
summary(g)

#napu <- 1/(apu+1)						# apu is not normally distributed despite many attempts at transformation!
#g <- glm(formula= apu ~ ctpu, family= Gamma)			# Ken tried a general linear model where the dependent variable can be distributed differently, but the model didn't perform much differently.
#summary(g,dispersion=1)					# This dispersion paramater corresponds to an exponential distribution.

# Hierarchical regression
# The order of the variables goes from best to worst candidate.
g1 <-lm(apu ~ acpu)
g2 <-lm(apu ~ acpu +ac2pu)
g3 <-lm(apu ~ acpu +ac2pu +acpu:acdp)
g4 <-lm(apu ~ acpu +acpu:acdp +ac2pu +ac2pu:acd2p)
g5 <-lm(apu ~ acpu +acpu:acdp +ac2pu +ac2pu:acd2p +ty2pu)
g6 <-lm(apu ~ acpu +acpu:acdp +ac2pu +ac2pu:acd2p +ty2pu +amtpu)
g7 <-lm(apu ~ acpu +acpu:acdp +ac2pu +ac2pu:acd2p +ty2pu +amtpu +rc2pu)
g8 <-lm(apu ~ acpu +acpu:acdp +ac2pu +ac2pu:acd2p +ty2pu +amtpu +rc2pu +rm2pu)
anova(g1,g2,g3,g4,g5,g6,g7,g8)
# "When given a sequence of objects, anova tests the models against one another in the order specified."

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="")
plot(g$fitted,apu)

# Creating a graph
g <-lm(apu ~ acpu:acdp +ac2pu:acd2p)
png("Activity.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(mfrow=c(1,1),mar=c(5,4,4,2))
 plot(ctpu[(acdp*acd2p)==0],apu[(acdp*acd2p)==0],ylim=c(0,575),pch=1,cex=0.6,col=rgb(0,0,1),xlab="Previous Gift Cards",ylab="Forecasts and Comments",main="Activities per User per Day")
 points(acpu[acdp==1],apu[acdp==1],pch=2,col=rgb(0.95,0,0))
 points(ac2pu[acd2p==1],apu[acd2p==1],pch=6,col=rgb(0,0.85,0))
  abline(g$coeff[1],0,col=rgb(0,0,1),lwd=3)
  abline(g$coeff[1],g$coeff[2],col=rgb(0.95,0,0),lwd=3)
  abline(g$coeff[1],g$coeff[3],col=rgb(0,0.9,0),lwd=3)
  legend(7.5,575,c("Days Study 2 Gift Cards Offered","Days Study 1 Gift Cards Offered","Days Gift Cards NOT Offered"),lwd=c(3,3),col=c(rgb(0,0.85,0),rgb(0.95,0,0),rgb(0,0,1)))
dev.off()

# This is a way to put error bands around the predictions from the coefficients in the model, but with the huge sample size, the standard error is tiny, so the lines wouldn't even show up.
#  abline(g$coeff[1]-1.96*summary(g)$coeff[4],0,col=rgb(0,0,1),lwd=1)
#  abline(g$coeff[1]+1.96*summary(g)$coeff[4],0,col=rgb(0,0,1),lwd=1)

###########################################
# Model of Number of Users
###########################################

daysopen2 <- daysopen^2;
#g <-lm(np ~ daysopen+daysopen2+incntv+mon+tue+wed+thu+fri+sat+sun+incntv:mon+incntv:tue+incntv:wed+incntv:thu+incntv:fri+incntv:sat+incntv:sun+nq+ann+ads+rec)
nnp <- log(np+1)
acd <- (tue+fri)*incntv; acd2 <- tue*incntv2
g <-lm(nnp ~ ads)		# normalized a bit
summary(g)
anova(g)

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="Log response")
plot(g$fitted,nnp)

#______________________________________________________________________________________________________________________________________________

#
# Accuracy (Updated on 2014-10-09)
# Binary and ordered means continuous; it makes no difference to BS, but it does make a difference on "poco" and "hit".

 nowish <- strsplit(as.character(stop), ' ')[[1]][1]
asq <- aso <- rep("a",length(tat))
for (t in 1:length(tat)) {
 asq[t] <- strsplit(as.character(as[t]),':')[[1]][1]
 aso[t] <- strsplit(as.character(as[t]),':')[[1]][2]
}
asqt <- as.double(asq); asot <- as.double(aso)
asqt[is.na(asqt)==T] <- -1; asot[is.na(asot)==T] <- -1

# Find resolved questions.
'%ni%' <- Negate('%in%')
gpq <- matrix(rep("a",length(qiq)*200),c(length(qiq),200)); vldq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
 tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]]); lv <- length(tmp)
 if (lv>0) {  gpq[q,1:lv] <- tmp }
 if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }
}
# How many are invalid?
length(vldq[vldq==0])

ctq <- qn$categories; orq <- qn$is_ordered; orq <- as.double(orq); rvq <- qn$resolution_value_array; svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40)); roqt <- roqat <-rep(-1,length(tat))
#rsq <- levels(factor(qiq[raq<=Sys.time()&caq>start]))
rsq <- levels(factor(qiq[saq<=Sys.time()&caq>start&ctq!="Study 2.1"&ctq!="Study 2.1,Study 2.1"&vldq==1]))
 frc <- numeric(); rqb <- length(rsq)
 for (q in 1:length(rsq)) {frc[q] <- length(tat[qit==rsq[q]])}								# Removing questions that have almost no (non-internal) forecasts
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

###########################################
# Accuracy of Each Forecast and Trade
###########################################

qito <-qit[qit%in%rsq]; tato <-tat[qit%in%rsq]; nvto <-nvt[qit%in%rsq]; pito <- pit[qit%in%rsq]; asqto <- asqt[qit%in%rsq]; asoto <- asot[qit%in%rsq]; roqato <- roqat[qit%in%rsq]; svto <- svt[qit%in%rsq,]; mdto <- mdt[qit%in%rsq]
or <-order(qito,tato)
 qito <-qito[or]; tato <-tato[or]; nvto <-nvto[or]; asqto <-asqto[or]; asoto <-asoto[or]; roqato <-roqato[or]; svto <- svto[or,]; mdto <- mdto[or]
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

ttro <- rep(0,length(qito))										# Time from forecast to resolution
for (j in 1:length(qito)){
  ttro[j] <- (raq[qiq==qito[j]]-base)-(tato[j]-base)
}

###########################################
# Expected points
###########################################

library(rjson)

scicast <- as.POSIXct("2013-11-25 00:00:00 EST"); fd <- floor(Sys.time()-scicast)+scicast#-1*60*60
fl <- paste("expected_value_",fd,".json",sep="")
download.file("https://s3.amazonaws.com/daggre_datamart_production/trade_involvement.json",method="curl",destfile=fl)
jti <- fromJSON(readLines(fl))
ti <- unlist(jti)
id <- ev <- rep(-1,length(ti))
for (t in 1:length(ti)) {
 id[t] <- as.double(names(ti[t]))
 ev[t] <- ti[t][[1]]
}

ep <- rep(0,length(tat))
for (t in 1:length(tat)) {
 w <- which(id==tit[t])
 if (length(w)>0) {
  ep[t] <- ev[w]
 }
}
etp <- rep(0,lp)
for (i in 1:lp) {
 etp[i] <- sum(ep[pit==pip[i]])
}

# How many people are broke?
hist(etp,1000); length(etp[etp<0])
po <- ep[qit%in%rsq]
tp <- rep(0,lp)
for (i in 1:lp) {
 tp[i] <- sum(po[pito==pip[i]])
}
length(tp[tp<(-4000)])

# Does score correlate with BS?

cor(po,BSo,use="complete.obs")

# Would the correlation change is BS were based on actual responses of users (safe-mode included)?


###########################################
# Expected Brier score
###########################################
# For now, expectation is based on latest marginal probability forecasts.  This is not even a good KLUDGE.

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

#Does expected score correlate with expected BS?  Not really.
cor(ep,eBS,use="complete.obs")

good <- complete.cases(eBS)
sum(!good)

# Accuracy and expected accuracy and ETA of forecasts per User per day

BSopua <- eBSopua <- popua <- epopua <- tropua <- rep(-1,lp*days)

for (i in 1:lp) {
 temp1 <- length(pito[pito==pip[i]])
 if (temp1>0) {
  for (d in 1:(days-1)) {
   w1 <- which(pito==pip[i]); w2 <- which(tato[w1]>Date[d]); w3 <- which(tato[w1][w2]<Date[d+1])
   temp2 <- length(w3)
   if (temp2>0) {
    BSopua[(i-1)*days+d] <- mean(BSo[w1][w2][w3])
    tropua[(i-1)*days+d] <- mean(ttro[w1][w2][w3])
    popua[(i-1)*days+d] <- mean(po[w1][w2][w3])
#    BSopua[(i-1)*days+d] <- mean(BSo[pito==pip[i]&tato>Date[d]&tato<Date[d+1]])
#    tropua[(i-1)*days+d] <- mean(ttro[pito==pip[i]&tato>Date[d]&tato<Date[d+1]])
   }
  }
 }
}

for (i in 1:lp) {
 temp1 <- length(pit[pit==pip[i]])
 if (temp1>0) {
  for (d in 1:(days-1)) {
   w1 <- which(pit==pip[i]); w2 <- which(tat[w1]>Date[d]); w3 <- which(tat[w1][w2]<Date[d+1])
   temp2 <- length(w3)
   if (temp2>0) {
    eBSopua[(i-1)*days+d] <- mean(eBS[w1][w2][w3])
    epopua[(i-1)*days+d] <- mean(ep[w1][w2][w3])
   }
  }
 }
}






#   Look at overall accuracy of market on incentives days.  Ken started it, but didn't get far.






###########################################
# Models of Accuracy per User (per day)
###########################################

npu <- rep(np,lp)

BSopua[BSopua==-1] <- NA
w <- which(is.na(BSopua)==FALSE)					# Analysis is limited to users on days in which forecasts occurred.

BSpua <- BSopua[w]; trpua <- tropua[w]; ppua<- popua[w];
#eBSpua <-eBSopua[w]; eppua<- epopua[w]
npua <- npu[w]; papua<- papu[w]; papua2<- papu2[w]; drpua<- drpu[w]; drpua2<- drpu2[w]; acpua<- acpu[w]; acpua2<- acpu2[w]; ctpua<- ctpu[w]
smpua<- smpu[w]; mcpua<- mcpu[w]; mcpua2<- mcpu2[w]; ampua<- ampu[w]; typua<- typu[w]; tcpua<- tcpu[w]; acdpa <- acdp[w]; amdpa <- amdp[w]
daysopenpa<- daysopenp[w]; daysopenpa2<- daysopenp2[w]; nqpa<- nqp[w]; qdpa<- qdp[w]
monpa<- monp[w]; tuepa<- tuep[w]; wedpa<-wedp[w]; thupa<- thup[w]; fripa<- frip[w]; satpa<- satp[w]; sunpa<- sunp[w];
annpa<-annp[w]; adspa<-adsp[w]; recpa<- recp[w]; incntvpa<- incntvp[w]; incntv2pa <- incntv2p[w]; i2tdpa <- i2tdp[w]
ty2pua <- ty2pu[w]; ac2pua <- ac2pu[w]; am2pua <- am2pu[w]; amtpua <- amtpu[w]; rc2pua <- rc2pu[w]; rm2pua <- rm2pu[w]
acd2pa <- acd2p[w]; rcd2pa <- rcd2p[w]; amd2pa <- amd2p[w]; rmd2pa <- rmd2p[w]

tmp <- ppua[ppua<300&ppua>(-300)]; w <- which(ppua%in%tmp)

# POINTS!
#g <-lm(ppua[w] ~ BSpua[w] +trpua[w] +papua[w]+drpua[w]+drpua2[w]+acpua[w]+smpua[w]+mcpua[w]+ampua[w]+typua[w]+tcpua[w]+daysopenpa[w]+nqpa[w]+monpa[w]+tuepa[w]+wedpa[w]+thupa[w]+fripa[w]+satpa[w]+sunpa[w]+annpa[w]+adspa[w]+recpa[w]+incntvpa[w]+acdpa[w]+acpua[w]:acdpa[w]+ac2pua[w]:acd2pa[w]+amtpua[w]+rm2pua[w]:rmd2pa[w]+am2pua[w]:amd2pa[w]+rc2pua[w]+rm2pua[w]+ac2pua[w]+am2pua[w]+ty2pua[w])
#g <-lm(ppua[w] ~ BSpua[w] +drpua[w] +ac2pua[w] +daysopenpa[w])
g <-lm(ppua[w] ~ drpua[w] +ac2pua[w] +daysopenpa[w])
summary(g)
anova(g)

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="")
plot(g$fitted,ppua[w])

# Brier score!
#g <-lm(BSpua^(1/4) ~ ppua +npua +qdpa +trpua +papua+drpua+drpua2+acpua+smpua+mcpua+ampua+typua+tcpua+daysopenpa+nqpa+monpa+tuepa+wedpa+thupa+fripa+satpa+sunpa+annpa+adspa+recpa+incntvpa+acdpa+acpua:acdpa+ac2pua:acd2pa+amtpua+rm2pua:rmd2pa+am2pua:amd2pa+rc2pua+rm2pua+ac2pua+am2pua+ty2pua)
g <-lm(BSpua^(1/4) ~ drpua +amtpua +rm2pua +ppua+rc2pua)		# Normalizing doesn't make much difference for conclusions but does improve residuals.
g <-lm(BSpua^(1/4) ~ drpua +amtpua)
summary(g)
anova(g)

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="")
plot(g$fitted,BSpua^(1/4))

g <-lm(BSpua ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua +acpua +ac2pua +ac2pua:acd2pa +drpua)
# Hierarchical regression
g1 <-lm(BSpua^(1/4) ~ rc2pua)
g2 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa)
g3 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua)
g4 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua)
g5 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua +acpua)
g6 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua +acpua +ac2pua)
g7 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua +acpua +ac2pua +ac2pua:acd2pa)
g8 <-lm(BSpua^(1/4) ~ rc2pua +rc2pua:rcd2pa +ty2pua +rm2pua +acpua +ac2pua +ac2pua:acd2pa +drpua)
anova(g1,g2,g3,g4,g5,g6,g7,g8)

g <-lm(BSpua ~ drpua +amtpua)
summary(g)

png("Accuracy Goodish.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(mfrow=c(1,1),mar=c(5,4,4,2))
 plot(amtpua,BSpua,ylim=c(0,1.5),pch=1,cex=0.6,col=rgb(0,0,1),xlab="Previous Activity Merits",ylab="Brier Score over Forecasts",main="Accuracy per User per Day")
  abline(g$coeff[1],g$coeff[3],col=rgb(0,0,1),lwd=3)
#  legend(7.5,575,c("Days Phase 2 Gift Cards Available","Days Phase 1 Gift Cards Available","Days Gift Cards NOT Available"),lwd=c(3,3),col=c(rgb(0,0.85,0),rgb(0.95,0,0),rgb(0,0,1)))
dev.off()

# Showing that the factors that explain activity do NOT explain accuracy is very important!
g <-lm(BSpua^(1/4) ~ acpua:acdpa +ac2pua:acd2pa)
summary(g)
anova(g)

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="")
plot(g$fitted,BSpua^(1/4))

g <-lm(BSpua ~ acpua:acdpa +ac2pua:acd2pa)
png("Accuracy Blah.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(mfrow=c(1,1),mar=c(5,4,4,2))
 plot(ctpua[(acdpa*acd2pa)==0],BSpua[(acdpa*acd2pa)==0],ylim=c(0,1.5),pch=1,cex=0.6,col=rgb(0,0,1),xlab="Previous Activity Gift Cards",ylab="Brier Score over Forecasts",main="Accuracy per User per Day")
 points(acpua[acdpa==1],BSpua[acdpa==1],pch=2,col=rgb(0.95,0,0))
 points(ac2pua[acd2pa==1],BSpua[acd2pa==1],pch=6,col=rgb(0,0.85,0))
  abline(g$coeff[1],0,col=rgb(0,0,1),lwd=3)
  abline(g$coeff[1],g$coeff[2],col=rgb(0.95,0,0),lwd=3)
  abline(g$coeff[1],g$coeff[3],col=rgb(0,0.9,0),lwd=3)
  legend(6,1.25,c("Days Phase 2 Activity Gift Cards Available","Days Phase 1 Activity Gift Cards Available","Days Gift Cards NOT Available"),lwd=c(3,3),col=c(rgb(0,0.85,0),rgb(0.95,0,0),rgb(0,0,1)))
dev.off()

eBSopua[eBSopua==-1] <- NA
w <- which(is.na(eBSopua)==FALSE)

eBSpua <-eBSopua[w]; eppua<- epopua[w]
npua <- npu[w]; papua<- papu[w]; papua2<- papu2[w]; drpua<- drpu[w]; drpua2<- drpu2[w]; acpua<- acpu[w]; acpua2<- acpu2[w];
smpua<- smpu[w]; mcpua<- mcpu[w]; mcpua2<- mcpu2[w]; ampua<- ampu[w]; typua<- typu[w]; tcpua<- tcpu[w]; acdpa <- acdp[w]; amdpa <- amdp[w]
daysopenpa<- daysopenp[w]; daysopenpa2<- daysopenp2[w]; nqpa<- nqp[w]; qdpa<- qdp[w]
monpa<- monp[w]; tuepa<- tuep[w]; wedpa<-wedp[w]; thupa<- thup[w]; fripa<- frip[w]; satpa<- satp[w]; sunpa<- sunp[w];
annpa<-annp[w]; adspa<-adsp[w]; recpa<- recp[w]; incntvpa<- incntvp[w]; incntv2pa <- incntv2p[w]; i2tdpa <- i2tdp[w]
ty2pua <- ty2pu[w]; ac2pua <- ac2pu[w]; am2pua <- am2pu[w]; amtpua <- amtpu[w]; rc2pua <- rc2pu[w]; rm2pua <- rm2pu[w]
acd2pa <- acd2p[w]; rcd2pa <- rcd2p[w]; amd2pa <- amd2p[w]; rmd2pa <- rmd2p[w]

tmp <- eppua[eppua<300&eppua>(-300)]; w <- which(eppua%in%tmp)

#g <-lm(eppua[w] ~ BSpua[w] +trpua[w] +papua[w]+drpua[w]+drpua2[w]+acpua[w]+smpua[w]+mcpua[w]+ampua[w]+typua[w]+tcpua[w]+daysopenpa[w]+nqpa[w]+monpa[w]+tuepa[w]+wedpa[w]+thupa[w]+fripa[w]+satpa[w]+sunpa[w]+annpa[w]+adspa[w]+recpa[w]+incntvpa[w]+acdpa[w]+acpua[w]:acdpa[w]+ac2pua[w]:acd2pa[w]+amtpua[w]+rm2pua[w]:rmd2pa[w]+am2pua[w]:amd2pa[w]+rc2pua[w]+rm2pua[w]+ac2pua[w]+am2pua[w]+ty2pua[w])
g <-lm(eppua[w] ~ drpua[w] +BSpua[w])
summary(g)
anova(g)

#g <-lm(eBSpua^(1/4) ~ eppua+daysopenpa+npua++qdpa+incntvpa +i2tdpa+papua+drpua+drpua2+acpua+smpua+mcpua+ampua+typua+tcpua+daysopenpa+nqpa+monpa+tuepa+wedpa+thupa+fripa+satpa+sunpa+annpa+adspa+recpa+incntvpa+acdpa+acpua:acdpa+ac2pua:acd2pa+amtpua+rm2pua:rmd2pa+am2pua:amd2pa+rc2pua+rm2pua+ac2pua+am2pua+ty2pua)
#g <-lm(eBSpua^(1/4) ~ ac2pua +typua +i2tdpa +npua +daysopenpa +eppua)
g <-lm(eBSpua^(1/4) ~ daysopenpa +eppua)
summary(g)
anova(g)

par(mfrow=c(3,1))
qqnorm(g$res)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="")
plot(g$fitted,eBSpua^(1/4))

png("Accuracy.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(mfrow=c(1,1),mar=c(5,4,4,2))
 plot(npua[incntvpa==0],eBSpua[incntvpa==0],xlim=c(0,1000),ylim=c(0,2.5),col=rgb(0,0,1),xlab="New People on SciCast",ylab="Expected Brier Score",main="Accuracy per User per Day")
 points(npua[incntvpa==1],eBSpua[incntvpa==1],col=rgb(0.95,0,0))
  abline(g$coeff[1],g$coeff[2],col=rgb(0,0,1),lwd=3)
  abline(g$coeff[1]+g$coeff[3],g$coeff[2],col=rgb(0.95,0,0),lwd=3)
  legend(400,2.5,c("Phase 1 Activity Incentive Period","Other Periods"),lwd=c(3,3),col=c(rgb(0.95,0,0),rgb(0,0,1)))
dev.off()

