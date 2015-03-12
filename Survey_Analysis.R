#
# Get Survey data

#Files must be downloaded from SurveyMonkey.

# Read files.
pr<-read.csv("person_report.csv")
des<-read.csv("demographic_survey.csv")
prs<-read.csv("professional_survey.csv")
pss<-read.csv("psychology_survey.csv")
sks<-read.csv("skill_survey.csv")#

# Merge files.
ps1 <- merge(des,prs,by="Custom.Data",all=T,incomparables=NA); ps2 <- merge(pss,sks,by="Custom.Data",all=T,incomparables=NA)
prsnl <- merge(ps1,ps2,by="Custom.Data",all=T,incomparables=NA)
write.table(prsnl,file="whole_survey.csv",sep=",",append=F,row.names=F)

# Need to remove older data.
un <- prsnl$Custom.Data
tstart <- as.POSIXct("2013-11-25 01:00:00 EST")
edxx <- as.POSIXct(strptime(prsnl$EndDate.x.x,format="%m/%d/%Y %H:%M:%S")); edyx <- as.POSIXct(strptime(prsnl$EndDate.y.x,format="%m/%d/%Y %H:%M:%S"));
edxy <- as.POSIXct(strptime(prsnl$EndDate.x.y,format="%m/%d/%Y %H:%M:%S")); edyy <- as.POSIXct(strptime(prsnl$EndDate.y.y,format="%m/%d/%Y %H:%M:%S"))
ed <- (edxx-tstart)+(edyx-tstart)+(edxy-tstart)+(edyy-tstart); tshek <- rep(NA,length(ed))
for (i in 1:length(ed)) {
 if ( length(ed[un==un[i]&is.na(ed)==FALSE]) > 0 ) {
 if ( ed[i]==min(ed[un==un[i]],na.rm=T) ) {
  tshek[i] <- "X"
 }
 }
}

# PUlling variables from file.
gndr <- prsnl$What.is.your.gender.; age <- prsnl$How.old.are.you.; ctzn1 <- prsnl$Where.do.you.hold.citizenship..; ctzn2 <- prsnl$Secondary.Citizenship
educ <- prsnl$What.is.your.highest.level.of.education.; f1eld <- prsnl$In.what.field.s..is.are.your.degree.s..or.certification.s..; f2eld <- prsnl$X.15
ocup1 <- prsnl$What.is.your.primary.occupation.; ocup2 <- prsnl$X.y; ocup3 <- prsnl$What.additional.occupations.have.you.had.; ocup4 <- prsnl$X.1.y
expr <- prsnl$In.what.additional.field.s..do.you.have.a.specialty.or.expertise.

bab <- as.double(prsnl$A.bat.and.a.ball.cost..1.10.in.total..The.bat.costs.a.dollar.more.than.the.ball..How.many.cents.does.the.ball.cost...Do.not.use.any.decimals.or.any.symbols.or.letters...)
msh <- as.double(prsnl$If.it.takes.5.machines.5.minutes.to.make.5.widgets..how.many.minutes.would.it.take.100.machines.to.make.100.widgets...Enter.the.number.of.minutes..)
pts <- as.double(prsnl$In.a.lake..there.is.a.patch.of.lily.pads..Every.day..the.patch.doubles.in.size..If.it.takes.48.days.for.the.patch.to.cover.the.entire.lake..how.many.days.would.it.take.for.the.patch.to.cover.half.of.the.lake...Enter.the.number.of.days.)
see <- prsnl$Have.you.seen.the.last.three.questions.before.
nrs <- as.double(prsnl$If.it.takes.2.nurses.2.minutes.to.measure.the.blood.of.2.patients..how.many.minutes.would.it.take.200.nurses.to.measure.the.blood.of.200.patients...Enter.the.number.of.minutes.)
sas <- as.double(prsnl$Soup.and.salad.cost..5.50.in.total..The.soup.costs.a.dollar.more.than.the.salad..How.much.does.the.salad.cost..in.dollars..without.a.dollar.sign..)
tea <- prsnl$Sally.is.making.sun.tea..Every.hour..the.concentration.of.the.tea.doubles..If.it.takes.6.hours.for.the.tea.to.be.ready..how.long.would.it.take.for.the.tea.to.reach.half.of.the.final.concentration...Enter.the.number.of.hours.

aot1 <- prsnl$Allowing.oneself.to.be.convinced.by.an.opposing.argument.is.a.sign.of.good.character...
aot2 <- prsnl$People.should.take.into.consideration.evidence.that.goes.against.their.beliefs...
aot3 <- prsnl$People.should.revise.their.beliefs.in.response.to.new.information.or.evidence...
aot4 <- prsnl$Changing.your.mind.is.a.sign.of.weakness.
aot5 <- prsnl$Intuition.is.the.best.guide.in.making.decisions..
aot6 <- prsnl$It.is.important.to.persevere.in.your.beliefs.even.when.evidence.is.brought.to.bear.against.them.
aot7 <- prsnl$One.should.disregard.evidence.that.conflicts.with.one.s.established.beliefs.
aot8 <- prsnl$People.should.search.actively.for.reasons.why.their.beliefs.might.be.wrong...
aot9 <- prsnl$It.is.more.useful.to.pay.attention.to.those.who.disagree.with.us.than.to.pay.attention.to.those.who.agree...

hvf <- prsnl$When.it.comes.to.making.predictions..would.you.describe.yourself.as.more.of.a.hedgehog.or.more.of.a.fox...

# Need to remove data from admin users
adu <- c("amsiegel","BAE11","brnlsl","brobins","cedarskye","christinafreyman","ctwardy","daggre_admin","dquere","gbs_tester","Inkling","jbeauregard","jessiejury","jlu_bae","kennyth0","klaskey","kmieke","manindune","Naveen Jay","pthomas524","Question_Admin","Question Mark","randazzese","RobinHanson","saqibtq","scicast_admin","slin8","ssmith","tlevitt","wsun")
for (i in 1:length(adu)) {
 tshek[un==adu[i]]  <- NA
}

good <- complete.cases(tshek)
sum(!good)     												# How many are not good?
un<-un[good]; gndr<-gndr[good]; age<-age[good]; ctzn1<-ctzn1[good]; ctzn2<-ctzn2[good];
educ<-educ[good]; f1eld<-f1eld[good]; f2eld<-f2eld[good]; ocup1<-ocup1[good]; ocup2<-ocup2[good];
ocup3<-ocup3[good]; ocup4<-ocup4[good]; expr<-expr[good]
bab<-bab[good]; msh<-msh[good]; pts<-pts[good]; see<-see[good]; nrs<-nrs[good]; sas<-sas[good]; tea<-tea[good]
aot1<-aot1[good]; aot2<-aot2[good]; aot3<-aot3[good]; aot4<-aot4[good]; aot5<-aot5[good]; aot6<-aot6[good]; aot7<-aot7[good]; aot8<-aot8[good]; aot9<-aot9[good]
hvf<-hvf[good]

# Transformations
gndr <- as.character(gndr); gndr[gndr=="Female"] <- 0; gndr[gndr=="Male"] <- 1; gndr <- as.double(gndr)
mean(gndr,na.rm=T) # 80.54% male

age <- as.character(age); age[age=="18-24 years"] <- 21; age[age=="25-32 years"] <- 28.5; age[age=="33-40 years"] <- 36.5; age[age=="41-49 years"] <- 45;
age[age=="50-59 years"] <- 54.5; age[age=="60-69 years"] <- 64.5; age[age=="70-79 years"] <- 74.5; age[age=="80 years or older"] <- 86; age <- as.double(age)

# Age graph
png("Age.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(las=1,mar=c(5,4,4,4),cex=1)
br <- c(18,25,33,41,50,60,70,80,91)
hist(age,breaks=br,freq=T,xlab="Age in Years",main="")
dev.off()

# Education
edu <- as.character(educ); edu[educ=="Not a high school graduate"] <- 0; edu[educ=="High school diploma or equivalency (for example, GED)"|educ=="High school diploma or equivalent (for example, GED)"] <- 2;
edu[educ=="Some post-high school education or certification"] <- 4; edu[educ=="Associate's degree (for example, AA, AS)"] <- 6;
edu[educ=="Bachelor's degree (for example, BA, AB, BS) "] <- 8; edu[educ=="Master's degree (for example, MS, MPH, MBA)"]<- 10;
edu[educ=="Professional degree or professional doctorate (for example, MD, DVM, JD or PsyD)"] <- 12; edu[educ=="Other doctorate degree (Phd, EdD)"|educ=="Other doctorate degree (for example, PhD, EdD)"] <- 14
edu <- as.double(edu)
br <- (0:15)-0.5
png("Edu.png", width = 3600, height = 3600, pointsize = 18, res = 360)
 par(las=1,mar=c(11,4,4,4),cex=0.75)
hist(edu,breaks=br,xlim=c(-0.5,14.5),xaxt="n",main="",xlab="")
 mtext("Highest Level of Education", outer=T,side=1,line=-1.5,font=1,cex=0.75,col=rgb(0,0,0))
 par(las=2)
axis(1,at=seq(0,14,1),lab=c("No Diploma","","High School Diploma","","Post-High-School","","Associate's Degree","","Bachelor's Degree","","Master's Degree","","Professional Degree","","Doctorate Degree"))
dev.off()

# Occupation graph
png("Ocu.png", width = 3600, height = 3600, pointsize = 18, res = 360)
ocup <- array(c(as.character(ocup1),as.character(ocup3)),c(length(ocup1),2)); ocup[ocup==""] <- NA;
par(las=2,mar=c(18,4,4,4),cex=0.8)
plot(as.factor(as.vector(ocup)))
dev.off()

# More transformations for Active Open-minded Thinking and Cognitive Reflection questions
babc<- bab; babc[babc!=5&is.na(babc)==F] <- 0; babc[babc==5&is.na(babc)==F] <- 1
mshc<- msh; mshc[mshc!=5&is.na(mshc)==F] <- 0; mshc[mshc==5&is.na(mshc)==F] <- 1
ptsc<- pts; ptsc[ptsc!=47&is.na(ptsc)==F] <- 0; ptsc[ptsc==47&is.na(ptsc)==F] <- 1
see <- as.character(see); see[see=="Yes"] <- 0; see[see=="No"] <- 1; see <- as.double(see)
nrsc<- nrs; nrsc[nrsc!=2&is.na(nrsc)==F] <- 0; nrsc[nrsc==2&is.na(nrsc)==F] <- 1
sasc<- sas; sasc[sasc!=2.25&is.na(sasc)==F] <- 0; sasc[sasc==2.25&is.na(sasc)==F] <- 1
teac<- tea; teac[teac!=5&is.na(teac)==F] <- 0; teac[teac==5&is.na(teac)==F] <- 1
crt1 <- babc+mshc+ptsc; crt2 <- nrsc+sasc+teac; crt <- crt1+crt2

aott <- aot1+aot2+aot3-aot4-aot5-aot6-aot7+aot8+aot9