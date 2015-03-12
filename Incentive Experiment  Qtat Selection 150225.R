################################################
#
# Adds Set and Quarter to TradeDtata
#
################################################

source("Incentive Overall Trade Selection 150212")


rsqExpA <- rsq[which(rsq%in%incentiveSet)]
rsqExpB <- rsq[which(rsq%in%controlSet)]



tatActA <- tatAAct[qit%in%rsqExpA]
titActA <- titAAct[qit%in%rsqExpA]
pitActA <- pitAAct[qit%in%rsqExpA]
qitActA <- qitAAct[qit%in%rsqExpA]
nvtActA <- nvtAAct[qit%in%rsqExpA]
ovtActA <- ovtAAct[qit%in%rsqExpA]
astActA <- astAAct[qit%in%rsqExpA]
apotActA <- apotAAct[qit%in%rsqExpA]
citActA <- citAAct[qit%in%rsqExpA]
rstActA <- rstAAct[qit%in%rsqExpA]
mdtActA <- mdtAAct[qit%in%rsqExpA]
asqtActA <- asqtAAct[qit%in%rsqExpA]
asotActA <- asotAAct[qit%in%rsqExpA]

statusActA <- rep("Act",length(tatActA))
setActA <- rep("A", length(tatActA))

qActA <- rep(1,length(tatActA))      # default is first quarter
qActA[tatActA>= expChange2 ] <- 3    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qActA[tatActA>= expChange1 & tatActA<expChange2] <- 2   
qActA[tatActA>= expChange3] <- 4

accSetAAct <- acquAct
perSetAAct <- pocosAct
hitSetAAct <- hitAct

########################## Needs to be another instance of "Incentive Accuracy Active Overall 141209.R"  ## &qit%in%incentiveSet => in set A
## Accuracy of Set A non-active trades
#Con trades are Dec,Feb trades
tat<-tatACon; tit<-titACon; pit<-pitACon; qit<-qitACon; nvt<-nvtACon; ovt<-ovtACon; ast<-astACon
apot<-apotACon; cit<-citACon; rst<-rstACon; mdt<-mdtACon; asqt<-asqtACon; asot<-asotACon

source("Incentive Accuracy Active Overall 141209.R")

tatConA <- tatACon[qit%in%rsqExpA]
titConA <- titACon[qit%in%rsqExpA]
pitConA <- pitACon[qit%in%rsqExpA]
qitConA <- qitACon[qit%in%rsqExpA]
nvtConA <- nvtACon[qit%in%rsqExpA]
ovtConA <- ovtACon[qit%in%rsqExpA]
astConA <- astACon[qit%in%rsqExpA]
apotConA <- apotACon[qit%in%rsqExpA]
citConA <- citACon[qit%in%rsqExpA]
rstConA <- rstACon[qit%in%rsqExpA]
mdtConA <- mdtACon[qit%in%rsqExpA]
asqtConA <- asqtACon[qit%in%rsqExpA]
asotConA <- asotACon[qit%in%rsqExpA]

statusConA <- rep("Con",length(tatConA))
setConA <- rep("A", length(tatConA))

qConA <- rep(2,length(tatConA))      # default is first quarter
qConA[tatConA>=expChange3 ] <- 4    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qConA[tatConA>=expChange2 & tatConA<expChange3] <- 3   
qConA[tatConA<expChange1] <- 1

accSetACon <- acquAct
perSetACon <- pocosAct
hitSetACon <- hitAct

###### Accruacy of Set B trades #####
## Accuracy of Set B active trades
# ACon trades = BAct trades -- Dec,Feb trades
tat<-tatACon; tit<-titACon; pit<-pitACon; qit<-qitACon; nvt<-nvtACon; ovt<-ovtACon; ast<-astACon
apot<-apotACon; cit<-citACon; rst<-rstACon; mdt<-mdtACon; asqt<-asqtACon; asot<-asotACon

source("Incentive Accuracy Control Overall 141209.R")

rsqCon <- rsqExpB

tatActB <- tatACon[qit%in%rsqExpB]
titActB <- titACon[qit%in%rsqExpB]
pitActB <- pitACon[qit%in%rsqExpB]
qitActB <- qitACon[qit%in%rsqExpB]
nvtActB <- nvtACon[qit%in%rsqExpB]
ovtActB <- ovtACon[qit%in%rsqExpB]
astActB <- astACon[qit%in%rsqExpB]
apotActB <- apotACon[qit%in%rsqExpB]
citActB <- citACon[qit%in%rsqExpB]
rstActB <- rstACon[qit%in%rsqExpB]
mdtActB <- mdtACon[qit%in%rsqExpB]
asqtActB <- asqt[qit%in%rsqExpB]
asotActB <- asot[qit%in%rsqExpB]

statusActB <- rep("Act",length(tatActB))
setActB <- rep("B", length(tatActB))

qActB <- rep(2,length(tatActB))      # default is first quarter
qActB[tatActB>=expChange3 ] <- 4    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qActB[tatActB>=expChange2 & tatActB<expChange3] <- 3   
qActB[tatActB<expChange1] <- 1

accSetBAct <- acquCon
perSetBAct <- pocosCon
hitSetBAct <- hitCon

## Accuracy of Set B non-active trades
#AAct trades = BCon trades -- Nov,Jan trades
tat<-tatAAct; tit<-titAAct; pit<-pitAAct; qit<-qitAAct; nvt<-nvtAAct; ovt<-ovtAAct; ast<-astAAct
apot<-apotAAct; cit<-citAAct; rst<-rstAAct; mdt<-mdtAAct; asqt<-asqtAAct; asot<-asotAAct

source("Incentive Accuracy Control Overall 141209.R")

tatConB <- tatAAct[qit%in%rsqExpB]
titConB <- titAAct[qit%in%rsqExpB]
pitConB <- pitAAct[qit%in%rsqExpB]
qitConB <- qitAAct[qit%in%rsqExpB]
nvtConB <- nvtAAct[qit%in%rsqExpB]
ovtConB <- ovtAAct[qit%in%rsqExpB]
astConB <- astAAct[qit%in%rsqExpB]
apotConB <- apotAAct[qit%in%rsqExpB]
citConB <- citAAct[qit%in%rsqExpB]
rstConB <- rstAAct[qit%in%rsqExpB]
mdtConB <- mdtAAct[qit%in%rsqExpB]
asqtConB <- asqtAAct[qit%in%rsqExpB]
asotConB <- asotAAct[qit%in%rsqExpB]

statusConB <- rep("Con",length(tatConB))
setConB <- rep("B", length(tatConB))

qConB <- rep(1,length(tatConB))      # default is first quarter
qConB[tatConB>= expChange2 ] <- 3    # setting quarter to 3 for third quater of experiment (07 NOV -06 DEC)
qConB[tatConB>= expChange1 & tatConB<expChange2] <- 2   
qConB[tatConB>= expChange3] <- 4



write.table (data.frame(setActA,statusActA,qActA,tatActA,titActA,pitActA,qitActA,nvtActA,ovtActA,astActA,apotActA,citActA,rstActA,mdtActA,asqtActA,asotActA),file="Incentive Exp Set A Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setConA,statusConA,qConA,tatConA,titConA,pitConA,qitConA,nvtConA,ovtConA,astConA,apotConA,citConA,rstConA,mdtConA,asqtConA,asotConA),file="Incentive Exp Set A Non-Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setActB,statusActB,qActB,tatActB,titActB,pitActB,qitActB,nvtActB,ovtActB,astActB,apotActB,citActB,rstActB,mdtActB,asqtActB,asotActB),file="Incentive Exp Set B Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setConB,statusConB,qConB,tatConB,titConB,pitConB,qitConB,nvtConB,ovtConB,astConB,apotConB,citConB,rstConB,mdtConB,asqtConB,asotConB),file="Incentive Exp Set B Non-Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)

tatAct <- c(tatActA,tatActB)
titAct <- c(titActA,titActB)
pitAct <- c(pitActA,pitActB)
qitAct <- c(qitActA,qitActB)
nvtAct <- c(nvtActA,nvtActB)
ovtAct <- c(ovtActA,ovtActB)
astAct <- c(astActA,astActB)
apotAct <- c(apotActA,apotActB)
citAct <- c(citActA,citActB)
rstAct <- c(rstActA,rstActB)
mdtAct <- c(mdtActA,mdtActB)
asqtAct <- c(asqtActA,asqtActB)
asotAct <- c(asotActA,asotActB)

setAct <- c(setActA,setActB)
statusAct <- c(statusActA,statusActB)
qAct <- c(qActA,qActB)

########
tatCon <- c(tatConA,tatConB)
titCon <- c(titConA,titConB)
pitCon <- c(pitConA,pitConB)
qitCon <- c(qitConA,qitConB)
nvtCon <- c(nvtConA,nvtConB)
ovtCon <- c(ovtConA,ovtConB)
astCon <- c(astConA,astConB)
apotCon <- c(apotConA,apotConB)
citCon <- c(citConA,citConB)
rstCon <- c(rstConA,rstConB)
mdtCon <- c(mdtConA,mdtConB)
asqtCon <- c(asqtConA,asqtConB)
asotCon <- c(asotConA,asotConB)

setCon <- c(setConA,setConB)
statusCon <- c(statusConA,statusConB)
qCon <- c(qConA,qConB)

write.table (data.frame(setAct,statusAct,qAct,tatAct,titAct,pitAct,qitAct,nvtAct,ovtAct,astAct,apotAct,citAct,rstAct,mdtAct,asqtAct,asotAct),file="Incentive Exp Active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)
write.table (data.frame(setCon,statusCon,qCon,tatCon,titCon,pitCon,qitCon,nvtCon,ovtCon,astCon,apotCon,citCon,rstCon,mdtCon,asqtCon,asotCon),file="Incentive Exp non_active.csv",sep=",",append=F,
             col.names=c("Set","Status","Quarter","traded_at","trade_id","user_id","question_id","new_value","old_value","serialized_assumptions","assets_per_option","choice_index","forecast","binary","assumed_question","assumed_option"),
             row.names=F)