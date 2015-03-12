####################################################
#
#  Selecting trades for each questoin set for active and non-active periods
#
####################################################
start <- Sys.time() 
print("Trade Selection started")

### sets dates of question exchanges
expStart <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expStop <- as.POSIXct("2015-03-07")

### Generating sets of trades for each condition of Set A questions by removing non-applicable trades
### Creating dummy tat variables for experimenatl conditions
tatExp <- tat

## Removing trades before and after experiment
tatExp[tatExp<expStart] <- NA           # NAs inplace of traded_at for trades before experiment start
tatExp[tatExp>=expStop] <- NA           # NAs inplace of traded_at for trades after experiment end

good <- complete.cases(tatExp)          # remove all trades not made during experiment
sum(!good)
tatExp<-tatExp[good]; titExp<-tit[good]; pitExp<-pit[good]; qitExp<-qit[good]; nvtExp<-nvt[good]; ovtExp<-ovt[good]; astExp<-ast[good]; apotExp<-apot[good]
citExp<-cit[good]; rstExp<-rst[good]; mdtExp<-mdt[good]; asqtExp<-asqt[good]; asotExp<-asot[good]

#######################
## Creating dummy variables for the active and control conditions for set A
tatAAct <- tatExp
tatACon <- tatExp
print(c(length(tatAAct),length(tatACon)))

## Removing trades when Set A not active
tatAAct[tatAAct>=expChange1 &tatAAct<expChange2] <- NA    # Removing DEC trades
tatAAct[tatAAct>=expChange3] <- NA                        # removing FEB trades
goodAct <- complete.cases(tatAAct)                           # remove all set A trades while set B active => trades when set A active
sum(!goodAct)
tatAAct<-tatExp[goodAct]; titAAct<-titExp[goodAct]; pitAAct<-pitExp[goodAct]; qitAAct<-qitExp[goodAct]; nvtAAct<-nvtExp[goodAct]; ovtAAct<-ovtExp[goodAct]; astAAct<-astExp[goodAct]
apotAAct<-apotExp[goodAct]; citAAct<-citExp[goodAct]; rstAAct<-rstExp[goodAct]; mdtAAct<-mdtExp[goodAct]; asqtAAct<-asqtExp[goodAct]; asotAAct<-asotExp[goodAct]

print(length(tatAAct))

## Removing trades when Set A active
tatACon[tatACon<expChange1] <-NA                          # Removing Nov trades
tatACon[tatACon>=expChange2 &tatACon<expChange3] <-NA     # Removing Jan Trades
goodCon <- complete.cases(tatACon)                           # remove all set A trades while set A active => trades when set A non-active
sum(!goodCon)

tatACon<-tatExp[goodCon]; titACon<-titExp[goodCon]; pitACon<-pitExp[goodCon]; qitACon<-qitExp[goodCon]; nvtACon<-nvtExp[goodCon]; ovtACon<-ovtExp[goodCon]; astACon<-astExp[goodCon]
apotACon<-apotExp[goodCon]; citACon<-citExp[goodCon]; rstACon<-rstExp[goodCon]; mdtACon<-mdtExp[goodCon]; asqtACon<-asqtExp[goodCon]; asotACon<-asotExp[goodCon]

print(length(tatACon))

duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print ("Trade Selection Complete")
print(duration)