######################################################
##
## REgerssions on Brier score and change in Brire score
## Imitation of Stratman's method for binary questions
## 
##
#####################################################

# setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
# source("Incentive Experiment Regressions 150325A.R")

print("Use Stored regression data? (y,n)")
if (readline()=="n") {
  source("Incentive Experiment Trade Brier Generation 150225A.R")
}

start <- Sys.time()



trd <- read.csv("Trade Regression Data (clean).csv")
trdNQ34 <- read.csv("Trade Regression Data (clean) not Q3 or Q4.csv")
trdQ12 <- read.csv("Trade Regression Data (clean) wo 149 Q1 & Q2.csv")
trdQ12A <- read.csv("Trade Regression Data (clean) wo 149 Q1 & Q2A.csv")
trdQ1 <- read.csv("Trade Regression Data (clean) only Q1.csv")
trdQ2 <- read.csv("Trade Regression Data (clean) only Q2.csv")
trdL10 <- read.csv("Trade Regression Data (clean) less 10+.csv")
trdQ12I <- read.csv("Trade Regression Data (clean) wo 149 Q1 & Q2A Incentive.csv")
actQ1 <- read.csv("Q1_trade_counts with set.csv")
actQ2 <- read.csv("Q2_trade_counts with set.csv")
actQ12 <- read.csv("Q1_Q2_trade_counts with setA.csv")
actAllQ <- read.csv("tradesPerQ1-4A.csv")

#modelNew <- lm(trd$brierDiff~trd$inExp+trd$set+trd$timeToRes+trd$timeSinceActive+trd$active)
#trd <- within(trd, set <- relevel(set, ref = "A"))
#factor(trd$set, levels = c("A","B"))
modelNew <- lm(trdQ12A$brierDiff~trdQ12A$setA+trdQ12A$setB)
#modelNew <- lm(trdQ12A$brierDiff~trdQ12A$setN)
print(summary(modelNew))

modelTrd <- lm(trd$Brier~trd$set)
print(summary(modelTrd))

modelActQ1 <- lm(actQ1$quarter1_trade_counts~actQ1$setA+actQ1$setB)
print(summary(modelActQ1))

modelActQ2 <- lm(actQ2$quarter2_trade_counts~actQ2$setA+actQ2$setB)
print(summary(modelActQ12))

modelActQ12 <- lm(actQ12$Sum~actQ12$setA+actQ12$setB)
print(summary(modelActQ2))

sink(file="Multiple Regression Summary.txt") 
summary(modelNew) 
sink() 
dev.copy(png,"Multi Regression Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(modelNew)
dev.off()

modelinExp <- lm(trd$brierDiff~trd$set)
print(summary(modelinExp))

modelAllQ <- glm(actAllQ$trade_count ~ actAllQ$setA + actAllQ$setB + actAllQ$active, family="poisson")
print(summary(modelAllQ))


#td<-read.csv("tradeData.csv")
#goodDataRegrsn <- complete.cases(tradeData$Set)
#tradeDataRegrsn <- tradeData[goodDataRegrsn==T]

ModelQ12A <- lm(trdQ12A$Brier~trdQ12A$inExp)
print(summary(ModelQ12A))

ModelQ12A <- lm(trdQ12A$Brier~trdQ12A$setA*trdQ12A$setB)
print(summary(ModelQ12A))

ModelQ12I <- lm(trdQ12I$Brier~trdQ12I$set)
print(summary(ModelQ12I))

ModelAll <- lm(trd$Brier~trd$set*trd$active)
print(summary(resModel))

sink(file="Time to Resolution Summary.txt") 
summary(resModel) 
sink() 
dev.copy(png,"Time to Resolution Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(resModel)
dev.off()

#tActModel <- lm(td$Brier~td$timeSinceActive)
#print(summary(tActModel))
#sink(file="Time Since Active Summary.txt") 
#summary(tActModel) 
#sink() 
#dev.copy(png,"Time Since Active Residuals.png")
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(tActModel)
#dev.off()

#actModel <- lm(td$Brier~td$active)
#print(summary(actModel))
#sink(file="Active Summary.txt") 
#summary(actModel) 
#sink() 
#dev.copy(png,"Active Residuals.png")
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(actModel)
#dev.off()

#setModel <- lm(td$Brier~td$set)
#print(summary(setModel))
#sink(file="Set Summary.txt") 
#summary(setModel) 
#sink() 
#dev.copy(png,"Set Residuals.png")
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(setModel)
#dev.off()



#model4 <- lm(td$Brier~td$active+td$set+td$timeToRes+td$timeSinceActive)
#print(summary(model4))
#sink(file="Multiple Regression Summary.txt") 
#summary(model4) 
#sink() 
#dev.copy(png,"Multipt Regression Residuals.png")
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(model4)
#dev.off()

#model.res = resid(model)
#plot(td$active, model.res, ylab="Residuals", xlab="Active", main="Active risiduals") 
#abline(0, 0)

duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print("Data Retrieval complete")
print(duration)