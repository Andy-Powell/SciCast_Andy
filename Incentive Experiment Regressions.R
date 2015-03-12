######################################################
##
## SciCast Brier Scores
## Imitation of Stratman's method for binary questions
## 
## Modified 2014-12-22 by kolson to fix a bug in carrying forward old estimates:
## Some had failed to carry forward; this seems to have been the main source of
## the discrepancy between our previous scores and Steve's.
##
#####################################################

# setwd("C:/Users/Walter/Documents/GMU/SciCast/Analysis")

print("Use Stored regression data? (y,n)")
if (readline()=="n") {
  source("Incentive Experiment Trade Brier Generation 150225A.R")
}

start <- Sys.time()


td<-read.csv("tradeData.csv")

#goodDataRegrsn <- complete.cases(tradeData$Set)
#tradeDataRegrsn <- tradeData[goodDataRegrsn==T]

resModel <- lm(td$Brier~td$timeToRes)
print(summary(resModel))
sink(file="Time to Resolution Summary.txt") 
summary(resModel) 
sink() 
dev.copy(png,"Time to Resolution Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(resModel)
dev.off()

tActModel <- lm(td$Brier~td$timeSinceActive)
print(summary(tActModel))
sink(file="Time Since Active Summary.txt") 
summary(tActModel) 
sink() 
dev.copy(png,"Time Since Active Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(tActModel)
dev.off()

actModel <- lm(td$Brier~td$active)
print(summary(actModel))
sink(file="Active Summary.txt") 
summary(actModel) 
sink() 
dev.copy(png,"Active Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(actModel)
dev.off()

setModel <- lm(td$Brier~td$set)
print(summary(setModel))
sink(file="Set Summary.txt") 
summary(setModel) 
sink() 
dev.copy(png,"Set Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(setModel)
dev.off()



model4 <- lm(td$Brier~td$active+td$set+td$timeToRes+td$timeSinceActive)
print(summary(model4))
sink(file="Multiple Regression Summary.txt") 
summary(model4) 
sink() 
dev.copy(png,"Multipt Regression Residuals.png")
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(model4)
dev.off()

#model.res = resid(model)
#plot(td$active, model.res, ylab="Residuals", xlab="Active", main="Active risiduals") 
#abline(0, 0)

duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print("Data Retrieval complete")
print(duration)