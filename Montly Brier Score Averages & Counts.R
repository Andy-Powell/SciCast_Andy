###################################################
#
#  Counts and brier Score totals from Montly Breier Score Data
#
###################################################

#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#source("Montly Brier Score Averages & Counts.R")

monthlyData<-read.csv("Monthly Regression Data.csv")

tStart <- as.POSIXct("2013-11-25")
expFirst <- as.POSIXct("2014-11-07")
expChange1 <- as.POSIXct("2014-12-07")
expChange2 <- as.POSIXct("2015-01-07")
expChange3 <- as.POSIXct("2015-02-07")
expStop <- as.POSIXct("2015-03-07")
tStop <- Sys.time()