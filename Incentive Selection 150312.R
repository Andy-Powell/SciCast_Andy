##########################################################
#
#   Reading in incentive questions
#
#########################################################
# source("Incentive Selection 150312.R")

start <- Sys.time()
print("Incentive Selection started")

#setAData <- read.csv("cat19questionA.csv")
#setBData <- read.csv("cat20questionA.csv")

#setAQstns <- sort(setAData$question_id)
#setBQstns <- sort(setBData$question_id)

expStart <- as.POSIXct("2013-11-25 EST")
expStop <- trunc(Sys.time(),units="days")
expFirst <- expStart

print("Use entire data set? (y/n)")                                            # "n" lets you bypass downloading files to save time
allData <- readline()
#allData <- "y"
if (allData=="n") {
  print("Enter quarter or start date (1, 2, 3, 4, or yyyy-mm-dd)")
  rawData <- readline()
  #expStart <- as>POSIXct(rawData)
  #expStart <- as.POSIXct(readline(),"%Y-%m-%d")
  #print("Enter stop date (yyyy-mm-dd)")
  #expStop <- as.POSIXct(readline())
  #expStop <- expStop + 60*60*24+1
#}  else {
#  expStart <- tstart
#  expStop <- tstop
#}

  if(rawData=="1") {
    expStart <- as.POSIXct("2014-11-07")
    expStop = as.POSIXct("2014-12-07")
    incentiveSet <- setAQstns
    controlSet <- setBQstns
  } else {
    if(rawData=="2") {
      print("1")
      expStart <- as.POSIXct("2014-12-07",usetz=TRUE)
      print("2")
      expStop = as.POSIXct("2015-01-07",usetz=TRUE)
      #incentiveSet <- setBQstns
      #controlSet <- setAQstns
      incentiveSet <- setAQstns
      controlSet <- setBQstns
    } else {
      if(rawData=="3") {
        expStart <- as.POSIXct("2015-01-07")
        expStop = as.POSIXct("2015-02-07")
        incentiveSet <- setAQstns
        controlSet <- setBQstns
      } else {
        if(rawData=="4") {
          expStart <- as.POSIXct("2015-02-07")
          expStop = as.POSIXct("2015-03-07")
          #incentiveSet <- setBQstns
          #controlSet <- setAQstns
          incentiveSet <- setAQstns
          controlSet <- setBQstnsQ4
        } else {
          expStart <- as.POSIXct(rawData)
          print ("Enter End Date (yyyy-mm-dd)")
          expStop <- as.POSIXct(readline())+24*60*60
          incentiveSet <- setAQstns
          controlSet <- setBQstnsQ4
        }
      }
    }
  }
}  else {
  expStart <- tstart
  expStop <- trunc(tstop+(20*60*60),"days")
  incentiveSet <- setAQstns
  controlSet <- setBQstns
}
  

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Incentive Selection Complete")
print(duration)