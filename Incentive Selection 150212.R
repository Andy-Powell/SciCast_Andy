##########################################################
#
#   Reading in incentive questions
#
#########################################################

start <- Sys.time()
print("Incentive Selection started")

#setAData <- read.csv("cat19questionA.csv")
#setBData <- read.csv("cat20questionA.csv")

#setAQstns <- sort(setAData$question_id)
#setBQstns <- sort(setBData$question_id)

expStart <- as.POSIXct("2013-11-25 EST")
expStop <- Sys.time()
expFirst <- expStart

print("Use entire data set? (y/n)")                                            # "n" lets you bypass downloading files to save time
allData <- readline()
#allData <- "y"
if (allData=="n") {
  print("Enter start date (yyyy-mm-dd)")
  expStart <- as.POSIXct(readline())
  #print("Enter stop date (yyyy-mm-dd)")
  #expStop <- as.POSIXct(readline())
  #expStop <- expStop + 60*60*24+1
#}  else {
#  expStart <- tstart
#  expStop <- tstop
#}

  if(expStart=="2014-11-07") {
    expStop = as.POSIXct("2014-12-07")
    incentiveSet <- setAQstns
    controlSet <- setBQstns
  } else {
    if(expStart=="2014-12-07") {
      expStop = as.POSIXct("2015-01-07")
      #incentiveSet <- setBQstns
      #controlSet <- setAQstns
      incentiveSet <- setAQstns
      controlSet <- setBQstns
    } else {
      if(expStart=="2015-01-07") {
        expStop = as.POSIXct("2015-02-07")
        incentiveSet <- setAQstns
        controlSet <- setBQstns
      } else {
        if(expStart=="2015-02-07") {
          expStop = as.POSIXct("2015-03-07")
          #incentiveSet <- setBQstns
          #controlSet <- setAQstns
          incentiveSet <- setAQstns
          controlSet <- setBQstns
        } else {
          print ("Enter End Date (yyyy-mm-dd)")
          expStop <- as.POSIXct(readline())+24*60*60
          incentiveSet <- setAQstns
          controlSet <- setBQstns
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