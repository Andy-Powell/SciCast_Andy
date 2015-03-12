##
# setwd("C:/Users/Walter/Documents/GMU/SciCast/Analysis")

thFileInfo <- file.info("trade_history_report.csv")
qrFileInfo <- file.info("question_report.csv")
prFileInfo <- file.info("Person_report.csv")
qhFileInfo <- file.info("Question_history_report.csv")
crFileInfo <- file.info("Comment_report.csv")

CurrentDay <- as.POSIXlt(Sys.Date())$yday
CurrentYear <- as.POSIXlt(Sys.Date())$year
#CurrentDate <- 365*CurrentYear+CurrentDay

thFileCreatdAt <- thFileInfo[1,4]
thFileAccessedAt <- thFileInfo[1,6]
thFileCreatdDay <- as.POSIXlt(thFileCreatdAt)$yday
thFileCreatdYear <- as.POSIXlt(thFileCreatdAt)$year
#thFileCreatdDate <- 365*(CurrentYear-thFileCreatdYear) + thFileCreatdDay
#thFileAccessedDate <- as.POSIXlt(AccessedAt)$yday

qrFileCreatdAt <- qrFileInfo[1,4]
qrFileAccessedAt <- qrFileInfo[1,6]
qrFileCreatdDay <- as.POSIXlt(qrFileCreatdAt)$yday
qrFileCreatdYear <- as.POSIXlt(qrFileCreatdAt)$year
#qrFileAccessedDate <- as.POSIXlt(AccessedAt)$yday

prFileCreatdAt <- prFileInfo[1,4]
prFileAccessedAt <- prFileInfo[1,6]
prFileCreatdDay <- as.POSIXlt(prFileCreatdAt)$yday
prFileCreatdYear <- as.POSIXlt(prFileCreatdAt)$year
#prFileAccessedDate <- as.POSIXlt(AccessedAt)$yday

qhFileCreatdAt <- qhFileInfo[1,4]
qhFileAccessedAt <- qhFileInfo[1,6]
qhFileCreatdDay <- as.POSIXlt(qhFileCreatdAt)$yday
qhFileCreatdYear <- as.POSIXlt(qhFileCreatdAt)$year
#qhFileAccessedDate <- as.POSIXlt(AccessedAt)$yday

crFileCreatdAt <- crFileInfo[1,4]
crFileAccessedAt <- crFileInfo[1,6]
crFileCreatdDay <- as.POSIXlt(crFileCreatdAt)$yday
crFileCreatdYear <- as.POSIXlt(crFileCreatdAt)$year
#crFileAccessedDate <- as.POSIXlt(AccessedAt)$yday



OldData <- "n"

if(thFileCreatdDay < (CurrentYear-thFileCreatdYear)*366+CurrentDay             # checks to see if any of the data files have not been downloaded today
   & qrFileCreatdDay < (CurrentYear-qrFileCreatdYear)*366+CurrentDay           # (CurrentYear-**FileCreatdYear)*366+CurrentDay  accounts for change of year      
   & prFileCreatdDay < (CurrentYear-prFileCreatdYear)*366+CurrentDay 
   & qhFileCreatdDay < (CurrentYear-qhFileCreatdYear)*366+CurrentDay 
   & crFileCreatdDay < (CurrentYear-crFileCreatdYear)*366+CurrentDay) {
  print("Use stored file(s)? (y/n)")                                            # "n" lets you bypass downloading files to save time
  OldData <- readline()
}

apikey <- "0fa62f0d8ebd4878c28a051301b3e67d"

start <- Sys.time()                                                             # Establshes base time for duration calcuations

if (OldData=="n") {
  if (thFileCreatdDay < (CurrentYear-thFileCreatdYear)*366 +CurrentDay) {       # only downloads files that are not current
    url <- paste("http://scicast.org:8200/trade_history/?format=csv&api_key=",apikey,sep="")
    download.file(url,destfile="trade_history_report.csv")
  }
  if (qrFileCreatdDay < (CurrentYear-qrFileCreatdYear)*366 +CurrentDay) {
    url <- paste("http://scicast.org:8200/question/?format=csv&api_key=",apikey,sep="")
    download.file(url,destfile="question_report.csv")
  }
  if (prFileCreatdDay < (CurrentYear-prFileCreatdYear)*366 +CurrentDay ) {
    url <- paste("http://scicast.org:8200/person/?format=csv&api_key=",apikey,sep="")
    download.file(url,destfile="person_report.csv")
  }
  if (qhFileCreatdDay < (CurrentYear-qhFileCreatdYear)*366 +CurrentDay ) {
    url <- paste("http://scicast.org:8200/question_history/?format=csv&api_key=",apikey,sep="")
    download.file(url,destfile="question_history_report.csv")
  }
  if (crFileCreatdDay < (CurrentYear-crFileCreatdYear)*366 +CurrentDay) {
    url <- paste("http://scicast.org:8200/comment/?format=csv&api_key=",apikey,sep="")
    download.file(url,destfile="comment_report.csv")
  }
}



th<-read.csv("trade_history_report.csv")                         # reads in data from files - original files are not ever altered
qh<-read.csv("question_history_report.csv")
qn<-read.csv("question_report.csv")
cm<-read.csv("comment_report.csv")
pr<-read.csv("person_report.csv")
lb<-read.csv("leaderboard_report.csv")

##   Reading in incentive questions
setAData <- read.csv("cat19questionB.csv")
setBData <- read.csv("cat20questionB.csv")



#### Getting basic data
# trade_history_report
pit <- th$user_id
qit <- th$question_id
tat <- th$traded_at; tat <- as.Date(tat); tat <- as.POSIXct(th$traded_at)
nvt <- th$new_value_list
ovt <- th$old_value_list
ast <- th$serialized_assumptions
apot <- th$assets_per_option
tit <- th$trade_id
cit <- th$choice_index
thInterface <- th$interface_type
rust <- as.character(th$raw_user_selection)

# Question_history_report


# Question_Report
qiq <- qn$question_id
caq <- as.POSIXct(qn$created_at)
grq <- as.character(qn$groups)
saq <- as.character(qn$resolution_at)
raq <- as.character(qn$pending_until)
qps <- qn$provisional_settled_at
pq <- qn$type
ls <- qn$relationships_source_question_id
ld <- qn$relationships_destination_question_id
ql <- qn$is_locked
qv <- qn$is_visible
orq <- qn$is_ordered; orq <- as.double(orq)
ctq <- qn$categories
rvq <- qn$resolution_value_array
clq <- qn$classification

# Comment_report
pic <- cm$user_id
qic <- cm$question_id
cac <- as.POSIXct(cm$created_at)

# Person_report
pip <- pr$user_id
pus <- as.character(pr$username)
cap <- as.POSIXct(pr$created_at)

# Incentinve questions
setAQstns <- sort(as.character(setAData$question_id))
setBQstns <- sort(as.character(setBData$question_id))

### Derived Data  ###
# trade_history_report







duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print ("Data Retrieval Complete")
print(duration)

