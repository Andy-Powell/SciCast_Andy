#############################################
#
#  This is a version of Get_Data that doesn't retreive data from external files
#  It is only intended to reset the mais progarm variables so that their status is know (not affected by previously called programs)
#  This requires that Get_Data has been called previously
#
##############################################

# setwd("C:/Users/Walter/Documents/GMU/SciCast/Analysis")

start <- Sys.time()
print("Data Retrieval started")

th<-read.csv("trade_history_report.csv")                         # reads in data from files - original files are not ever altered
qh<-read.csv("question_history_report.csv")
qn<-read.csv("question_report.csv")
cm<-read.csv("comment_report.csv")
pr<-read.csv("person_report.csv")
lb<-read.csv("leaderboard_report.csv")
act<-read.csv("tradesPerDay 150207.csv")

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
print(c("0", length(ast)))
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
saq <- as.character(qn$resolution_at); saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)
raq <- as.character(qn$pending_until); raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)
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

# Activity Data
actQestnId <- act$quest_id
actTradedAt <- act$created_at
actnumTrades <- act$trade_id


### Derived Data  ###
# trade_history_report







duration <- as.double(difftime(Sys.time(),start,units="sec"))   #reports time to retrieve files
print ("Data Retrieval Complete")
print(duration)

