
start <- Sys.time()

####### Removing user ids from person_report for admin accounts and specifically excluded users
# Match to Steve's list!

# Pulling bits of person_report into variables

adi <- numeric()
grps <- pr$groups
rip <-pr$referral_id

adu <- c("amsiegel","BAE11","brnlsl","brobins","cedarskye","christinafreyman","ctwardy",
         "daggre_admin","dquere","gbs_tester","Inkling","jessiejury","jlu_bae","kennyth0",
         "klaskey","kmieke","manindune","Naveen Jay","pthomas524","Question_Admin",
         "Question Mark","randazzese","RobinHanson","saqibtq","scicast_admin","slin8",
         "ssmith","tlevitt","wsun")

for (i in 1:length(adu)) {
  adi[i] <- pip[pus==adu[i]]                                    # List of pr_user_ids of specifically excluded users
  cap[pip==adi[i]] <- NA                                        # List of pr_created_at with NAs for specifically  exlcuded users
}

grp <- array(rep("a",length(pip)*20),c(length(pip),20)); igrp <- rep(0,length(pip))
for (i in 1:length(pip)) {
  temp <- as.vector(strsplit(as.character(grps[i]),",")[[1]])   # vector of groups associated with a pr_user_id
  grp[i,1:length(temp)] <- temp                                 # inputs vector into grp array
}

# Cleaning up question groups - creating vectors of groups for each user
wtg <- rep(0,length(qiq))
gq <- array(numeric(), c(length(qiq),200))
for (j in 1:length(qiq)) {
  if(grq[j]!="") {
    temp <- levels(factor(strsplit(as.character(grq[j]),",")[[1]]))  # generating a vector of groups for each user  
    wtg[j]<- 1/length(temp)                                          # ???
    gq[j,1:(length(temp))] <- temp                                   # putting the vector into array gq                                
  }
}


adi <- numeric()
for (i in 1:length(pip)) {
  for (g in 1:20) {
    if (grp[i,g]=="Admin"|grp[i,g]=="SuperAdmin"|grp[i,g]=="UserAdmin"|grp[i,g]=="BadgesAdmin"|grp[i,g]=="RolesAdmin"|grp[i,g]=="QuestionAdmin") {
      cap[i] <- NA                                             # replaces pr_created_at wiht NA for admin users
      adi <- c(adi,pip[i])                                     # list of admin users (not inlcuding specifically excluded users)
    }
    if (grp[i,g]=="Internal") {    								             # Keeping but noting internal accounts!
      igrp[i] <- 1
    }
  }
}
adi <- unique(adi)

good <- complete.cases(cap)
sum(!good)     												# How many are not good?
cap<-cap[good]; pus<-pus[good]; pip<-pip[good]; grp<-grp[good,]; rip<-rip[good]

good1 <- complete.cases(igrp)
pip1 <- pip[good1]                                            # List of pr_user_ids of internal accounts

# Pulling bits of trade history into variables

nvt <- th$new_value_list
ovt <- th$old_value_list
as <- th$serialized_assumptions
apot <- th$assets_per_option
tit <- th$trade_id
cit <- th$choice_index
rust <- as.character(th$raw_user_selection)

# There were a few irreparable errors in the data mart that are handled case by case here.
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\",[0.9545454545454546,1],null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\",[0.9545454545454546,1],null]"
rust[rust=="[\"\\\"Will Not occur by December 31, 2034\\\"\", [0.9545454545454546, 1], null]"] <- "[\"\\\"Will Not occur by December 31 2034\\\"\", [0.9545454545454546,1], null]" 
rust[rust=="[\"\\\"Less than 4.75%\\\"\",[-0.33333333333333326,-0.33333333333333326],null]"] <- "None"
rust[rust=="[\"\\\"Between 4.75% and 5%\\\"\",[-0.33333333333333326,0.33333333333333326],\"Lower\"]"] <- "None"
rust[rust=="[\"\\\"Less than 4.75%\\\"\", [-0.33333333333333326, -0.33333333333333326], null]"] <- "None"
rust[rust=="[\"Down ~12% or more\",[-0.21428571428571427,0.5],null]"] <- "None"
rust[rust=="[\"Up ~12% or more\",[0.842857142857143,1.2142857142857142],null]"] <- "None"

#####  Ken's Creating response and response mode variables
rst <- mdt <- rep(0,length(rust))
#for (t in 1:length(rust)) {
# m <- strsplit(rust[t],",")[[1]][1]
# if (m!="None") {                                                   # "none" is indicative of a safe mode forcast
#  mdt[t] = 1												                                # 1 indicates safe-mode forecast.
#  tmp1 <- as.double(strsplit(as.vector(ovt[t]),",")[[1]])						# <string> <string> A later selection on SciCast.org like "Higher" will assume the user wants the forecast halfway between the current market estimate and the top of the bin.
#  tmp2 <- strsplit(strsplit(rust[t],',')[[1]][4],'"',fixed=T)[[1]][2]
#  if (is.na(tmp2)==T) { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
#  if (is.na(tmp2)==F) { 
#   if (tmp2=="Lower" ) { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(strsplit(rust[t],',')[[1]][2],',')[[1]][1],'[',fixed=T)[[1]][2]) -tmp1[cit[t]+1])/2 }
#   if (tmp2=="Higher") { rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)) -tmp1[cit[t]+1])/2 }
#   if (tmp2=="What they are now") { rst[t] <- tmp1[cit[t]+1]}
#   if (tmp2=="null") { rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] ))) }
#  }
# }
#}

##### Modified Creating response (rst) and response "safe" mode (mdt) variables

for (t in 1:length(rust)) {
  m <- strsplit(rust[t],",")[[1]][1]
  if (m=="None") next 
  
  mdt[t] = 1
  tmp1 <- as.double(strsplit(as.vector(ovt[t]),",")[[1]])
  tmp2 <- strsplit(strsplit(rust[t],',')[[1]][4],'"',fixed=T)[[1]][2]
  if (is.na(tmp2)==T) {
    rst[t] <- mean(as.double(c(strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] )))
  } else {
    if (tmp2=="Lower" ) {
      rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(strsplit(rust[t],',')[[1]][2],',')[[1]][1],'[',fixed=T)[[1]][2]) -tmp1[cit[t]+1])/2
    } else if (tmp2=="Higher") {
      rst[t] <- tmp1[cit[t]+1] + (as.double(strsplit(strsplit(rust[t],',')[[1]][3],']',fixed=T)) -tmp1[cit[t]+1])/2
    } else if (tmp2=="What they are now") {
      rst[t] <- tmp1[cit[t]+1]
    } else if (tmp2=="null") {
      rst[t] <- mean(as.double(c( strsplit(strsplit(rust[t],",")[[1]][2],"[",fixed=T)[[1]][2], strsplit(strsplit(rust[t],",")[[1]][3],"]",fixed=T)[[1]][1] )))
    }
  }
}
# list(rst=rst, mdt=mdt)

#
###### Removing admin & excluded user activity from trade_history

for (i in 1:length(adi)) {
 tat[pit==adi[i]] <- NA               # NAs for admin and exlcuded user ids
}
good <- complete.cases(tat)
sum(!good)     												# How many trades are not good?

tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]; tit<-tit[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]

####### Removing admin & excluded user activity from comment_report


for (i in 1:length(adi)) {
 cac[pic==adi[i]] <- NA               # replaces cm_created_at with NAs for admin & specifically ecsluded users
}
good <- complete.cases(cac)
sum(!good)     												# How many comments are not good?
cac<-cac[good]; pic<-pic[good]; qic<-qic[good]

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Data cleaned")
print(duration)
