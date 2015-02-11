start <- Sys.time()
print("Analysis Setup started")

print("Use all data? (y/n)")                                            # "n" lets you bypass downloading files to save time
allData <- readline()


tstart <- as.POSIXct("2013-11-25 00:00:00 EST")
base <- tstart-28*24*60*60
tstop <- tstart+floor(Sys.time()-tstart)-1*60*60
days <- seq(1,ceiling(as.double(tstop - tstart)),1)

# Pulling question_report into variables
#qiq <- qn$question_id
#caq <- as.POSIXct(qn$created_at)
#grq <- as.character(qn$groups)
#saq <- as.character(qn$resolution_at)
#raq <- as.character(qn$pending_until)

#qps <- qn$provisional_settled_at
#pq <- qn$type
#ls <- qn$relationships_source_question_id
#ld <- qn$relationships_destination_question_id
#ql <- qn$is_locked
#qv <- qn$is_visible
#orq <- qn$is_ordered; orq <- as.double(orq)
#ctq <- qn$categories
#rvq <- qn$resolution_value_array

#qn$provisional_settled_at is start of comment period and qn$pending_until will be reused for event resolution (not question resolution/settlement)
saq[saq=="None"] <- as.character(Sys.time()+10*365*60*60*24); saq <- as.POSIXct(saq)  # if not resolved sets resolved_at to 10 years from today
raq[raq=="None"] <- as.character(Sys.time()+10*365*60*60*24); raq <- as.POSIXct(raq)  # if not resolved sets pending_unitl to today 10 years from today

tpq <- rep(0,length(qiq))          # creates vector of numeric values representing question type
tpq[pq=="binary"] <- 2             # old - uses type not clasification.  Does it handle ordered & Scalar?
tpq[pq=="multi"] <- 3              # old - uses type not clasification.  Does it handle ordered & Scalar?

drq <- as.double(raq-caq)          # length of time between qn_pending_utnil and qn_created_at
hist(drq)     								     # Range of questions' duration

# Removing forecasts that occurred after resolution was known.
for (t in 1:length(tat)) {
 if (tat[t]>raq[qiq==qit[t]]) {
  tat[t] <- NA
 }
}

good <- complete.cases(tat)
sum(!good)
tat<-tat[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; as<-as[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]


########## generates list of condional assumption questions (asqt) and options (asot), -1 indicates in either indicades non-conditional trade
nowish <- strsplit(as.character(tstop), ' ')[[1]][1]
asq <- aso <- rep("a",length(tat))
for (t in 1:length(tat)) {
  asq[t] <- strsplit(as.character(as[t]),':')[[1]][1]     # assumptin question as str
  aso[t] <- strsplit(as.character(as[t]),':')[[1]][2]     # assumtion option as str
}
asqt <- as.double(asq)                                    # assumptin question as double
asot <- as.double(aso)                                    # assumptin option as double
asqt[is.na(asqt)==T] <- -1                                # replaces NAs wiht -1
asot[is.na(asot)==T] <- -1

# Reordering to simplify other operations later.
ord <- order(qit,tat)
tat<-tat[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; as<-as[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]

##### Find resolved questions.
'%ni%' <- Negate('%in%')
gpq <- matrix(rep("a",length(qiq)*200),c(length(qiq),200)); vldq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
  tmp <- as.vector(strsplit(grq[q],',',fixed=T)[[1]]); lv <- length(tmp)
  if (lv>0) {  gpq[q,1:lv] <- tmp }
  if ("Invalid Questions"%ni%tmp) { vldq[q] <- 1 }
}
# How many are invalid?
length(vldq[vldq==0])

ctq <- qn$categories
#orq <- qn$is_ordered
#orq <- as.double(orq)
#rvq <- qn$resolution_value_array
svt <- rvqt <- rvqat<- array(rep(0,length(tat)*40),c(length(tat),40))
roqt <- roqat <-rep(-1,length(tat))
#rsq <- levels(factor(qiq[raq<=Sys.time()&caq>tstart]))

######## Cleaning up question groups - creating vectors of groups for each user
wtg <- rep(0,length(qiq))
gq <- array(numeric(), c(length(qiq),200))
for (j in 1:length(qiq)) {
  if(grq[j]!="") {
    temp <- levels(factor(strsplit(as.character(grq[j]),",")[[1]]))  # generating a vector of groups for each user  
    wtg[j]<- 1/length(temp)                                          # ???
    gq[j,1:(length(temp))] <- temp                                   # putting the vector into array gq                                
  }
}




duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Analysis setup Complete")
print(duration)