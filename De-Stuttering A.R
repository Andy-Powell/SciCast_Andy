
# Removing stuttered forecasts ("de-stuttering")
#


start <- Sys.time()
print("De-stuttering Started")

# Sort trade variables by question and time
ord <- order(qit,tat)
tat<-tat[ord]; tit<-tit[ord]; pit<-pit[ord]; qit<-qit[ord]; nvt<-nvt[ord]; ovt<-ovt[ord]; ast<-ast[ord]; apot<-apot[ord]
cit<-cit[ord]; rst<-rst[ord]; mdt<-mdt[ord]; asqt<-asqt[ord]; asot<-asot[ord]
# Find trades that occur on the same question, same option, with the same assumptions, within 6 hours of each other with no intervening trades.
for (t in 1:(length(tat)-1)) {
 #if (pit[t]==pit[t+1] &qit[t]==qit[t+1] &cit[t]==cit[t+1] &asqt[t]==asqt[t+1] &asot[t]==asot[t+1] &((tat[t+1]-base)-(tat[t]-base))<=0.25) {
  if (pit[t]==pit[t+1] &qit[t]==qit[t+1] &asqt[t]==asqt[t+1] &asot[t]==asot[t+1] &((tat[t+1]-base)-(tat[t]-base))<=0.25) {   # choice index might not be the same if adjusting distsribution
  tat[t] <- NA			# Keep t+1 trade. Ultimately only the final trade in a stuttered sequence will remain.
 }
}
good <- complete.cases(tat)
sum(!good)

tat<-tat[good]; tit<-tit[good]; pit<-pit[good]; qit<-qit[good]; nvt<-nvt[good]; ovt<-ovt[good]; ast<-ast[good]; apot<-apot[good]
cit<-cit[good]; rst<-rst[good]; mdt<-mdt[good]; asqt<-asqt[good]; asot<-asot[good]

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("De-stuttering Complete")
print(duration)