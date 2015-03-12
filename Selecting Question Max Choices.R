##################################################################
#
#  Selecting questions with more than three or more states
#  inputs: qiq (or rsq) & rvq
#  outputs table of qiq and # choices
#  outputs table of qiq and # choices (Questions with more than maxChoices choices.csv) 
#  modifies rsq
#  Used by Accuracy and Uniform.R
#
################################################################
startSel <- Sys.time() 
print("Selceting Questions with few choices started")

rvqChoice <- qn$resolution_value_array
qiqChoice <- qn$question_id

rvqa<- rvqlReslvd <- array(rep(0,length(qiqChoice)*40),c(length(qiqChoice),40))
qiqNonBinary <- rep(0, length(qiqChoice))
rvql <- numeric()

print("Max number of choices (<=)")
maxChoices <- as.integer(readline())

for (q in 1:length(qiqChoice)) {
 # print(q)
temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvqChoice[q]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
rvqa[q,1:length(temp1)] <- temp1
rvql[q] <- length(temp1)
}

#rvql[rvql=="1"] <- NA
#goodRvql <- complete.cases(rvql)
#sum(!goodRvql)
#rvqlReslvd <- rvql[goodRvql]; qiqReslvd <- qiq[goodRvql]

title <- paste("Question Choices.csv", sep="" )
write.table(data.frame(qiqChoice,rvql),file="Question Choices.csv",sep=",",append=F,col.names=c("questionId","numChoices"),row.names=F)

rvqlReslvd[rvql<=maxChoices] <- NA
goodRvqlReslvd <- complete.cases(rvqlReslvd)
sum(!goodRvqlReslvd)
rvqlNonBinary <- rvqlReslvd[goodRvqlReslvd]; qiqNonBinary <- qiq[goodRvqlReslvd]

title <- paste("Questions with more than ", maxChoices, " choices.csv", sep="" )
write.table(data.frame(qiqNonBinary,rvqlNonBinary),file="Questions with too many choices.csv",sep=",",append=F,col.names=c("questionId","numChoices"),row.names=F)

duration <- as.double(difftime(Sys.time(),startSel,units="sec"))   #reports time to retrieve files
print ("Question Selection Complete")
print(duration)

