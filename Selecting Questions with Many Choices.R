##################################################################
#
#  Selecting questions with more than three or more states
#  inputs: qiq & rvq
#  outputs table of qiq and # choices (Questions with more than 2 choices.csv) 
################################################################
rvq <- qn$resolution_value_array
qiq <- qn$question_id

rvqa<- array(rep(0,length(qiq)*40),c(length(qiq),40))
rvql <- numeric()

for (q in 1:length(qiq)) {
  print(q)
temp1 <- as.double(strsplit(strsplit(strsplit(as.character(rvq[q]),"[",fixed=T)[[1]][2],"]",fixed=T)[[1]],",")[[1]])
rvqa[q,1:length(temp1)] <- temp1
rvql[q] <- length(temp1)
}

rvql[rvql=="1"] <- NA
goodRvql <- complete.cases(rvql)
sum(!goodRvql)
rvqlReslvd <- rvql[goodRvql]; qiqReslvd <- qiq[goodRvql]

rvqlReslvd[rvqlReslvd<3] <- NA
goodRvqlReslvd <- complete.cases(rvqlReslvd)
sum(!goodRvqlReslvd)
rvqlNonBinary <- rvqlReslvd[goodRvqlReslvd]; qiqNonBinary <- qiqReslvd[goodRvqlReslvd]

write.table(data.frame(qiqNonBinary,rvqlNonBinary),file="Questions with Many Choices.csv",sep=",",append=F,col.names=c("questionId","numchoices"),row.names=F)
