start <- Sys.time()

### timeframe?

# Removing admin accounts and activity
# Match to Steve's list!

# Creating Dummy variable to track binary questions.  Question 206 is misclassified, however.
clq <- qn$classification
cls <- rep(0,length(clq))
cls[clq=="binary"] <- 1
cls[qiq=="206"] <- 1

# Creating variables for the number of safe-mode forecasters and the time since the first safe-mode forecast on each question.
nsmfq <- nsmdq <- rep(0,length(qiq))
for (q in 1:length(qiq)) {
  nsmfq[q] <- length(unique(pit[qit==qiq[q] &mdt==1 &asqt<0]))  # number of unique th_user_ids for each qr_question_id which are same (mdt==1) and non-conditional (asqt<0)
  if (nsmfq[q]>0) {
    nsmdq[q] <- (tstop-tstart) - min(floor(tat[qit==qiq[q] &mdt==1 &asqt<0]-tstart))  # floor -> first trade for each question that is safe (mdt==1) and non-conditional (asqt<0) (min???)
  }
}

# Restrict analysis to public questions and also binary questions with 10 safe-mode forecasters and at least 10 days since the first safe-mode forecast (ULinOP).
# Check that 232 and 648 are included!qn$
#rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &ctq!="Study 2.1" &ctq!="Study 2.1,Study 2.1" &"Public"%in%gq &vldq==1 &cls==1 &nsmfq>9 &nsmdq>9]))) 
rsq <- sort(levels(factor(qiq[saq<=tstop &caq>tstart &"Public"%in%gq &vldq==1]))) 
# selects and sorts qr_user_id for
# resolution before tstop & question creted after tstart
# catagories not "Study 2.1" or &ctq!="Study 2.1,Study 2.1   ###note - study 2.1 only appears in group - cqt is qn$catagories -> not used
# public is in gq
# question not designated invalid questions (vldq==1)
# is bianary (cls==1)
# has 10+ users executing trades (nsmfq>9)
####### time since first safe-mode trade > 9 days ?????  This is not the same as 10+ day with trades. ########

#rsq[rsq=="5"] <- NA                    # Steve doesn't include because of mixture resolution.
#rsq[rsq=="671"] <- NA                  # Steve doesn't include because of mixture resolution.
good <- complete.cases(rsq); rsq<-rsq[good]  


duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Data Selection Complete")
print(duration)