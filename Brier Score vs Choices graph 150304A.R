#######################################################
#
#  Plot of Brier score vs. choices (>=3)
#  Uses "Brier Scores All Qestions.csv" from Accuracy and Uniform
#  Uses "Questions with Many Choices.csv" from Selecting Questions with Many Choices.R
#
#######################################################

br <-read.csv("Brier Scores All Qestions.csv")
c3 <-read.csv("Question Choices.csv")



validQstnId <- br$Question_Number[br$Question_Number%in%c3$questionId]
brier <- rep(0,length(validQstnId))
numChoices <- rep(0,length(validQstnId))

print("Use all questions? (y/n)")
allQstn <- readline()

if (allQstn=="n") {
  print("Max number of choices?")
  maxChoices <- readline()
  c3$questionId[c3$numChoices>maxChoices] <- NA
  goodQstn <- complete.cases(c3$questionId)
  c3$questoinId <- c3$questoinId[goodQstn]; c3$numChoices - c3$numChoices[goodQstn]
}

for (q in 1:length(validQstnId)) {
  brier[q] <- br$SciCast_Brier_Score[br$Question_Number==validQstnId[q]]
  numChoices[q] <- c3$numChoices[c3$questionId==validQstnId[q]]

}


date <- Sys.Date()                                           #Adding Date to title
title <- paste("Brier Score vs Number of Choices ",date,".png", collapse="")      #Expanding title name

png(title, width = 7200, height = 3600, pointsize = 18, res = 360)
par(mar=c(5,4,4,4))
plot(numChoices, brier, type="n",main="Brier Score vs. Number of Choices", xlab="Number of Choices ", ylab="Brier Score ", pch=19)
text(numChoices,brier,labels=validQstnId)
dev.off()


png(file="animals72.png",width=400,height=350,res=72)
plot(Animals, log="xy", type="n", main="Animal brain/body size")
text(Animals, lab=row.names(Animals))
dev.off()