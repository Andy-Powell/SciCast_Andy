##########################################################
#
# Creating list of months for data analysis
#
##########################################################


dateStart <- as.POSIXct("2013-12-07")
dateStop <- as.POSIXct(trunc(Sys.time(), "days"))
yearStop <- as.numeric(format(dateStop, "%Y"))
monthStop <- as.numeric(format(dateStop, "%m"))
monthEnd <- dateStart
x <- 0
monthEnd[1] <- dateStart

for (year in 2014:yearStop){
  for (month in 1:12) {
 
    print(c(year,month))
    print(paste(year,"-",month,"-07",sep=""))
    tempEnd<- as.POSIXct(paste(year,"-",month,"-",07,sep=""))
    print(difftime(tempEnd,dateStop,"days"))
    if (difftime(tempEnd,dateStop,"days")<0) {
      monthEnd[month+(12*x+1)] <- tempEnd
    } else {break}   
  }
  x <- x+1
}

write.table(data.frame(monthEnd),file="SciCast Months.csv",sep=",",append=F,col.names="monthEnd",row.names=F)
