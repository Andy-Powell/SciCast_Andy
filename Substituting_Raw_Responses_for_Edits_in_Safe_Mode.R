frcst <- array(rep(0,length(tat)*40),c(length(tat),40))
for (t in 1:length(tat)) {
 if (mdt[t]==0) {
  temp <- as.double(strsplit(as.vector(nvt[t]),",")[[1]])
  frcst[t,1:length(temp)] <- temp
 }
 if (mdt[t]==1) {
  frcst[t,] <- svt[t,]										# Substitute raw response of safe mode.
 }
}