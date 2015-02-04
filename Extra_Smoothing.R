start <- Sys.time()

# Smoothing function    # I don't think this is actually doing anything.  What data is it acting on?

smth <- function(x) {y<-x; for (i in 3:(length(x)-2)) { y[i] <- x[i-2]*0.1+x[i-1]*0.2+x[i]*0.4+x[i+1]*0.2+x[i+2]*0.1}; return(y)}
# I tried several, but settled on this simple function.

duration <- as.double(difftime(Sys.time(),start,units="sec"))
print("Smooting Function Defined")
print(duration)