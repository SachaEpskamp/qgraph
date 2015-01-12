scale2 <- function(x) {
  if (sd(x,na.rm=TRUE)!=0){
    return((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
  } else {
    return(rep(0, length(x)))
  }
}