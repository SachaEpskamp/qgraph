scale2 <- function(x) {
  if (sd(x)!=0){
    return((x-mean(x))/sd(x))
  } else {
    return(rep(0, length(x)))
  }
}