## Function to mix color vector x with weight w
mixCols <- function(x,w)
{
  # x = vector of colors
  # w = weights
  if (missing(w)) w <- rep(1,length(x))
  if (length(w)==1) w <- rep(w,length(x))
  
  RGB <- col2rgb(x)
  wMeans <- apply(RGB,1,weighted.mean,w=w)
  return(rgb(wMeans[1],wMeans[2],wMeans[3],maxColorValue=255))
}