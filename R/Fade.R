Fade <- function(col,alpha,bg)
{
  # col = color to fade
  # bg = color to fade to
  # alpha = inverse transparency, 1 = fully visable, 0 = fully transparant.
  
  if (missing(bg)) bg <- par("bg")
  if (length(bg)!=1) stop("'bg' must be of length 1")
  if (length(alpha)==1) alpha <- rep(alpha,length(col))
  if (length(col)==1) col <- rep(col,length(alpha))
  if (length(col)!=length(alpha)) stop("Length of 'col' not equal to length of 'alpha'")
  
  n <- length(col)
  
  rgbCols <- col2rgb(col)/255
  rgbBG <-  col2rgb(bg)/255
  
  colAlpha <- col2rgb(col,alpha=TRUE)[4,]/255
  
  Mix <- rgbCols*rep(alpha,each=3) + rgbBG%*%t(1-alpha)
  
  return(rgb(Mix[1,],Mix[2,],Mix[3,],colAlpha))
}