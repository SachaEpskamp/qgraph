

### CONVERTS CENTER COORDINATES TO EDGE OF NODE ###:

DrawArrow <- function(x,y,r,angle=pi/4,cex,open=FALSE,lwd=1,lty=1,col="black")
{
  r <- r%%(2*pi)
  
  xrange <- abs(diff(par("usr")[1:2]))
  yrange <- abs(diff(par("usr")[3:4]))
  
  xmarrange <- sum(par("mai")[c(2,4)])
  ymarrange <- sum(par("mai")[c(1,3)])
  
  xin <- par("pin")[1]
  yin <- par("pin")[2]
  
  xLeft <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r-angle + pi)/17.5
  yLeft <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r-angle + pi)/17.5
  
  xRight <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r+angle + pi)/17.5
  yRight <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r+angle + pi)/17.5

  if (open) 
  {
    lines(c(xLeft,x,xRight),c(yLeft,y,yRight),lwd=lwd,col=col,lty=lty)
  } else {
    polygon(c(xLeft,x,xRight),c(yLeft,y,yRight),lwd=lwd,col=col,border=NA)
  }
}

## Midpoint of arrow:
ArrowMidPoint <- function(x,y,r,angle=pi/4,cex)
{
  r <- r%%(2*pi)
  
  xrange <- abs(diff(par("usr")[1:2]))
  yrange <- abs(diff(par("usr")[3:4]))
  
  xmarrange <- sum(par("mai")[c(2,4)])
  ymarrange <- sum(par("mai")[c(1,3)])
  
  xin <- par("pin")[1]
  yin <- par("pin")[2]
  
  xLeft <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r-angle + pi)/17.5
  yLeft <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r-angle + pi)/17.5
  
  xRight <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r+angle + pi)/17.5
  yRight <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r+angle + pi)/17.5
  
  mids <- c((xRight+xLeft)/2,(yRight+yLeft)/2)
  
  return(mids)
}

# Radius of arrow in inches:
ArrowRadIn <- function(angle=pi/4,cex)
{
  r <- 0
  
  xrange <- abs(diff(par("usr")[1:2]))
  yrange <- abs(diff(par("usr")[3:4]))
  
  xmarrange <- sum(par("mai")[c(2,4)])
  ymarrange <- sum(par("mai")[c(1,3)])
  
  xin <- par("pin")[1]
  yin <- par("pin")[2]
  
  xLeft <- ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r-angle + pi)/17.5
  yLeft <- ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r-angle + pi)/17.5
  
  xRight <- ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r+angle + pi)/17.5
  yRight <- ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r+angle + pi)/17.5
  
  mids <- c(usr2inX2((xRight+xLeft)/2),usr2inY2((yRight+yLeft)/2))
  
  return(sqrt(sum(mids^2)))
}

