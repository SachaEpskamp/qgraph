### CONVERTS CENTER COORDINATES TO EDGE OF NODE ###:

Cent2Edge <- function(x,y,r,cex,cex2,shape,offset=0, polygonList, noPar = FALSE)
{
  r <- r%%(2*pi)
  
  if (missing(polygonList))
  {
    polygonList = list(
      ellipse = ELLIPSEPOLY,
      heart  = HEARTPOLY,
      star = STARPOLY
    )
  }
  
  if (shape %in% names(polygonList))
  {
    xOff <- Cent2Edge(x,y,pi/2,cex,cex2,"rectangle")[1] - x
    yOff <- Cent2Edge(x,y,0,cex,cex2,"rectangle")[2] - y
    
    xOutline <- x + polygonList[[shape]]$x * xOff
    yOutline <-  y + polygonList[[shape]]$y * yOff
    
    rad <- atan2usr2in(xOutline - x, yOutline - y)
    xNew <- xOutline[which.min(abs(rad - r))]
    yNew <- yOutline[which.min(abs(rad - r))]
    
    return(c(x+(cex+offset)/cex*(xNew-x),y+(cex+offset)/cex*(yNew-y)))
  } else {

    # Set mar:
    marOrig <- par("mar")
    if (!noPar) par(mar=c(0,0,0,0))
    
    r <- r%%(2*pi)
    
    xrange <- abs(diff(par("usr")[1:2]))
    yrange <- abs(diff(par("usr")[3:4]))
    
    xmarrange <- sum(par("mai")[c(2,4)])
    ymarrange <- sum(par("mai")[c(1,3)])
    
    xin <- par("pin")[1]
    yin <- par("pin")[2]
    

    
    #   if (shape == "circle")
    #   {
    xNew <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*(cex+offset)*(1 + 1/2*(names(dev.cur())=="devSVG"))*par("csi")*sin(r)/17.5
    yNew <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*(cex+offset)*(1 + 1/2*(names(dev.cur())=="devSVG"))*par("csi")*cos(r)/17.5
    
    #   }
    
    if (shape == "square")
    {
      dx <- xNew - x
      dy <- yNew - y
      
      widthX <- ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*1/17.5
      widthY <- ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*1/17.5    
      
      xNew <- x + min(abs(widthX/dx),abs(widthY/dy)) * dx
      yNew <- y + min(abs(widthX/dx),abs(widthY/dy)) * dy    
      
      # Restore mar:
      if (!noPar) par(mar=marOrig)
      return(c(x+(cex+offset)/cex*(xNew-x),y+(cex+offset)/cex*(yNew-y)))
    } else if (shape == "rectangle")
    {
      dx <- xNew - x
      dy <- yNew - y
      
      widthX <- ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*1/17.5
      widthY <- ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex2*par("csi")*1/17.5    
      
      xNew <- x + min(abs(widthX/dx),abs(widthY/dy)) * dx
      yNew <- y + min(abs(widthX/dx),abs(widthY/dy)) * dy    
      
      # Restore mar:
      if (!noPar) par(mar=marOrig)
      return(c(x+(cex+offset)/cex*(xNew-x),y+(cex2+offset)/cex2*(yNew-y)))
    } else  {
      # Restore mar:
      if (!noPar) par(mar=marOrig)
      return(c(xNew,yNew)) 
    }
  }
}

