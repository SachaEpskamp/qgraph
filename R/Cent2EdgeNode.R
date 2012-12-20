### CONVERTS CENTER COORDINATES TO EDGE OF NODE ###:

Cent2Edge <- function(x,y,r,cex,cex2,shape,offset=0)
{
  # Set mar:
  marOrig <- par("mar")
  par(mar=c(0,0,0,0))
  
  r <- r%%(2*pi)
  
  xrange <- abs(diff(par("usr")[1:2]))
  yrange <- abs(diff(par("usr")[3:4]))
  
  xmarrange <- sum(par("mai")[c(2,4)])
  ymarrange <- sum(par("mai")[c(1,3)])
  
  xin <- par("pin")[1]
  yin <- par("pin")[2]

#   if (shape == "circle")
#   {
    xNew <- x + ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*(cex+offset)*par("csi")*sin(r)/17.5
    yNew <- y + ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*(cex+offset)*par("csi")*cos(r)/17.5
  
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
    par(mar=marOrig)
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
    par(mar=marOrig)
    return(c(x+(cex+offset)/cex*(xNew-x),y+(cex2+offset)/cex2*(yNew-y)))
  } else 
  {
    # Restore mar:
    par(mar=marOrig)
    return(c(xNew,yNew)) 
  }
}

    