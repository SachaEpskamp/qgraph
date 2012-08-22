### CONVERTS CENTER COORDINATES TO EDGE OF NODE ###:

Cent2Edge <- function(x,y,r,cex,shape,offset=1)
{
  r <- r%%(2*pi)
  
  xrange <- abs(diff(par("usr")[1:2]))
  yrange <- abs(diff(par("usr")[3:4]))
  
  xmarrange <- sum(par("mai")[c(2,4)])
  ymarrange <- sum(par("mai")[c(1,3)])
  
  xin <- par("pin")[1]
  yin <- par("pin")[2]

#   if (shape == "circle")
#   {
    xNew <- x + offset*((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*sin(r)/17.5
    yNew <- y + offset*((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*cos(r)/17.5
  
#   }

  if (shape == "square")
  {
    dx <- xNew - x
    dy <- yNew - y
    
    widthX <- ((xin+xmarrange)/xin)*(7/(xin+xmarrange))*(xrange/2.16)*cex*par("csi")*1/17.5
    widthY <- ((yin+ymarrange)/yin)*(7/(yin+ymarrange))*(yrange/2.16)*cex*par("csi")*1/17.5    
    
    xNew <- x + min(abs(widthX/dx),abs(widthY/dy)) * dx
    yNew <- y + min(abs(widthX/dx),abs(widthY/dy)) * dy    
    return(c(x+offset*(xNew-x),y+offset*(yNew-y)))
  } else 
  {
    return(c(xNew,yNew)) 
  }
}

    