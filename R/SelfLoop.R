SelfLoop <- function(x,y,rotation=0,cex,cex2,shape,residual=FALSE,resScale=1, polygonList = polygonList, offset = 0)
{
  if (missing(polygonList))
  {
    polygonList = list(
      ellipse = ELLIPSEPOLY,
      heart  = HEARTPOLY,
      star = STARPOLY
    )
  }
  
  # If shape is rectangle, compute square on largext cex and move to border:
  if (shape == "rectangle")
  {
    loop <- SelfLoop(x,y,rotation,min(cex,cex2),min(cex,cex2),"square",residual,resScale)
    
    xOff <- (Cent2Edge(x,y,pi/2,cex,cex2,shape, offset=offset,polygonList = polygonList)[1] - x)
    yOff <- (Cent2Edge(x,y,0,cex,cex2,shape, offset=offset,polygonList = polygonList)[2] - y)
    
    SmallX <- (Cent2Edge(x,y,pi/2,min(cex,cex2),min(cex,cex2),"square", offset=offset,polygonList = polygonList)[1] - x)
    SmallY <- (Cent2Edge(x,y,0,min(cex,cex2),min(cex,cex2),"square", offset=offset,polygonList = polygonList)[2] - y)
    
    # Move up or down:
    if (cex2 > cex)
    {
      if (any(loop$x[c(1,length(loop$x))] > x-xOff & loop$x[c(1,length(loop$x))] < x+xOff))
      {
        if (cos(rotation) > 0)  
        {
          # Move up:
          loop$y <- loop$y + yOff - SmallY
        } else {
          # Move down:
          loop$y <- loop$y - yOff + SmallY
        }
      }
    } else 
    {
      if (any(loop$y[c(1,length(loop$y))] > y-yOff & loop$y[c(1,length(loop$y))] < y+yOff))
      {
        if (sin(rotation) > 0)  
        {
          # Move right:
          loop$x <- loop$x + xOff - SmallX
        } else {
          # Move left:
          loop$x <- loop$x - xOff + SmallX
        }
      }
      
    }
    
    
    return(loop)
  }
  
  loopAngle <- pi/8
  
  if (!residual)
  {
    Cent <- Cent2Edge(x,y,rotation,cex,cex2,shape,offset=offset,polygonList = polygonList)
    Cent[1] <- x + 1.5*(Cent[1]-x)
    Cent[2] <- y + 1.5*(Cent[2]-y)
    
    LoopPointsRight <- Cent2Edge(x,y,loopAngle + rotation,cex,cex2,shape, offset=offset,polygonList = polygonList)
    LoopPointsLeft <- Cent2Edge(x,y,(-1*loopAngle + rotation),cex,cex2,shape, offset=offset,polygonList = polygonList)
    
    Circ <- lapply(seq(1.5*pi+ rotation,2.5*pi + rotation,length=4),Cent2Edge,x=Cent[1],y=Cent[2],cex=0.8*min(cex,cex2),cex2=0.8*min(cex,cex2),shape="circle", polygonList = polygonList)
    #     deg <- atan2usr2in(LoopPointsRight[1] - LoopPointsLeft[1], LoopPointsRight[2] - LoopPointsLeft[2])
    #     Circ <- lapply(seq(deg-pi,deg,length=4),Cent2Edge,x=Cent[1],y=Cent[2],cex=0.8*mean(cex,cex2),cex2=0.8*mean(cex,cex2),shape="circle")
    CircX <- sapply(Circ,'[',1)
    CircY <- sapply(Circ,'[',2)
    
    CircX <- c(LoopPointsLeft[1],CircX,LoopPointsRight[1])
    CircY <- c(LoopPointsLeft[2],CircY,LoopPointsRight[2])
    
    spl <- xspline(CircX,CircY,1,draw=FALSE)
    return(spl)
  } else {
    Start <- Cent2Edge(x,y,rotation,cex,cex2,shape,offset=resScale ,polygonList = polygonList)
    End <- Cent2Edge(x,y,rotation,cex,cex2,shape,offset=offset,polygonList = polygonList)
    #     Start <- c(0,0)
    #     Start[1] <- x + 2*(End[1]-x)
    #     Start[2] <- y + 2*(End[2]-y)
    
    spl <- xspline(c(Start[1],End[1]),c(Start[2],End[2]),1,draw=FALSE)
    return(spl)
  }
}