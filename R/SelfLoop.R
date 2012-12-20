SelfLoop <- function(x,y,rotation=0,cex,cex2,shape,residual=FALSE,resScale=1)
{
  loopAngle <- pi/8
  
  if (!residual)
  {
    Cent <- Cent2Edge(x,y,rotation,cex,cex2,shape)
    Cent[1] <- x + 1.5*(Cent[1]-x)
    Cent[2] <- y + 1.5*(Cent[2]-y)
    
    LoopPointsRight <- Cent2Edge(x,y,loopAngle + rotation,cex,cex2,shape)
    LoopPointsLeft <- Cent2Edge(x,y,(-1*loopAngle + rotation),cex,cex2,shape)

    Circ <- lapply(seq(1.5*pi+ rotation,2.5*pi + rotation,length=4),Cent2Edge,x=Cent[1],y=Cent[2],cex=0.8*mean(cex,cex2),cex2=0.8*mean(cex,cex2),shape="circle")
#     deg <- atan2usr2in(LoopPointsRight[1] - LoopPointsLeft[1], LoopPointsRight[2] - LoopPointsLeft[2])
#     Circ <- lapply(seq(deg-pi,deg,length=4),Cent2Edge,x=Cent[1],y=Cent[2],cex=0.8*mean(cex,cex2),cex2=0.8*mean(cex,cex2),shape="circle")
    CircX <- sapply(Circ,'[',1)
    CircY <- sapply(Circ,'[',2)
    
    CircX <- c(LoopPointsLeft[1],CircX,LoopPointsRight[1])
    CircY <- c(LoopPointsLeft[2],CircY,LoopPointsRight[2])
    
    spl <- xspline(CircX,CircY,1,draw=FALSE)
    return(spl)
  } else {
    Start <- Cent2Edge(x,y,rotation,cex,cex2,shape,offset=resScale)
    End <- Cent2Edge(x,y,rotation,cex,cex2,shape,offset=0)
#     Start <- c(0,0)
#     Start[1] <- x + 2*(End[1]-x)
#     Start[2] <- y + 2*(End[2]-y)
    
    spl <- xspline(c(Start[1],End[1]),c(Start[2],End[2]),1,draw=FALSE)
    return(spl)
  }
}