### Draws an edge. x = vector of x coordinates, y = vector of y coordinates:

drawEdge <- function(x,y,col=1,lwd=1,arrowlwd=1,lty=1,directed=FALSE,bidirectional=FALSE,arrows=TRUE,arrowAngle=pi/6,open=FALSE)
{
  stopifnot(length(x)==length(y))
  
  n <- length(x)
  xRange <- c(x[1],x[n])
  yRange <- c(y[1],y[n])
  
  stopifnot(n>1)
  
  # If arrowheads are drawn, censor start and finish of edge:
  if (directed && isTRUE(arrows))
  {
    # Radius of the arrow in inches:
    arrowRad <- ArrowRadIn(angle=arrowAngle,cex=arrowlwd)
    
    # Distance of each curve point in inches:
    curveDist <- sqrt((usr2inX(x)-usr2inX(xRange[2]))^2 + (usr2inY(y)-usr2inY(yRange[2]))^2)
    
    # Points that fall outside the arrow:    
    OutsideArrow <- curveDist > arrowRad
    
    # Censor x and y:
    x <- x[rev(cumsum(rev(OutsideArrow))>0)]
    y <- y[rev(cumsum(rev(OutsideArrow))>0)]
    
    # Add midpoint:
    mid <- ArrowMidPoint(xRange[2],yRange[2],atan2usr2in(xRange[2]-x[length(x)],yRange[2]-y[length(y)]),angle=arrowAngle,cex=arrowlwd)
        
    x <- c(x,mid[1])
    y <- c(y,mid[2])
    
    if (bidirectional)
    {
      # Distance of each curve point in inches:
      curveDist <- sqrt((usr2inX(x)-usr2inX(xRange[1]))^2 + (usr2inY(y)-usr2inY(yRange[1]))^2)
      
      # Points that fall outside the arrow:    
      OutsideArrow <- curveDist > arrowRad
      
      # Censor x and y:
      x <- x[cumsum(OutsideArrow)>0]
      y <- y[cumsum(OutsideArrow)>0]
      
      # Add midpoint:
      mid <- ArrowMidPoint(xRange[1],yRange[1],atan2usr2in(xRange[1]-x[1],yRange[1]-y[1]),angle=arrowAngle,cex=arrowlwd)
      
      x <- c(mid[1],x)
      y <- c(mid[2],y)
    }
    
    n <- length(x) 
  }
  
  # Draw the edge:
  lines(x,y,lwd=lwd,col=col,lty=lty)
  
  # Draw the arrowheads:
  if (directed)
  {
    if (!is.logical(arrows))
    {
      if (n > 2)
      {
        Ax=seq(1,n,length=arrows+2)
        Ay=seq(1,n,length=arrows+2)
        for (a in 2:(arrows+1))
        {
          DrawArrow(x[Ax[a]+1],y[Ay[a]+1],atan2usr2in(x[Ax[a]+1]-x[Ax[a]],y[Ay[a]+1]-y[Ay[a]]),angle=arrowAngle,cex=arrowlwd,open=open,lwd=max(lwd/2,1),lty=lty,col)
        }
      } else {
        Ax=seq(x[1],x[n],length=arrows+2)
        Ay=seq(y[1],y[n],length=arrows+2)
        for (a in 1:arrows+1)
        {
          DrawArrow(Ax[a],Ay[a],atan2usr2in(Ax[a]-x[1],Ay[a]-y[1]),angle=arrowAngle,cex=arrowlwd,open=open,lwd=max(lwd/2,1),lty=lty,col)
        } 
      }
    }
    
    else if (arrows)
    {
      DrawArrow(xRange[2],yRange[2],atan2usr2in(xRange[2]-x[n-1],yRange[2]-y[n-1]),angle=arrowAngle,cex=arrowlwd,open=open,lwd=max(lwd/2,1),lty=lty,col)
      
      if (bidirectional)
      {
        DrawArrow(xRange[1],yRange[1],atan2usr2in(xRange[1]-x[2],yRange[1]-y[2]),angle=arrowAngle,cex=arrowlwd,open=open,lwd=max(lwd/2,1),lty=lty,col)
      }
    }
  }
}