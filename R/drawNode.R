
drawNode <- function(x, y, shape, cex1, cex2, border, vcolor, bcolor, border.width, polygonList, bars, barSide, barColor, barLength, barsAtSide, font = 1)
{

  if (shape %in% c("circle","square","triangle","diamond"))
  {
    if (shape=="circle")
    {
      pch1=16
      pch2=1
    }
    if (shape=="square")
    {
      pch1=15
      pch2=0
    }
    if (shape=="triangle")
    {
      pch1=17
      pch2=2
    }
    if (shape=="diamond")
    {
      pch1=18
      pch2=5
    }
    
    points(x, y, ,cex=cex1,col=vcolor,lwd=border.width,pch=pch1)
    if (border)
    {
      points(x, y, ,cex=cex1,col=bcolor,lwd=border.width,pch=pch2)
    }
  } 
  else if (shape == "rectangle")  {
    xOff <- Cent2Edge(x,y,pi/2,cex1,cex2,shape)[1] - x
    yOff <- Cent2Edge(x,y,0,cex1,cex2,shape)[2] - y
    
    # Plot background:
    rect(x-xOff,y-yOff,x+xOff,y+yOff,col=vcolor,border=NA)
    if (border)
    {
      rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor,lwd=border.width)
    }              
  } else if (shape %in% names(polygonList))
  {
    xOff <- Cent2Edge(x,y,pi/2,cex1,cex2,"rectangle")[1] - x
    yOff <- Cent2Edge(x,y,0,cex1,cex2,"rectangle")[2] - y
    
    if (border) bord <- bcolor else bord <- NA
    
    polygon(x + polygonList[[shape]]$x * xOff, y + polygonList[[shape]]$y * yOff, lwd=border.width, border = bord, col = vcolor)

  } else stop(paste("Shape",shape,"is not supported or included in 'polygonList'."))
  
  ### ADD BARS ####
  if (!is.null(bars))
  {
    if (any(bars < 0) | any(bars > 1))
    {
      warning("Bar detected < 0 or > 1, unexpected results might occur.")
    }
    for (i in seq_along(bars))
    {
      IntInNode(t(c(x,y)),cex1,cex2,shape,bars[i],width=barLength,triangles=FALSE,col=barColor,barSide,!barsAtSide) 
    }
  }
  
  
}