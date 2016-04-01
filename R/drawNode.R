
drawNode <- function(x, y, shape, cex1, cex2, border, vcolor, bcolor, border.width, polygonList, bars, barSide, barColor, barLength, barsAtSide, font = 1, 
                     usePCH = TRUE, resolution = 100, noPar = FALSE, bw = FALSE, density = NULL, angle = NULL,
                     mean, SD, meanRange)
{
  if (shape %in% c("circle","square","triangle","diamond"))
  {

    if (usePCH | shape %in% c("triangle","diamond"))
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
    } else {
      if (shape == "square")
      {
          xOff <- Cent2Edge(x,y,pi/2,cex1,cex1,shape, noPar = noPar)[1] - x
          yOff <- Cent2Edge(x,y,0,cex1,cex1,shape, noPar = noPar)[2] - y
          
          if(bw)
          # Plot a white background behind each node to avoid transparency due to density in the next line
          {
            rect(x-xOff,y-yOff,x+xOff,y+yOff,col="white",border=NA)
          }
          # Plot background:
          rect(x-xOff,y-yOff,x+xOff,y+yOff,col=vcolor,border=NA, density = density, angle = angle)
          if (border)
          {
            rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor,lwd=border.width)
          }  
      } else {
        
        Coord <- lapply(seq(0,2*pi,length=resolution),function(r)Cent2Edge(x,y,r,cex1,cex2,shape, noPar = noPar))
        
        
        xs <- sapply(Coord,'[[',1)
        ys <- sapply(Coord,'[[',2)
        
        if (border) bord <- bcolor else bord <- NA
        
        if(bw)
          # Plot a white background behind each node to avoid transparency due to density in the next line
        {
          polygon(xs, ys, lwd=border.width, border = bord, col = "white")
        }
        
        polygon(xs, ys, lwd=border.width, border = bord, col = vcolor, density = density, angle = angle)
        
      }

    }
    
  } 
  else if (shape == "rectangle")  {
    xOff <- Cent2Edge(x,y,pi/2,cex1,cex2,shape, noPar = noPar)[1] - x
    yOff <- Cent2Edge(x,y,0,cex1,cex2,shape, noPar = noPar)[2] - y
    
    
    if(bw)
    {
      # Plot a white background behind each node to avoid transparency due to density in the next line
      rect(x-xOff,y-yOff,x+xOff,y+yOff,col="white",border=NA)
    }
    
    # Plot background:
    rect(x-xOff,y-yOff,x+xOff,y+yOff,col=vcolor,border=NA, density = density, angle = angle)
    if (border)
    {
      rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor,lwd=border.width)
    }              
  } else if (shape %in% names(polygonList))
  {
    xOff <- Cent2Edge(x,y,pi/2,cex1,cex2,"rectangle", noPar = noPar)[1] - x
    yOff <- Cent2Edge(x,y,0,cex1,cex2,"rectangle", noPar = noPar)[2] - y
    
    if (border) bord <- bcolor else bord <- NA
    
    if(bw)
    {
      # Plot a white background behind each node to avoid transparency due to density in the next line
      polygon(x + polygonList[[shape]]$x * xOff, y + polygonList[[shape]]$y * yOff, lwd=border.width, border = bord, col = "white")
    }
    
    polygon(x + polygonList[[shape]]$x * xOff, y + polygonList[[shape]]$y * yOff, lwd=border.width, border = bord, col = vcolor, density = density, angle = angle)

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

  ### Add means in same way:
  if (!is.na(mean)){
  
    meanScaled <- (mean - meanRange[1]) / (meanRange[2] - meanRange[1])
    endScaled <- min(1,(mean + 2*SD - meanRange[1]) / (meanRange[2] - meanRange[1]))
    startScaled <- max(0,(mean - 2*SD - meanRange[1]) / (meanRange[2] - meanRange[1]))
    
    # Draw mean:
    IntInNode(t(c(x,y)),cex1,cex2,shape,meanScaled,width=0.5,triangles=FALSE,col=barColor,barSide,!barsAtSide) 

    # Draw SD ends:
    if (endScaled < 1){
      IntInNode(t(c(x,y)),cex1,cex2,shape,endScaled,width=0.25,triangles=FALSE,col=barColor,barSide,!barsAtSide) 
    }
    
    if (startScaled > 0){
      IntInNode(t(c(x,y)),cex1,cex2,shape,startScaled,width=0.25,triangles=FALSE,col=barColor,barSide,!barsAtSide) 
    }
    
    # Draw horizontal bar:
    IntInNode(t(c(x,y)),cex1,cex2,shape,mean(c(startScaled,endScaled)),width=endScaled - startScaled,triangles=FALSE,col=barColor,barSide,!barsAtSide, flip=TRUE) 
    
  }
  
  
}

