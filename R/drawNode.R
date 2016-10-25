darken <- function(x, dark = 0.25){
  sapply(x,function(xx){
    col <- c(col2rgb(xx))/255
    col <- (1-dark)*col
    rgb(col[1],col[2],col[3])      
  })

}

drawNode <- function(x, y, shape, cex1, cex2, border, vcolor, bcolor, border.width, polygonList, bars, barSide, barColor, barLength, barsAtSide, font = 1, 
                     usePCH = TRUE, resolution = 100, noPar = FALSE, bw = FALSE, density = NULL, angle = NULL,
                     mean, SD, meanRange, pie, pieColor = NA, pieColor2 = "white", pieBorder = 0.15, pieStart = 0,
                     pieDarken = 0.25, pastel = FALSE,rainbowStart=0)
{
  if (!is.null(pie) &&  !shape %in% c("circle", "square",names(polygonList))){
    stop("Pie charts only supported for shape = 'circle' or shape = 'square'")
  }
  if (shape %in% c("circle","square","triangle","diamond"))
  {

    if (is.null(pie) && (usePCH | shape %in% c("triangle","diamond")))
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
      if (is.null(pie) && shape == "square")
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
        
        # Draw the node:
        polygon(xs, ys, lwd=border.width, border = bord, col = vcolor, density = density, angle = angle)
 
        # Draw the pie diagram:
        if (!is.null(pie)){
          # If any element not in (0,1),stop:
          if(any(pie < 0 | pie > 1)) stop("All elements in 'pie' must be between 0 and 1.")
          
          # Check sum:
          if (sum(pie) > 1){
            stop("Sum of 'pie' argument may not be greater than 1 for any node.")
          }
          
          # Rep arguments:
          if (length(pie) != length(pieColor)){
            pieColor <- rep(pieColor,length=length(pie))
          }

          # Add NA colors:
          if (any(is.na(pieColor))){
            # if length = 1, inherit from node color but 50% darker:
            if (length(pie) == 1){
              pieColor <- darken(vcolor,pieDarken)
            } else {
              if (!pastel){
                pieColor[is.na(pieColor)] <- rainbow(sum(is.na(pieColor)))
              } else {
                pieColor[is.na(pieColor)] <- rainbow_hcl(sum(is.na(pieColor)), start = rainbowStart * 360, end = (360 * rainbowStart + 360*(sum(is.na(pieColor))-1)/sum(is.na(pieColor))))
              }
            }
          }

          
          # Add sum != 1, add one part and white color:
          if (sum(pie)!=1){
            pie <- c(pie,1-sum(pie))
            pieColor <- c(pieColor,pieColor2)
          }
        
          nPie <- length(pie)
          pie <- c(0,cumsum(pie))
          pie <- pieStart + pie # Shift pie diagram
          
          for (i in seq_len(nPie)){
            # Step 1: compute first pie part:
            innerCoord <- lapply(seq(pie[i]*2*pi,pie[i+1]*2*pi,length=resolution),function(r)Cent2Edge(x,y,r,(1-pieBorder) * cex1,(1-pieBorder) * cex2,shape, noPar = noPar))
            innerXs <- sapply(innerCoord,'[[',1)
            innerYs <- sapply(innerCoord,'[[',2)
            
            outerCoord <- lapply(seq(pie[i]*2*pi,pie[i+1]*2*pi,length=resolution),function(r)Cent2Edge(x,y,r,cex1,cex2,shape, noPar = noPar))
            outerXs <- sapply(outerCoord,'[[',1)
            outerYs <- sapply(outerCoord,'[[',2)
            
            pie1Xs <- c(outerXs,rev(innerXs))
            pie1Ys <- c(outerYs,rev(innerYs))
            
            # Plot first pie part:
            polygon(pie1Xs, pie1Ys, lwd=border.width, border = bord, col = pieColor[i])
          }

          
        } 
        
        
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
    
    
    # Draw the pie diagram:
    if (!is.null(pie)){
      if (pieStart != 0){
        stop("'pieStart' argument not supported when using shape = 'heart', shape = 'star' or shape = 'ellipse'")
      }
      # If any element not in (0,1),stop:
      if(any(pie < 0 | pie > 1)) stop("All elements in 'pie' must be between 0 and 1.")
      
      # Check sum:
      if (sum(pie) > 1){
        stop("Sum of 'pie' argument may not be greater than 1 for any node.")
      }
      
      # Rep arguments:
      if (length(pie) != length(pieColor)){
        pieColor <- rep(pieColor,length=length(pie))
      }
      
      # Add NA colors:
      if (any(is.na(pieColor))){
        # if length = 1, inherit from node color but 50% darker:
        if (length(pie) == 1){
          pieColor <- darken(vcolor,pieDarken)
        } else {
          if (!pastel){
            pieColor[is.na(pieColor)] <- rainbow(sum(is.na(pieColor)))
          } else {
            pieColor[is.na(pieColor)] <- rainbow_hcl(sum(is.na(pieColor)), start = rainbowStart * 360, end = (360 * rainbowStart + 360*(sum(is.na(pieColor))-1)/sum(is.na(pieColor))))
          }
        }
      }
      
      
      # Add sum != 1, add one part and white color:
      if (sum(pie)!=1){
        pie <- c(pie,1-sum(pie))
        pieColor <- c(pieColor,pieColor2)
      }
      
      nPie <- length(pie)
      pie <- c(0,cumsum(pie))
      pie <- pieStart + pie # Shift pie diagram
      
      Inds <- 1 + round(pie * (length(polygonList[[shape]]$x)-1))

      for (i in seq_len(nPie)){
        # Step 1: compute first pie part:
        innerXs <- x + polygonList[[shape]]$x[Inds[i]:Inds[i+1]] * xOff * (1-pieBorder)
        innerYs <- y + polygonList[[shape]]$y[Inds[i]:Inds[i+1]] * yOff * (1-pieBorder)
        
        outerXs <- x + polygonList[[shape]]$x[Inds[i]:Inds[i+1]] * xOff
        outerYs <- y + polygonList[[shape]]$y[Inds[i]:Inds[i+1]] * yOff
        
        pie1Xs <- c(outerXs,rev(innerXs))
        pie1Ys <- c(outerYs,rev(innerYs))
        
        # Plot first pie part:
        polygon(pie1Xs, pie1Ys, lwd=border.width, border = bord, col = pieColor[i])
      }
      
      
    } 
    
    
    
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

