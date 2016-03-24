plot.qgraph <- function(x, ...)
{
  ### Extract arguments:
  # My apologies, dear people that actually read my code, for the right 
  # assignment operator---I was lazy.
  
  ## Edgelist:
  E <- list()
  x$Edgelist$from -> E$from
  x$Edgelist$to -> E$to
  x$Edgelist$weight -> E$weight
  x$Edgelist$directed -> directed
  x$Edgelist$bidirectional -> bidirectional
  
  # Nodes:
  x$graphAttributes$Nodes$border.color -> bcolor
  x$graphAttributes$Nodes$borders -> borders
  x$graphAttributes$Nodes$border.width -> border.width
  x$graphAttributes$Nodes$label.cex -> label.cex
  x$graphAttributes$Nodes$label.font -> label.font
  x$graphAttributes$Nodes$label.color -> lcolor
  x$graphAttributes$Nodes$labels -> labels
  x$graphAttributes$Nodes$names -> nodeNames
  x$graphAttributes$Nodes$loopRotation -> loopRotation
  x$graphAttributes$Nodes$shape -> shape
  x$graphAttributes$Nodes$color -> vertex.colors
  x$graphAttributes$Nodes$width -> vsize
  x$graphAttributes$Nodes$height -> vsize2
  x$graphAttributes$Nodes$subplots -> subplots
  x$graphAttributes$Nodes$images -> images
  x$graphAttributes$Nodes$tooltips -> tooltips
  x$graphAttributes$Nodes$SVGtooltips -> SVGtooltips
  x$graphAttributes$Nodes$bars -> bars
  x$graphAttributes$Nodes$barSide -> barSide
  x$graphAttributes$Nodes$barColor -> barColor
  x$graphAttributes$Nodes$barLength -> barLength
  x$graphAttributes$Nodes$means -> means
  x$graphAttributes$Nodes$SDs -> SDs
  
  # for BW only
  bw <- FALSE
  if(!is.null(x$graphAttributes$Nodes$density))
  {
    x$graphAttributes$Nodes$density -> density
    bw <- TRUE
  } else density <- rep(NA, length(shape))
    
  if(!is.null(x$graphAttributes$Nodes$angle))
  {
    x$graphAttributes$Nodes$angle -> angle
  }  else angle <- rep(0, length(shape))
    
  
  
  # Edges:
  x$graphAttributes$Edges$curve -> curve
  x$graphAttributes$Edges$color -> edge.color
  x$graphAttributes$Edges$labels -> edge.labels
  x$graphAttributes$Edges$label.cex -> edge.label.cex
  x$graphAttributes$Edges$label.bg -> edge.label.bg
  x$graphAttributes$Edges$label.font -> edge.label.font
  x$graphAttributes$Edges$label.color -> ELcolor
  x$graphAttributes$Edges$width -> edge.width
  x$graphAttributes$Edges$lty -> lty
  x$graphAttributes$Edges$edge.label.position -> edge.label.position
  x$graphAttributes$Edges$asize -> asize
  x$graphAttributes$Edges$residEdge -> residEdge
  x$graphAttributes$Edges$CircleEdgeEnd -> CircleEdgeEnd
  x$graphAttributes$Edges$Pvals -> Pvals
  x$graphAttributes$Edges$parallelEdge -> parallelEdge
  x$graphAttributes$Edges$parallelAngle -> parallelAngle
  x$graphAttributes$Edges$edgeConnectPoints -> edgeConnectPoints
  
  # Knots:
  x$graphAttributes$Knots$knots -> knots
  x$graphAttributes$Knots$knot.size -> knot.size
  x$graphAttributes$Knots$knot.color -> knot.color
  x$graphAttributes$Knots$knot.borders -> knot.borders
  x$graphAttributes$Knots$knot.border.color -> knot.border.color
  x$graphAttributes$Knots$knot.border.width -> knot.border.width
  
  # Graph:
  x$graphAttributes$Graph$nNodes -> nNodes
  x$graphAttributes$Graph$weighted -> weighted
  x$graphAttributes$Graph$edgesort -> edgesort
  x$graphAttributes$Graph$scores -> scores
  x$graphAttributes$Graph$scores.range -> scores.range
  x$graphAttributes$Graph$groups -> groups
  x$graphAttributes$Graph$minimum -> minimum
  x$graphAttributes$Graph$maximum -> maximum
  x$graphAttributes$Graph$cut -> cut
  x$graphAttributes$Graph$polygonList -> polygonList
  x$graphAttributes$Graph$mode -> mode
  x$graphAttributes$Graph$color -> color
  
  # Layout:
  x$layout -> layout
  x$layout.orig -> original.layout
  
  # Plot options:
#   x$plotOptions$filetype -> filetype
#   if (missing(filetype)) x$plotOptions$filetype -> filetype
#   if (missing(filename)) x$plotOptions$filename -> filename
#   if (missing(normalize)) x$plotOptions$normalize -> normalize
#   if (missing(plot)) x$plotOptions$plot -> plot
#   if (missing(mar)) x$plotOptions$mar -> mar
#   if (missing(GLratio)) x$plotOptions$GLratio -> GLratio
#   if (missing(legend)) x$plotOptions$legend -> legend
#   if (missing(legend.cex)) x$plotOptions$legend.cex -> legend.cex
#   if (missing(pty)) x$plotOptions$pty -> pty
#   if (missing(XKCD)) x$plotOptions$XKCD -> XKCD
#   if (missing(arrows)) x$plotOptions$arrows -> arrows
#   if (missing(arrowAngle)) x$plotOptions$arrowAngle -> arrowAngle
#   if (missing(open)) x$plotOptions$open -> open
#   if (missing(curvePivot)) x$plotOptions$curvePivot -> curvePivot
#   if (missing(curveShape)) x$plotOptions$curveShape -> curveShape
#   if (missing(curveScale)) x$plotOptions$curveScale -> curveScale
#   if (missing(curvePivotShape)) x$plotOptions$curvePivotShape -> curvePivotShape
#   if (missing(label.scale)) x$plotOptions$label.scale -> label.scale
#   if (missing(label.norm)) x$plotOptions$label.norm -> label.norm
#   if (missing(label.prop)) x$plotOptions$label.prop -> label.prop
#   if (missing(overlay)) x$plotOptions$overlay -> overlay
#   if (missing(details)) x$plotOptions$details -> details
#   if (missing(legend.mode)) x$plotOptions$legend.mode -> legend.mode

x$plotOptions$filetype -> filetype
x$plotOptions$filename -> filename
x$plotOptions$normalize -> normalize
x$plotOptions$plot -> plot
x$plotOptions$mar -> mar
x$plotOptions$GLratio -> GLratio
x$plotOptions$legend -> legend
x$plotOptions$legend.cex -> legend.cex
x$plotOptions$pty -> pty
x$plotOptions$XKCD -> XKCD
x$plotOptions$arrows -> arrows
x$plotOptions$arrowAngle -> arrowAngle
x$plotOptions$open -> open
x$plotOptions$curvePivot -> curvePivot
x$plotOptions$curveShape -> curveShape
x$plotOptions$curveScale -> curveScale
x$plotOptions$curvePivotShape -> curvePivotShape
x$plotOptions$label.scale -> label.scale
x$plotOptions$label.norm -> label.norm
x$plotOptions$label.prop -> label.prop
x$plotOptions$overlay -> overlay
x$plotOptions$details -> details
x$plotOptions$legend.mode -> legend.mode

  x$plotOptions$background -> background
  x$plotOptions$bg -> bg
  x$plotOptions$residuals -> residuals
  x$plotOptions$residScale -> residScale


  x$plotOptions$srt -> srt
  x$plotOptions$gray -> gray
  x$plotOptions$overlaySize -> overlaySize
  x$plotOptions$plotELBG -> plotELBG
  x$plotOptions$alpha -> alpha
  x$plotOptions$width -> width
  x$plotOptions$height -> height
  x$plotOptions$aspect -> aspect
  x$plotOptions$rescale -> rescale
  x$plotOptions$barsAtSide -> barsAtSide
  x$plotOptions$bgres -> bgres
  x$plotOptions$bgcontrol -> bgcontrol
  x$plotOptions$resolution -> res
  x$plotOptions$subpars -> subpars
  x$plotOptions$subplotbg -> subplotbg
  x$plotOptions$title -> title
  x$plotOptions$preExpression -> preExpression
  x$plotOptions$postExpression -> postExpression
  x$plotOptions$usePCH -> usePCH
  x$plotOptions$node.resolution -> node.resolution

  x$plotOptions$noPar -> noPar
  x$plotOptions$meanRange -> meanRange
  
  rm(x)
  
  # Some setup
  vAlpha <- col2rgb(vertex.colors,TRUE)[4,]
  midX=numeric(0)
  midY=numeric(0)
  
  if (length(E$from)>0) 
  {
    plotEdgeLabel <- sapply(1:length(E$from),function(i)(is.character(edge.labels[[i]]) | is.expression(edge.labels[[i]]) |  is.call(edge.labels[[i]])) && !identical(edge.labels[[i]],''))
  } else {
    plotEdgeLabel <- logical(0)
  }
  
  if (!(is.expression(edge.labels) | is.character(edge.labels) | is.list(edge.labels) ))  edge.labels <- as.character(edge.labels)
  
  ### Open device:
  # Start output:
  if (is.function(filetype))
  {
    filetype(width=width, height = height)
    filetype <- ''
  } else {
    if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width,height=height)
    if (filetype=='R') dev.new(rescale="fixed",width=width,height=height)
    # if (filetype=='X11' | filetype=='x11') x11(width=width,height=height)
    if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height,width=width, horizontal=FALSE)
    if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height,width=width)
    if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),units='in',res=res,height=height,width=width)
    if (filetype=='png') png(paste(filename,".png",sep=""),units='in',res=res,height=height,width=width)
    if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),units='in',res=res,height=height,width=width)
    if (filetype=="svg")
    {
#       if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
      if (!requireNamespace("RSVGTipsDevice", quietly = TRUE)) stop("Please install 'RSVGTipsDevice' package first.")
      RSVGTipsDevice::devSVGTips(paste(filename,".svg",sep=""),width=width,height=height,title=filename)
    }
    if (filetype=="tex")
    {
      #   # Special thanks to Charlie Sharpsteen for supplying these tikz codes on stackoverflow.com !!!
      # 	
      # 	if (!suppressPackageStartupMessages(require(tikzDevice,quietly=TRUE))) stop("tikzDevice must be installed to use filetype='tex'")
      # 	opt= c( 
      # 	getOption('tikzLatexPackages'),  
      #     "\\def\\tooltiptarget{\\phantom{\\rule{1mm}{1mm}}}",
      #     "\\newbox\\tempboxa\\setbox\\tempboxa=\\hbox{}\\immediate\\pdfxform\\tempboxa \\edef\\emptyicon{\\the\\pdflastxform}",
      #     "\\newcommand\\tooltip[1]{\\pdfstartlink user{/Subtype /Text/Contents  (#1)/AP <</N \\emptyicon\\space 0 R >>}\\tooltiptarget\\pdfendlink}"
      # 	)
      # 	
      # 	place_PDF_tooltip <- function(x, y, text)
      # 	{
      # 
      # 		# Calculate coordinates
      # 		tikzX <- round(grconvertX(x, to = "device"), 2)
      # 		tikzY <- round(grconvertY(y, to = "device"), 2)
      # 		# Insert node
      # 		tikzAnnotate(paste(
      # 		"\\node at (", tikzX, ",", tikzY, ") ",
      # 		"{\\tooltip{", text, "}};",
      # 		sep = ''
      # 		))
      # 	  invisible()
      # 	}
      # 	
      # 	print("NOTE: Using 'tex' as filetype will take longer to run than other filetypes")
      # 	
      # 	tikzDevice:::tikz(paste(filename,".tex",sep=""), standAlone = standAlone, width=width, height=height, packages=opt)
      
      stop("Tikz device no longer supported due to removal from CRAN. Please see www.sachaepskamp.com/qgraph for a fix")
    }
  }  
  

  ### START PLOT:
  marOrig <- par("mar")
  bgOrig <- par("bg")
  if (plot)
  {
    if (!noPar) par(mar=c(0,0,0,0), bg=background)
    
    plot(1, ann = FALSE, axes = FALSE, xlim = c(-1 - mar[2], 1 + mar[4] + (((legend&is.null(scores))|(filetype=="svg")) * (2+mar[2]+mar[4])/GLratio)), ylim = c(-1 - mar[1] ,1 + mar[3]),type = "n", xaxs = "i", yaxs = "i")
    
    
    #         plot(1, ann = FALSE, axes = FALSE, xlim = c(-1 - mar[2], 1 + mar[4] + (((legend&is.null(scores))) * 2.4/GLratio)), ylim = c(-1 - mar[1] ,1 + mar[3]),type = "n", xaxs = "i", yaxs = "i")
  }
  
  # Run preExpression
  if (!is.null(preExpression))
  {
    eval(parse(text = preExpression))
  }
  
  # if (PlotOpen) 
  # {
  width <- par('pin')[1]
  height <- par('pin')[2]
  
  if (rescale & aspect) {
    l <- original.layout
    # center:
    l[,1] <- l[,1] - mean(range(l[,1]))
    l[,2] <- l[,2] - mean(range(l[,2]))
    
    # Ajust for aspect:
    l[,1] <- l[,1] * min(height/width, 1)
    l[,2] <- l[,2] * min(width/height, 1)
    
    lTemp <- l
    
    if (length(unique(lTemp[,1]))>1)
    {
      l[,1]=(lTemp[,1]-min(lTemp))/(max(lTemp)-min(lTemp))*2-1
    } else l[,1] <- 0
    if (length(unique(lTemp[,2]))>1)
    {
      l[,2]=(lTemp[,2]-min(lTemp))/(max(lTemp)-min(lTemp))*2-1 
    } else l[,2] <- 0
    
    # center again for good measures! (I really have no idea why but whatever):
    l[,1] <- l[,1] - mean(range(l[,1]))
    l[,2] <- l[,2] - mean(range(l[,2]))
    
    rm(lTemp)
    
    
    #     # Equalize white space:
    #     if (diff(range(l[,1])) < 2)
    #     {
    #       l[,1] <- diff(range(l[,1]))/2 + l[,1]
    #     }
    #     if (diff(range(l[,2])) < 2)
    #     {
    #       l[,2] <- (2-diff(range(l[,2])))/2 + l[,2]
    #     }
    
    layout <- l    
  }
  
  # Rescale dims:
  if (pty=='s')
  {
    width=height=min(c(width,height))
  }
  # }  
  
  
  if (legend)
  {
    width <- width * (GLratio/(1+GLratio))
  }
  
  
  # Super cool background:
  
  if (is.logical(bg)) if (bg) {
    
    colarray=array(dim=c(bgres,bgres,length(groups)))
    
    seq=seq(-1.2,1.2,length=bgres+1)
    
    for (G in 1:length(groups)) {
      
      Xg=layout[groups[[G]],1]
      Yg=layout[groups[[G]],2]
      
      for (i in 1:bgres) {
        for (j in 1:bgres) {
          
          Xp=mean(seq[i:(i+1)])
          Yp=mean(seq[j:(j+1)])
          
          colarray[i,j,G]=min(sqrt( (Xp-Xg)^2 + (Yp-Yg)^2)) }}}
    
    colarray=((2.2-colarray)/2.2)^bgcontrol
    
    colarray2=array(dim=c(3,bgres,bgres))
    #   }
    #   
    #   if (is.logical(bg)) if (bg){
    #     
    
    for (i in 1:bgres) {
      for (j in 1:bgres) {
        
        for (C in 1:3) {
          
          colarray2[C,i,j]=min(c(1,max(colarray[i,j,]*(col2rgb(color)[C,]/255))))  }
        
        polygon(c(seq[i],seq[i+1],seq[i+1],seq[i]),c(seq[j],seq[j],seq[j+1],seq[j+1]),
                col=rgb(colarray2[1,i,j],colarray2[2,i,j],colarray2[3,i,j]),border=rgb(colarray2[1,i,j],colarray2[2,i,j],colarray2[3,i,j]))
        
      } }
    
  }      		
  
  # Compute normalizing constant:
  if (isTRUE(normalize))
  {
    normC <- sqrt(sum(par("pin")^2)) / sqrt(7^2 + 7^2)
    vsize <- vsize * normC
    vsize2 <- vsize2 * normC
    edge.width <- edge.width * normC
    border.width <- border.width * normC
    asize <- asize * normC
    edge.label.cex <- edge.label.cex * normC
    
    knot.size <- knot.size * normC
    knot.border.width <- knot.border.width * normC
    
    residScale <- residScale * normC
  }
  
  ## Normalize curve (linear to half of diagonal in user coordinates):
  if (isTRUE(curveScale))
  {
    usr <- par("usr")
    AverageLength <- sqrt(((usr[2]-usr[1]) * (usr[4]-usr[3])) / nNodes)
    EdgeLenghts <- sqrt((layout[E$to,1] - layout[E$from,1])^2 + (layout[E$to,2] - layout[E$from,2])^2)
    curve <- curve * EdgeLenghts /AverageLength
  }
  
  # Create 'omitEdge' vector to make sure bidirectional edges are not plotted.
  if (any(bidirectional))
  {
    omitEdge <- duplicated(srt)&bidirectional
  } else omitEdge <- NULL 
  
  # If images is not NULL, replace subplots with images calls:
  if (!is.null(images))
  {
    images <- gsub("\\\\","/", images)
    if (length(images) == 1) images <- rep(images, nNodes)
    if (is.null(subplots)) subplots <- vector( "list", nNodes)
    
    for (i in seq_along(images))
    {
      if (!is.na(images[i]) && file.exists(images[[i]]))
      {
        if (grepl("\\.jpe?g$",images[i]))
        {
          subplots[[i]] <- parse(text=sprintf('
                                              plot(1,type="n",xlim=0:1,ylim=0:1,axes=FALSE,xlab="",ylab="",bty="n",xaxs="i",yaxs="i")
                                              rasterImage(readJPEG("%s"), 0,0,1,1, interpolate=FALSE)', images[i]))
        } else if (grepl("\\.png$",images[i]))
        {
          subplots[[i]] <- parse(text=sprintf('
                                              plot(1,type="n",xlim=0:1,ylim=0:1,axes=FALSE,xlab="",ylab="",bty="n",xaxs="i",yaxs="i")
                                              rasterImage(readPNG("%s"), 0,0,1,1, interpolate=FALSE)', images[i]))              
        } else warning("Only jpeg and png images supported in 'images'")
      }
    }
  }
  
  # Set non-rectangular/square dge shapes with subplots to square:
  if (!is.null(subplots))
  {
    # Get which nodes become a subplot:
    whichsub <- which(sapply(subplots,function(x)is.expression(x)|is.function(x)))
    
    shape[whichsub][!shape[whichsub]%in%c("square","rectangle")] <- "square"
  }
  
  # Plot edges: 
  if (length(curve)==1) curve=rep(curve,length(edgesort))
  curve[E$from==E$to]=1
  
  # Compute knot placement:
  if (length(knots) > 0)
  {
    knotLayout <- matrix(,max(knots),2)
    for (i in seq_len(max(knots)))
    {
      knotNodes <- c(E$from[knots==i],E$to[knots==i])
      
      # mid X:
      knotLayout[i,1] <- in2usrX(mean(usr2inX(layout[knotNodes,1])))
      knotLayout[i,2] <- in2usrY(mean(usr2inY(layout[knotNodes,2])))
    }
  } else {
    knotLayout <- matrix(,0,0)
  }
  
  
  # For each (sorted from weak to strong) edge:
  for (i in edgesort)
  {
    
    # Only plot if over minimum:
    if (abs(E$weight[i])>minimum & !isTRUE(omitEdge[i]))
    {
      x1=layout[E$from[i],1]
      x2=layout[E$to[i],1]
      y1=layout[E$from[i],2]
      y2=layout[E$to[i],2]
      
      # If not curved, knotted or XKCD plot straigth line instead of spline:
      if (curve[i]==0 & !XKCD & knots[i] == 0)
      {
        # Replace destination to fixed points if specified in edgeConnectPoints:
        if (!is.null(edgeConnectPoints) && !is.na(edgeConnectPoints[i,2]))
        {
          NewPoints <- Cent2Edge(x2,y2,edgeConnectPoints[i,2],vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
          x2 <- NewPoints[1]
          y2 <- NewPoints[2]
        } else {
          
          # Replace destination of edge to edge of node if needed:
          #         if (parallelEdge[i] | is.logical(arrows) | vAlpha[E$to[i]] < 255)
          if (parallelEdge[i] | (isTRUE(arrows) & directed[i]) | vAlpha[E$to[i]] < 255)
          {
            NewPoints <- Cent2Edge(x2,y2,ifelse(residEdge[i],loopRotation[E$to[i]],atan2usr2in(x1-x2,y1-y2) + parallelEdge[i]*parallelAngle[i]),vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
            x2 <- NewPoints[1]
            y2 <- NewPoints[2]
          }
        }
        
        # Replace source to fixed points if specified in edgeConnectPoints:
        if (!is.null(edgeConnectPoints) && !is.na(edgeConnectPoints[i,1]))
        {
          NewPoints <- Cent2Edge(x1,y1,edgeConnectPoints[i,1],vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
          x1 <- NewPoints[1]
          y1 <- NewPoints[2]
        } else {
          
          # Replace source of edge to edge of node if needed:
          if (parallelEdge[i] | plotEdgeLabel[i] || (any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i]) | vAlpha[E$from[i]] < 255)
          {
            
            NewPoints <- Cent2Edge(x1,y1,ifelse(residEdge[i],loopRotation[E$from[i]],atan2usr2in(x2-x1,y2-y1) - parallelEdge[i]*parallelAngle[i]),vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
            x1 <- NewPoints[1]
            y1 <- NewPoints[2]  
            
          }
        }
        
        
        if (plotEdgeLabel[i])
        {
          midX[i] <- ((1-edge.label.position[i])*x1 + edge.label.position[i]*x2)
          midY[i] <- ((1-edge.label.position[i])*y1 + edge.label.position[i]*y2)
        }
        
        
        
        ## Plot edges and arrows:
        drawEdge(c(x1,x2),c(y1,y2),
                 col=edge.color[i],
                 lwd=edge.width[i],
                 arrowlwd=asize[i],
                 lty=lty[i],directed=directed[i],
                 bidirectional=any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i],
                 arrows=arrows,
                 arrowAngle=arrowAngle,
                 open=open)
        #         
        #         lines(c(x1,x2),c(y1,y2),lwd=edge.width[i],col=edge.color[i],lty=lty[i])
        #         if (directed[i])
        #         {
        #           if (!is.logical(arrows))
        #           {
        #             Ax=seq(x1,x2,length=arrows+2)
        #             Ay=seq(y1,y2,length=arrows+2)
        #             for (a in 1:arrows+1)
        #             {
        #               #                   qgraph.arrow(Ax[a],Ay[a],x1,y1,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #               #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #               DrawArrow(Ax[a],Ay[a],atan2usr2in(Ax[a]-x1,Ay[a]-y1),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             }
        #           }
        #           else if (arrows)
        #           {
        #             #                 qgraph.arrow(x2,y2,x1,y1,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #             #                              col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #             DrawArrow(x2,y2,atan2usr2in(x2-x1,y2-y1),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             
        #             if (any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i])
        #             {
        #               #                   qgraph.arrow(x1,y1,x2,y2,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #               #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #               DrawArrow(x1,y1,atan2usr2in(x1-x2,y1-y2),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             }
        #           }
        #         }
      } else {
        if (E$from[i]==E$to[i])
        {
          #               loopX=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2])
          #               spx=c(x1+loopX,x1,x1-loopX)
          #               loopY=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2])
          #               spy=c(y1,y1+loopY,y1)
          #               spl <- spl2 <- xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=FALSE)
          
          spl <- SelfLoop(x1,y1,loopRotation[E$from[i]],vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],residuals,residScale,polygonList,offset=ifelse(XKCD,2,0))
          
        } else 
        {
          #spx <- midx - curve[i] * (y2 - y1)/2
          #spy <- midy + curve[i] * (x2 - x1)/2
          #               curvemid <- Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
          
          if (knots[i]!=0)
          {
            spl <- xspline(c(x1,knotLayout[knots[i],1],x2),c(y1,knotLayout[knots[i],2],y2),0,draw=FALSE)
          } else {     
            #                midx <- (x1 + x2)/2
            #                midy <- (y1 + y2)/2
            
            curvemid <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i]) 
            
            # Add pivots:
            if (is.numeric(curvePivot))
            {
              splShape <- c(curveShape, curvePivotShape, curveShape, curvePivotShape, curveShape)
              
              curveQ1 <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i], q = curvePivot) 
              curveQ2 <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i], q = 1-curvePivot) 
              
              spx <- c(curveQ1[1]  , curvemid[1], curveQ2[1])
              spy <- c(curveQ1[2]  , curvemid[2], curveQ2[2])
            } else 
            {
              splShape <- rep(curveShape,3)
              spx <- curvemid[1]
              spy <- curvemid[2]
            }
            spl=xspline(c(x1,spx,x2),c(y1,spy,y2),splShape,draw=FALSE) 
          }
          
        }	
        if (E$from[i]!=E$to[i])
        {
          recurve <- FALSE
          
          
          # Replace source to fixed points if specified in edgeConnectPoints:
          if (!is.null(edgeConnectPoints) && !is.na(edgeConnectPoints[i,1]))
          {
            NewPoints <- Cent2Edge(x1,y1,edgeConnectPoints[i,1],vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
            x1 <- NewPoints[1]
            y1 <- NewPoints[2]
          } 
          
          # Replace destination to fixed points if specified in edgeConnectPoints:
          if (!is.null(edgeConnectPoints) && !is.na(edgeConnectPoints[i,2]))
          {
            NewPoints <- Cent2Edge(x2,y2,edgeConnectPoints[i,2],vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
            x2 <- NewPoints[1]
            y2 <- NewPoints[2]
            
            recurve <- TRUE
          } 
          
          
          
          # Replace destination of edge to edge of node if needed:
          #           if (is.logical(arrows)| vAlpha[E$to[i]] < 255)
          #           {
          
          if (parallelEdge[i] || isTRUE(arrows) & directed[i]| vAlpha[E$to[i]] < 255 |  vAlpha[E$from[i]] < 255)
          {
            if (is.null(edgeConnectPoints) || is.na(edgeConnectPoints[i,2]))
            {
              NewPoints <- Cent2Edge(x2,y2,ifelse(residEdge[i],loopRotation[E$to[i]],atan2usr2in(spl$x[length(spl$x)-1]-x2,spl$y[length(spl$y)-1]-y2)) + parallelEdge[i]*parallelAngle[i],vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
              x2 <- NewPoints[1]
              y2 <- NewPoints[2]
              recurve <- TRUE
            }
            
            if (is.null(edgeConnectPoints) || is.na(edgeConnectPoints[i,1]))
            {
              NewPoints <- Cent2Edge(x1,y1,ifelse(residEdge[i],loopRotation[E$from[i]],atan2usr2in(spl$x[2]-x1,spl$y[2]-y1)) - parallelEdge[i]*parallelAngle[i],vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,ifelse(XKCD,2,0)), polygonList)
              x1 <- NewPoints[1]
              y1 <- NewPoints[2]
              recurve <- TRUE
            }
          }
          
          
          #           }
          
          if (recurve)
          {
            #             # Update curve if needed:
            #             if (isTRUE(curveScale))
            #             {
            #               usr <- par("usr")
            #               AverageLength <- sqrt(((usr[2]-usr[1]) * (usr[4]-usr[3])) / nNodes)
            #               EdgeLenght <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
            #               curve[i] <- curve[i] * EdgeLenght /AverageLength
            #             }
            
            if (knots[i]!=0)
            {
              spl <- xspline(c(x1,knotLayout[knots[i],1],x2),c(y1,knotLayout[knots[i],2],y2),0,draw=FALSE)
            } else {               
              
              if (residEdge[i])  curvemid <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i]) 
              
              # Add pivots:
              if (is.numeric(curvePivot))
              {
                splShape <- c(curveShape, curvePivotShape, curveShape, curvePivotShape, curveShape)
                
                curvemid <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i]) 
                curveQ1 <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i], q = curvePivot) 
                curveQ2 <- PerpMid(c(x1,y1),c(x2,y2),cex=curve[i], q = 1-curvePivot) 
                
                
                spx <- c(curveQ1[1]  , curvemid[1], curveQ2[1])
                spy <- c(curveQ1[2]  , curvemid[2], curveQ2[2])
              } else 
              {
                spx <- curvemid[1]
                spy <- curvemid[2]
              }
              
              # Check if midpoint is not curved but indented:r
              tans <- atan2(x2-x1,y2-y1) - (atan2(spx-x1,spy-y1) + c(-2*pi,0,2*pi))
              tans <- tans[which.min(abs(tans))]
#               if (sign(atan2(x2-x1,y2-y1) - atan2(spx-x1,spy-y1)) != sign(curve[i]))
              if (sign(tans) != sign(curve[i]))
              {
                spl <- list(x = seq(x1, x2, length=length(spx)+2), y = seq(y1, y2, length=length(spx)+2))
              } else 
              {
                spl=xspline(c(x1,spx,x2),c(y1,spy,y2),splShape,draw=FALSE)  
              }
            }
          }
        }
        
        
        # If XKCD jitter edge:
        if (XKCD)
        {
          jitt <- xkcd_jitter(spl$x,spl$y)
          spl$x[3:(length(spl$x)-3)] <- jitt$x[3:(length(spl$x)-3)]
          spl$y[3:(length(spl$y)-3)] <- jitt$y[3:(length(spl$y)-3)]
        }
        
        # If XKCD extra white edge:
        if (XKCD)
        {
          lines(spl,lwd=edge.width[i]*2,col="white")
        }
        
        if (plotEdgeLabel[i])
        {
          if (E$from[i] != E$to[i] && knots[i] == 0 && edge.label.position[i] == 0.5)
          {
            midX[i] <- curvemid[1]
            midY[i] <- curvemid[2]
          } else {
            midX[i]=spl$x[round(edge.label.position[i]*length(spl$x))]
            midY[i]=spl$y[round(edge.label.position[i]*length(spl$y))] 
          }
        }
        
        
        ### Plot Edges and arrows:
        drawEdge(spl$x,spl$y,
                 col=edge.color[i],
                 lwd=edge.width[i],
                 arrowlwd=asize[i],
                 lty=lty[i],directed=directed[i],
                 bidirectional=any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i],
                 arrows=arrows,
                 arrowAngle=arrowAngle,
                 open=open)
        #         lines(spl,lwd=edge.width[i],col=edge.color[i],lty=lty[i])        
        #         
        #         if (directed[i])
        #         {
        #           if (!is.logical(arrows))
        #           {
        #             Ax=seq(1,length(spl$x),length=arrows+2)
        #             Ay=seq(1,length(spl$y),length=arrows+2)
        #             for (a in 2:(arrows+1))
        #             {
        #               #                   qgraph.arrow(spl$x[Ax[a]+1],spl$y[Ay[a]+1],spl$x[Ax[a]],spl$y[Ay[a]],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #               #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #               
        #               DrawArrow(spl$x[Ax[a]+1],spl$y[Ay[a]+1],atan2usr2in(spl$x[Ax[a]+1]-spl$x[Ax[a]],spl$y[Ay[a]+1]-spl$y[Ay[a]]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             }
        #           }
        #           else if (arrows)
        #           {
        #             #                 qgraph.arrow(spl$x[length(spl$x)],spl$y[length(spl$y)],spl$x[length(spl$x)-1],spl$y[length(spl$y)-1],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #             #                              col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #             DrawArrow(spl$x[length(spl$x)],spl$y[length(spl$y)],atan2usr2in(spl$x[length(spl$x)]-spl$x[length(spl$x)-1],spl$y[length(spl$y)]-spl$y[length(spl$y)-1]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             
        #             if (any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i])
        #             {
        #               #                   qgraph.arrow(spl$x[1],spl$y[1],spl$x[2],spl$y[2],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
        #               #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
        #               DrawArrow(spl$x[1],spl$y[1],atan2usr2in(spl$x[1]-spl$x[2],spl$y[1]-spl$y[2]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
        #             }
        #           }
        #           
        #         }
      }
    } 
  }
  
  
  # Plot knots:
  if (any(knots>0))
  {
    if (length(knot.size)==1) knot.size <- rep(knot.size,length=max(knots))
    if (length(knot.color)==1) knot.color <- rep(knot.color,length=max(knots))
    if (length(knot.borders)==1) knot.borders <- rep(knot.borders,length=max(knots))
    if (length(knot.border.color)==1) knot.border.color <- rep(knot.border.color,length=max(knots))
    if (length(knot.border.color)==1) knot.border.color <- rep(knot.border.width,length=max(knots))
    
    for (i in 1:max(knots)) if (is.na(knot.color[i])) knot.color[i] <- mixCols(edge.color[knots==i])
    
    if (any(knot.borders))
    {
      for (i in 1:max(knots))
      {            
        points(knotLayout[i,1],knotLayout[i,2],cex=knot.size[i],col=knot.color[i],pch=16)
        if (knot.borders[i]) points(knotLayout[i,1],knotLayout[i,2],cex=knot.size[i],col=knot.border.color[i],pch=1) 
      }
    } else points(knotLayout[,1],knotLayout[,2],cex=knot.size,col=knot.color,pch=16)
  }
  
  
  # Edge labels
  if (is.null(ELcolor))
  {
    ELcolor <- edge.color
  }
  
  
  if (!is.logical(edge.labels) & length(edge.labels)>0)
  {
    # Fix midpoints for knots:
    for (i in seq_len(max(knots)))
    {
      midX[knots==i] <- knotLayout[i,1]
      midY[knots==i] <- knotLayout[i,2]
    }
    
    edgesort2 <- edgesort[abs(E$weight[edgesort])>minimum]
    edgesort2 <- edgesort2[!(duplicated(srt[edgesort2,,drop=FALSE])&bidirectional[edgesort2]) & (!duplicated(knots[edgesort2])|knots[edgesort2]==0)]
    
    if (length(edge.label.cex)==1) edge.label.cex <- rep(edge.label.cex,length(E$from))
    
    if (plotELBG)
    {
      for (i in edgesort2)
      {
        #        if (((is.character(edge.labels[[i]]) | is.expression(edge.labels[[i]]) |  is.call(edge.labels[[i]])) && !identical(edge.labels[[i]],'')) || length(edge.labels) == 0)
        #if ((is.character(edge.labels[[i]]) | is.expression(edge.labels[[i]]) |  is.call(edge.labels[[i]])) && !identical(edge.labels[[i]],''))
        if (plotEdgeLabel[i])
        {
          labwd <- strwidth(edge.labels[[i]],cex=edge.label.cex[i])
          labht <- strheight(edge.labels[[i]],cex=edge.label.cex[i])
          polygon(c(midX[i]-labwd/2,midX[i]+labwd/2,midX[i]+labwd/2,midX[i]-labwd/2),
                  c(midY[i]-labht/2,midY[i]-labht/2,midY[i]+labht/2,midY[i]+labht/2),
                  border=NA,
                  col=edge.label.bg[i]) 
        }
      }
    }
    
    if (!is.list(edge.labels))
    {
      text(midX[edgesort2],midY[edgesort2],edge.labels[edgesort2],cex=edge.label.cex[edgesort2],col=ELcolor[edgesort2],
           font = edge.label.font[edgesort2])
    } else {
      for (i in edgesort2)
      {
        text(midX[i],midY[i],edge.labels[[i]],font=edge.label.font[i],cex=edge.label.cex[i],col=ELcolor[i])
      }
    }
  }			
  
  
  #if (nNodes==1) layout=matrix(0,1,2)
  # Plot nodes:
  
  # scale border width:
  #       border.width <- border.width * normC
  
  # If usePCH = NULL, detect if device or resizable R plot:
  if (is.null(usePCH)) usePCH <- grepl("(RStudioGD)|(x11)|(X11)|(quartz)|(windows)",dev.cur(), ignore.case = TRUE)
  
  if (!XKCD)
  {
    # Check if nodes need to be plotted in for loop:
    if (!usePCH || !is.null(subplots) || any(shape=="rectangle") || !all(shape %in% c("circle","square","triangle","diamond")) || any(sapply(bars,length) > 0) & 
        !all(is.na(means)) & !all(is.na(SDs)))
    {
      # Get which nodes become a subplot:
      #           whichsub <- which(sapply(subplots,function(x)is.expression(x)|is.function(x)))
      
      #           # Plot normal nodes:
      #           bordVec <- unlist(lapply(order(vsize*vsize2,decreasing=FALSE),function(x)rep(x,1+borders[x])))
      #           bordVec <- bordVec[!bordVec%in%whichsub]
      #           points(layout[bordVec,],cex=vsize[bordVec],col=ifelse(duplicated(bordVec),bcolor[bordVec],vertex.colors[bordVec]),lwd=border.width,pch=ifelse(duplicated(bordVec),pch2[bordVec],pch1[bordVec]))            
      
      
      for (i in order(vsize*vsize2,decreasing=TRUE))
      {
        x <- layout[i,1]
        y <- layout[i,2]
        
        if (isTRUE(is.expression(subplots[[i]])))
        {
          xOff <- Cent2Edge(x,y,pi/2,vsize[i],vsize2[i],shape[i], offset=0, polygonList=polygonList)[1] - x
          yOff <- Cent2Edge(x,y,0,vsize[i],vsize2[i],shape[i], offset=0, polygonList=polygonList)[2] - y
          
          usr <- par("usr")
          # Plot background:
          rect(max(usr[1],x-xOff),max(usr[3],y-yOff),min(usr[2],x+xOff),min(usr[4],y+yOff),col=subplotbg,border=NA)
          # Plot subplot:
          subplot(eval(subplots[[i]],envir=globalenv()),c(max(usr[1],x-xOff),min(usr[2],x+xOff)), c(max(usr[3],y-yOff),min(usr[4],y+yOff)), pars = subpars)  
          # Plot border:
          if (borders[i]) rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor[i],lwd=border.width[i])
        } else {

          drawNode(x, y, shape[i], vsize[i], vsize2[i], borders[i], vertex.colors[i], bcolor[i], border.width[i], polygonList, bars[[i]], barSide[i], barColor[i], barLength[i], barsAtSide,
                   usePCH = usePCH, resolution = node.resolution, noPar = noPar, bw = bw, density = density[i], angle = angle[i],
                   mean=means[i],SD=SDs[i],meanRange=meanRange)
        }
      }      
    } else {
      
      
      pch1=numeric(0)
      pch2=numeric(0)
      
      for (i in 1:length(shape))
      {
        if (shape[i]=="circle")
        {
          pch1[i]=16
          pch2[i]=1
        }
        if (shape[i]=="square")
        {
          pch1[i]=15
          pch2[i]=0
        }
        if (shape[i]=="triangle")
        {
          pch1[i]=17
          pch2[i]=2
        }
        if (shape[i]=="diamond")
        {
          pch1[i]=18
          pch2[i]=5
        }
        if (!shape[i]%in%c("circle","square","triangle","diamond")) stop(paste("Shape",shape[i],"is not supported"))
      }
      
      
      bordVec <- unlist(lapply(order(vsize,decreasing=FALSE),function(x)rep(x,1+borders[x])))
      points(layout[bordVec,],cex=vsize[bordVec],col=ifelse(duplicated(bordVec),bcolor[bordVec],vertex.colors[bordVec]),lwd=border.width[bordVec],pch=ifelse(duplicated(bordVec),pch2[bordVec],pch1[bordVec]))  
    }
    
    
    
    #         points(layout,cex=vsize,col=vertex.colors,pch=pch1)
    #                 
    #         if (any(borders) & nNodes > 1) points(layout[borders,],cex=vsize[borders],lwd=border.width,pch=pch2[borders],col=bcolor[borders])
    #         
    #         if (any(borders) & nNodes == 1) points(layout,cex=vsize[borders],lwd=border.width,pch=pch2[borders],col=bcolor[borders])
    
  } else {
    
    circ <- seq(0,2*pi,length=100)
    for (i in 1:nNodes)
    {
      pts <- lapply(circ,function(r)Cent2Edge(layout[i,1],layout[i,2],r,vsize[i],vsize2[i],shape[i],0,polygonList))
      mod <- xkcd_jitter(sapply(pts,'[',1),sapply(pts,'[',2),2000)
      
      if (borders[i]) {
        polygon(mod$x,mod$y,border="white",col=NA,lwd=10)
        polygon(mod$x,mod$y,border="black",col=vertex.colors[i],lwd=5)
      } else {
        polygon(mod$x,mod$y,border="white",col=NA,lwd=10)
        polygon(mod$x,mod$y,border=NULL,col=vertex.colors[i],lwd=5)            
      }
    }
  }
  
  
  if (any(labels != ''))
  {
    #         labels=as.character(labels)
    # Vertex label symbols:
    # Set symbol font:
    #     if (is.character(labels))
    #     {
    #       strsplV=strsplit(labels,"")
    #       greekV=logical(0)
    #       for (i in 1:length(strsplV)) 
    #       {
    #         greekV[i]=any(strsplV[[i]]=="*")
    #         labels[i]=paste(strsplV[[i]][which(strsplV[[i]]!="*")],collapse="") 
    #       }
    #       V.font=rep(1,length(E$from))
    #       V.font[greekV]=5
    #     } else V.font <- 1
    
    if (is.null(label.cex)) label.cex <- pmax(1,vsize)
    # Rescale labels:
    if (label.scale)
    {
      VWidths <- sapply(mapply(Cent2Edge,cex=vsize,cex2=vsize2,shape=shape,MoreArgs=list(x=0,y=0,r=pi/2,polygonList=polygonList, noPar = noPar),SIMPLIFY=FALSE),'[',1) * 2
      VHeights <- sapply(mapply(Cent2Edge,cex=vsize,cex2=vsize2,shape=shape,MoreArgs=list(x=0,y=0,r=0,polygonList=polygonList, noPar = noPar),SIMPLIFY=FALSE),'[',2) * 2          
      LWidths <- pmax(sapply(label.cex,function(x)strwidth(label.norm,cex=x)),mapply(strwidth, s=labels, cex=label.cex))
      LHeights <- pmax(sapply(label.cex,function(x)strheight(label.norm,cex=x)),mapply(strheight, s=labels, cex=label.cex))
      
      label.cex <- label.cex * label.prop * pmin(VWidths/LWidths,VHeights/LHeights)
      #           label.cex[nchar(labels)>1]=label.cex[nchar(labels)>1]*2/nchar(labels[nchar(labels)>1],"width")
    }
    
    # Plot labels:
    if (!is.list(labels))
    {
      text(layout[,1],layout[,2],labels,cex=label.cex,col=lcolor,font=label.font)
    } else {
      lcolor <- rep(lcolor,length=nNodes)
      for (i in seq_along(labels))
      {
        text(layout[i,1],layout[i,2],labels[[i]],cex=label.cex[i],col=lcolor[i],font=label.font[i])
      }
    }
  }
  
  if (!is.null(tooltips)) 
  {
    # Set Tooltips:
    for (i in 1:nNodes) 
    {
      if (!is.na(tooltips[i]))
      {
        if (filetype=='svg') RSVGTipsDevice::setSVGShapeToolTip(desc=tooltips[i])
      }
      if (!is.null(SVGtooltips)) if (!is.na(SVGtooltips[i]))
      {
        RSVGTipsDevice::setSVGShapeToolTip(desc=SVGtooltips[i])
      }       
      NodeOutline <- lapply(seq(0,2*pi,length=10),function(r)Cent2Edge(layout[i,1],layout[i,2],r,vsize[i],vsize2[i],shape[i],offset=0,polygonList))
      polygon(sapply(NodeOutline,'[',1),sapply(NodeOutline,'[',2),col="#01010101",border=NA)
      
    }
  }    
  
  
  ### CIRCLES AT END EDGES (RANDOM INTERCEPTS) ###
  if (any(CircleEdgeEnd))
  {
    for (i in which(CircleEdgeEnd))
    {
      if (abs(E$weight[i]) > minimum)
      {
        # Center of destination node:
        x <- layout[E$to[i],1]
        y <- layout[E$to[i],2]
        
        # Edge entry point:
        if (!is.null(edgeConnectPoints) && !is.na(edgeConnectPoints[i,2]))
        {
          edge <- Cent2Edge(x,y,edgeConnectPoints[i,2],vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],offset=0, polygonList)  
        } else {
          r <- atan2usr2in(layout[E$from[i],1] - x, layout[E$from[i],2] - y )
          edge <- Cent2Edge(x,y,r,vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],offset=0, polygonList)  
        }
        
        
        # Size of bal:
        sizeBal <- mean(vsize[E$to[i]],vsize2[E$to[i]]) / 4
        
        # Center of bal:
        r <- atan2usr2in(x-edge[1], y-edge[2] )
        ball <- Cent2Edge(edge[1],edge[2],r,sizeBal,sizeBal,'circle',offset=0, polygonList)
        
        # Draw ball:
        points(ball[1], ball[2], pch = 16, col = edge.color[i], cex = sizeBal)
      }
    }
  }
  
  ### Overlay:
  if (overlay)
  {
    # Transparance in vertex colors:
    num2hex <- function(x)
    {
      hex=unlist(strsplit("0123456789ABCDEF",split=""))
      return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
    }
    
    colHEX <- rgb(t(col2rgb(color)/255))
    
    fillCols <- paste(sapply(strsplit(colHEX,split=""),function(x)paste(x[1:7],collapse="")),num2hex(25),sep="")

    for (i in 1:length(groups)) 
    {
      if (length(groups[[i]]) > 1)
      {
        polygon(ellipse(cov(layout[groups[[i]],,drop=FALSE]),centre=colMeans(layout[groups[[i]],,drop=FALSE]),level=overlaySize),border=color[i],col=fillCols[i]) 
      }
    }
  }
  
  if (is.null(names(groups))) names(groups) <- LETTERS[1:length(groups)]
  
  #if (!legend && filetype=="svg") plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1 ,1 ),type = "n", xaxs = "i", yaxs = "i")
  
  # Plot Legend:
  if (legend)
  {
    if (is.null(scores))
    {
      legend.cex=legend.cex*2
      #plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1 ,1 ),type = "n", xaxs = "i", yaxs = "i")
      
      if (mode=="sig")
      {
        if (legend.mode == "names")
        {
          text(1 + mar[4] ,0, paste(labels,": ",nodeNames,sep="",collapse="\n"), cex=legend.cex, adj = c(0, 0.5)) 
        } else 
        {
          if (length(groups) > 1)
          {
            legend (1 + mar[4] + 0.5 * 2.4/GLratio,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            legend (1 + mar[4] + 0.5 * 2.4/GLratio,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n') 
          }
        }
        
        if (gray)
        {
          legend(1 + mar[4] + 0.5 * 2.4/GLratio,(length(groups) > 1) * -0.5,paste("p <",alpha[length(alpha):1]),
                 col = c(rgb(0.7,0.7,0.7),rgb(0.5,0.5,0.5),rgb(0.3,0.3,0.3),"black")[(5-length(alpha)):4],
                 lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
        } else
        {
          if (any(Pvals < 0))
          {
            legend(1 + mar[4] + 0.25 * 2.4/GLratio,(length(groups) > 1) * -0.5,paste("p <",alpha[length(alpha):1]),
                   col = c("cadetblue1","#6495ED","blue","darkblue")[(5-length(alpha)):4],
                   lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            
            legend(1 + mar[4] + 0.75 * 2.4/GLratio,(length(groups) > 1) * -0.5,paste("p <",alpha[length(alpha):1]),
                   col = c(rgb(1,0.8,0.4) ,"orange","darkorange","darkorange2")[(5-length(alpha)):4],
                   lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            
          } else
          {
            legend(1 + mar[4] + 0.5 * 2.4/GLratio,(length(groups) > 1) * -0.5,paste("p <",alpha[length(alpha):1]),
                   col = c("cadetblue1","#6495ED","blue","darkblue")[(5-length(alpha)):4],
                   lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
          }
        }
      } else
      {

        if (legend.mode == "style2"){
          # Generate names in list:
          LEGENDgroups <- lapply(groups,function(x)paste0(labels[x],": ",nodeNames[x]))
          LEGENDstr <- character(0)
          LEGENDcol <- character(0)
          LEGENDpch <- numeric(0)
          LEGENDtextfont <- numeric(0)
          for (GR in seq_along(groups)){
            LEGENDstr <- c(LEGENDstr,names(groups)[GR],LEGENDgroups[[GR]])
            LEGENDcol <- c(LEGENDcol,rep(color[GR],length(LEGENDgroups[[GR]])+1))
            LEGENDpch <- c(LEGENDpch,16,rep(1,length(LEGENDgroups[[GR]])))
            LEGENDtextfont <- c(LEGENDtextfont,2,rep(1,length(LEGENDgroups[[GR]])))
          }
          
          legend (1.2 + 0.5 * 2.4/GLratio,0,LEGENDstr, col= LEGENDcol ,pch = LEGENDpch, text.font = LEGENDtextfont, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
          
          
        } else if (legend.mode == "style1"){

          # Generate names in list:
          LEGENDgroups <- lapply(groups,function(x)paste0(labels[x],": ",nodeNames[x]))
          LEGENDstr <- character(0)
          LEGENDcol <- character(0)
          LEGENDbord <- character(0)
          LEGENDpch <- numeric(0)
          LEGENDtextfont <- numeric(0)

          getShape <- function(x, border=FALSE){
            sapply(x,function(xx){
              if (xx == "circle"){
                return(16)
              } else if (xx == "square"){
                return(15)
              } else if (xx == "triangle"){
                return(17)
              } else return(16)
            })
          }
            
            
          for (GR in seq_along(groups)){
            LEGENDstr <- c(LEGENDstr,names(groups)[GR],LEGENDgroups[[GR]],"")
            LEGENDcol <- c(LEGENDcol,NA,rep(color[GR],length(LEGENDgroups[[GR]])),NA)
            LEGENDbord <- c(LEGENDbord,NA,bcolor[groups[[GR]]],NA)
            LEGENDpch <- c(LEGENDpch,NA,getShape(shape[groups[[GR]]]),NA)
            LEGENDtextfont <- c(LEGENDtextfont,2,rep(1,length(LEGENDgroups[[GR]])),NA)
          }
          
          legend (1.2 + 0.5 * 2.4/GLratio,0,LEGENDstr, col= LEGENDcol ,pch = LEGENDpch, text.font = LEGENDtextfont, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
          legend (1.2 + 0.5 * 2.4/GLratio,0,LEGENDstr, col= LEGENDbord ,pch = LEGENDpch-15, text.font = LEGENDtextfont, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
          
          
        } else if (legend.mode == "names")
        {
          text(1 + mar[4] ,0, paste(labels,": ",nodeNames,sep="",collapse="\n"), cex=legend.cex, adj = c(0, 0.5)) 
        } else 
        {
          legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
          legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n')   
        }
      }
    }
    if (!is.null(scores))
    {
      plot(1, ann = FALSE, axes = FALSE, xlim = c(-0.5, scores.range[2]-scores.range[1]+8), ylim = c(0.5, length(groups)+2),
           type = "n", xaxs = "i", yaxs = "i")
      
      for (i in 1:length(groups)) {
        
        groupcols="white"
        groupcolors=1-t(col2rgb(color[i])/255)
        c=1
        for (j in (scores.range[1]:scores.range[2]-scores.range[1])/(scores.range[2]-scores.range[1])) 
        {
          groupcols[c]=rgb(1-j*groupcolors)
          c=c+1
        }
        
        for (j in scores.range[1]:scores.range[2]-scores.range[1]) {
          
          polygon(c(j,j,j+1,j+1),c(i+0.05,i+0.95,i+0.95,i+0.05),col=groupcols[j+1],border=bcolor[i],lwd=2)
          
        } 
        text(j+1.5,i+0.5,names(groups)[i],pos=4)
      }
      
      for (i in scores.range[1]:scores.range[2]-scores.range[1]) text(i+0.5,length(groups)+1.5,i+scores.range[1])
      
      
    }
  }
  
  # Plot details:
  if (details & weighted)
  {
    if (cut != 0) text(0,-1.1,paste("Cutoff:",round(cut,2)),cex=0.6)
    if (minimum != 0) text(-1,-1.1,paste("Minimum:",round(minimum,2)),pos=4,cex=0.6)
    text(1,-1.1,paste("Maximum:",round(maximum,2)),pos=2,cex=0.6)
  }
  
  # plot title:
  if (!is.null(title))
  {
    addTitle(title)
  }
  
  # Run postExpression
  if (!is.null(postExpression))
  {
    eval(parse(text = postExpression))
  }
  
  if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff','tex')) 
  {
    message(paste("Output stored in ",getwd(),"/",filename,".",filetype,sep=""))
    dev.off()
  }
  if (!noPar) par(mar=marOrig, bg=bgOrig)
  
}