# Main qgraph function

qgraph <- function( input, ... )
{
  
  if ("qgraph"%in%class(input)) arguments <- list(...,input) else arguments=list(...)
  
  if (isTRUE(arguments[['gui']]) | isTRUE(arguments[['GUI']])) 
  {
    arguments$gui <- arguments$GUI <- FALSE
    return(invisible(do.call(qgraph.gui,c(list(input=input),arguments))))
  }
  
  if(!is.null(arguments$adj))
  {
    stop("'adj' argument is no longer supported. Please use 'input'")
  }
  
  # S3 object methods:
  if (any(class(input)=="factanal") )
  {
    qgraph.efa(input,...)
  } else if (any(class(input)=="principal") )
  {
    qgraph.pca(input,...)
  } else if (any(class(input)=="lavaan"))
  {
    qgraph.lavaan(input,edge.labels=TRUE,include=8,filetype="",...)
  } else if (any(class(input)=="sem"))
  {
    qgraph.sem(input,edge.labels=TRUE,include=6,filetype="",...)
  } else if (any(class(input)=="loadings"))
  {
    qgraph.loadings(input,...)
  }  else if (any(class(input)=="semmod"))
  {
    qgraph.semModel(input,...)
  } else {
    
    #     if (length(arguments)>0)
    #     {
    #       for (i in 1:length(arguments))
    #       {
    #         if ("qgraph"%in%class(arguments[[i]])) 
    #         {
    #           if (!is.null(names(arguments[[i]])))
    #           {
    #             for (j in 1:length(arguments[[i]]))
    #             {
    #               if (!(names(arguments[[i]])[j]%in%names(arguments)))
    #               {
    #                 arguments[length(arguments)+1]=arguments[[i]][j]
    #                 names(arguments)[length(arguments)]=names(arguments[[i]])[j]
    #               }
    #             }
    #           }
    #         }
    #       }
    #     }
    
    # Import arguments:
    if (length(arguments) > 0) arguments <- getArgs(arguments)
    
    # Import default arguments:
    def <- options("qgraph")
    if (!is.null(def$qgraph)) class(def$qgraph) <- "qgraph"
    if (any(sapply(def,function(x)!is.null(x))))
    {
      arguments <- getArgs(c(arguments,def))
    }
    
    if (!is.null(arguments$qgraphEdgelist)&"qgraph"%in%class(input)) 
    {
      input <- cbind(arguments$qgraphEdgelist$from,arguments$qgraphEdgelist$to,arguments$qgraphEdgelist$weight)
      if (is.null(arguments$directed)) arguments$directed <- arguments$qgraphEdgelist$directed
      if (is.null(arguments$bidirectional)) arguments$bidirectional <- arguments$qgraphEdgelist$bidir
    }
    
    if (class(input) %in% c("graphNEL","pcAlgo"))
    {
      if (class(input) == "pcAlgo") graphNEL <- input@graph else graphNEL <- input
      arguments$bidirectional <- TRUE
      arguments$labels <- graphNEL@nodes
      weights <- sapply(graphNEL@edgeData@data,'[[','weight')
      
      input <- laply(strsplit(names(weights),split="\\|"),'[',c(1,2))
      input <- apply(input,2,as.numeric)
      if (any(weights!=1)) input <- cbind(input,weights)
    }
    
    input <- as.matrix(input)
    
    # Set mode:
    sigSign <- FALSE
    if(is.null(arguments[['graph']])) graph="association" else graph=arguments[['graph']]
    if (graph %in% c("sig2","significance2"))
    {
      graph <- "sig"
      sigSign <- TRUE
    }
    if (graph %in% c("sig","significance"))
    {
      if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?") 
      arguments[['mode']] <- "sig"
    }
    if(is.null(arguments[['alpha']])) alpha <- c(0.0001,0.001,0.01,0.05) else alpha <- arguments[['alpha']]
    if (length(alpha) > 4) stop("`alpha' can not have length > 4")
    
    if(is.null(arguments[['mode']])) mode <- "strength" else mode <- arguments[['mode']]
    if(is.null(arguments$sigScale)) sigScale <- function(x)0.7*(1-x)^(log(0.4/0.7,1-0.05)) else sigScale <- arguments$sigScale
    if (!mode%in%c("strength","sig","direct")) stop("Mode must be 'direct', 'sig' or 'strength'")	
    if(is.null(arguments$bonf)) bonf=FALSE else bonf=arguments$bonf
    if(is.null(arguments$OmitInsig)) OmitInsig=FALSE else OmitInsig <- arguments$OmitInsig
    # Settings for the edgelist
    if(is.null(arguments$edgelist)) 
    {
      if (nrow(input)!=ncol(input)) edgelist=TRUE else edgelist=FALSE 
    } else edgelist=arguments$edgelist
    if(is.null(arguments$labels))
    {
      labels <- TRUE
      if (!edgelist && !is.null(colnames(input)) && !is.null(rownames(input)))
      {
        if (nrow(input) <= 20 & all(colnames(input)==rownames(input)))
        {
          labels <- abbreviate(colnames(input),3)
        }
      }
    } else labels <- arguments$labels
    if(is.null(arguments[['label.prop']])) label.prop <- 0.9 else label.prop <- arguments[['label.prop']]
    if(is.null(arguments[['label.norm']])) label.norm <- "OOO" else label.norm <- arguments[['label.norm']]
    if(is.null(arguments[['label.cex']])) label.cex <- NULL else label.cex <- arguments[['label.cex']]
    
    if(is.null(arguments[['subplots']])) subplots <- NULL else subplots <- arguments[['subplots']]
    if(is.null(arguments[['images']])) images <- NULL else images <- arguments[['images']]
    
    # Knots:
    if(is.null(arguments[['knots']])) knots <- list() else knots <- arguments[['knots']]
    if(is.null(arguments[['knot.size']])) knot.size <- 1 else knot.size <- arguments[['knot.size']]
    if(is.null(arguments[['knot.color']])) knot.color <- NA else knot.color <- arguments[['knot.color']]
    if(is.null(arguments[['knot.borders']])) knot.borders <- FALSE else knot.borders <- arguments[['knot.borders']]
    if(is.null(arguments[['knot.border.color']])) knot.border.color <- "black" else knot.border.color <- arguments[['knot.border.color']]
    if(is.null(arguments[['knot.border.width']])) knot.border.width <- 1 else knot.border.width <- arguments[['knot.border.width']]
    
    if (edgelist)
    {
      if (is.character(input))
      {
        if(!is.logical(labels)) allNodes <- labels else allNodes <- unique(c(input[,1:2]))
        input[,1:2] <- match(input[,1:2],allNodes)
        input <- apply(input,2,as.numeric)
        if (is.logical(labels) && labels) labels <- allNodes
      }
    }
    
    if(is.null(arguments$nNodes)) 
    {
      if (edgelist)
      {
        if (!is.logical(labels)) nNodes <- length(labels) else nNodes <- max(c(input[,1:2])) 
      } else nNodes=nrow(input)
    } else nNodes=arguments$nNodes
    
    # Default for fact cut and groups
    if (graph=="factorial") fact=TRUE else fact=FALSE
    if (fact & edgelist) stop('Factorial graph needs a correlation matrix')
    if (graph=="concentration") partial=TRUE else partial=FALSE
    if(is.null(arguments$cut)) 
    {
      if (nNodes<50) cut=0 
      if (nNodes>=50 | fact) cut=0.3
      if (mode=="sig") cut <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)-1]),sigScale(alpha[length(alpha)]))
    } else if (mode != "sig") cut <- arguments$cut else cut <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)-1]),sigScale(alpha[length(alpha)]))
    
    if(is.null(arguments$groups)) groups=NULL else groups=arguments$groups
    
    if (is.factor(groups) | is.character(groups)) groups <- tapply(1:length(groups),groups,function(x)x)
    
    # Factorial graph:
    if(is.null(arguments$nfact))
    {
      nfact=NULL
    } else nfact=arguments$nfact
    
    if (fact)
    {
      if (is.null(nfact)) 
      {
        if (is.null(groups)) nfact=sum(eigen(input)$values>1) else nfact=length(groups)
      }
      
      loadings=loadings(factanal(factors=nfact,covmat=input,rotation="promax"))
      
      loadings=loadings[1:nrow(loadings),1:ncol(loadings)]
      
      loadings[loadings<cut]=0
      loadings[loadings>=cut]=1
      
      input=(loadings%*%t(loadings)>0)*1
      
      diag(input)=0
    }
    
    
    # SET DEFAULT ARGUMENTS:
    # General arguments:
    if(is.null(arguments$DoNotPlot)) DoNotPlot=FALSE else DoNotPlot=arguments$DoNotPlot
    if(is.null(arguments[['layout']])) layout=NULL else layout=arguments[['layout']]
    if(is.null(arguments$maximum)) maximum=0 else maximum=arguments$maximum
    if(is.null(arguments$minimum))
    {
      if (nNodes<50)  minimum=0
      if (nNodes>=50)  minimum=0.1
      if (mode=="sig") minimum <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)]),0)
    } else if (mode!="sig") minimum=arguments$minimum else minimum <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)]),0)
    if(is.null(arguments$weighted)) weighted=NULL else weighted=arguments$weighted
    if(is.null(arguments$rescale)) rescale=TRUE else rescale=arguments$rescale
    if(is.null(arguments[['edge.labels']])) edge.labels=FALSE else edge.labels=arguments[['edge.labels']]
    if(is.null(arguments[['edge.label.bg']])) edge.label.bg=TRUE else edge.label.bg=arguments[['edge.label.bg']]
    if (identical(FALSE,edge.label.bg)) plotELBG <- FALSE else plotELBG <- TRUE
    
    if(is.null(arguments[['posCol']])) posCol <- c("#009900","darkgreen") else posCol <- arguments[['posCol']]
    if (length(posCol)==1) posCol <- rep(posCol,2)
    if (length(posCol)!=2) stop("'posCol' must be of length 1 or 2.")
    
    if(is.null(arguments[['negCol']])) negCol <- c("#BF0000","red") else negCol <- arguments[['negCol']]
    if (length(negCol)==1) negCol <- rep(negCol,2)
    if (length(negCol)!=2) stop("'negCol' must be of length 1 or 2.")
    
    if(is.null(arguments[['unCol']])) unCol <- "#808080" else unCol <- arguments[['unCol']] 
    
    if(is.null(arguments[['colFactor']])) colFactor <- 1 else colFactor <- arguments[['colFactor']]
    
    if(is.null(arguments[['edge.color']])) edge.color <- NULL else edge.color=arguments[['edge.color']]
    if(is.null(arguments[['edge.label.cex']])) edge.label.cex=1 else edge.label.cex=arguments[['edge.label.cex']]
    if(is.null(arguments$directed))
    {
      if (edgelist) directed=TRUE else directed=NULL 
    } else directed=arguments$directed
    if(is.null(arguments$legend))
    {
      if (!is.null(groups) & !is.null(names(groups))) legend=TRUE else legend=FALSE
    } else legend=arguments$legend
    if (is.null(groups)) legend <- FALSE
    if(is.null(arguments$plot)) plot=TRUE else plot=arguments$plot
    if(is.null(arguments$rotation)) rotation=NULL else rotation=arguments$rotation
    if(is.null(arguments$layout.control)) layout.control=0.5 else layout.control=arguments$layout.control
    if(is.null(arguments$layout.par)) layout.par=list() else layout.par=arguments$layout.par
    if(is.null(arguments$details)) details=FALSE else details=arguments$details
    
    
    # Output arguments:
    if(is.null(arguments$bg)) bg <- FALSE else bg <- arguments$bg
    
    if(is.null(arguments[['edge.label.color']])) ELcolor <- NULL else ELcolor <- arguments[['edge.label.color']]
    
    if(is.null(arguments[['border.color']])) {
      if(is.null(arguments[['border.colors']])) bcolor <- NULL else bcolor <- arguments[['border.colors']]
    } else bcolor <- arguments[['border.color']]
    
    if(is.null(arguments[['border.width']])) border.width <- 1 else border.width <- arguments[['border.width']]
    #if (!DoNotPlot & !is.null(dev.list()[dev.cur()]))
    #{
    #	par(mar=c(0,0,0,0), bg=background)
    #	if (plot)
    #	{
    #		plot(1, ann = FALSE, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2 ,1.2),type = "n", xaxs = "i", yaxs = "i")
    #		plot <- FALSE
    #	}
    #}
    
    PlotOpen <- !is.null(dev.list()[dev.cur()])
    
    if(is.null(arguments$filetype)) filetype="default" else filetype=arguments$filetype
    if(is.null(arguments$filename)) filename="qgraph" else filename=arguments$filename
    if(is.null(arguments$width)) width <- 7 else width <- arguments[['width']]
    if(is.null(arguments$height)) height <- 7 else height <- arguments[['height']]
    if(is.null(arguments$pty)) pty='m' else pty=arguments$pty
    if(is.null(arguments$res)) res=320 else res=arguments$res
    if(is.null(arguments[['normalize']])) normalize <- TRUE else normalize <- arguments[['normalize']]
    
    # Graphical arguments
    #     defNodeSize <- max((-1/72)*(nNodes)+5.35,1) ### Default node size, used as standard unit.
    if(is.null(arguments[['mar']])) mar <- c(3,3,3,3)/10 else mar <- arguments[["mar"]]/10
    if(is.null(arguments[['vsize']])) 
    {
      vsize <- max((-1/72)*(nNodes)+5.35,1)
      if(is.null(arguments[['vsize2']])) vsize2 <- vsize else vsize2 <- vsize * arguments[['vsize2']]
    } else {
      vsize <- arguments[['vsize']]
      if(is.null(arguments[['vsize2']])) vsize2 <- vsize else vsize2 <- arguments[['vsize2']]
    }
    if(is.null(arguments$color)) color=NULL else color=arguments$color
    
    if(is.null(arguments[['gray']])) gray <- FALSE else gray <- arguments[['gray']]
    
    if (gray) posCol <- negCol <- c("gray10","black")
    
    if(is.null(arguments$bgcontrol)) bgcontrol=6 else bgcontrol=arguments$bgcontrol
    if(is.null(arguments$bgres)) bgres=100 else bgres=arguments$bgres
    if(is.null(arguments[['trans',exact=FALSE]])) transparency <- NULL else transparency <- arguments[['trans',exact=FALSE]]
    if (is.null(transparency))
    {
      if (isTRUE(bg)) transparency <- TRUE else transparency <- FALSE
    }
    if(is.null(arguments[['fade']])) fade <- TRUE else fade <- FALSE
    if(is.null(arguments[['loop']])) loop=1 else loop=arguments[['loop']]
    if(is.null(arguments[['loopRotation']]))
    {
      loopRotation <- 0
      DefLoopRot <- TRUE
    } else {
      loopRotation=arguments[['loopRotation']]
      DefLoopRot <- FALSE
    }
    
    if(is.null(arguments[['residuals']])) residuals=FALSE else residuals=arguments[['residuals']]
    if(is.null(arguments[['residScale']])) residScale=1 else residScale=arguments[['residScale']]
    if(is.null(arguments[['residEdge']])) residEdge=FALSE else residEdge=arguments[['residEdge']]
    if(is.null(arguments[['loopAngle']])) loopangle=pi/2 else loopAngle=arguments[['loopAngle']]
    if(is.null(arguments$legend.cex)) legend.cex=0.6 else legend.cex=arguments$legend.cex
    if(is.null(arguments$borders)) borders=TRUE else borders=arguments$borders
    if(is.null(arguments$shape)) shape="circle" else shape=arguments$shape
    if(is.null(arguments$label.scale)) label.scale=TRUE else label.scale=arguments$label.scale
    if(is.null(arguments$scores)) scores=NULL else scores=arguments$scores
    if(is.null(arguments$scores.range)) scores.range=NULL else scores.range=arguments$scores.range
    if(is.null(arguments$lty)) lty=NULL else lty=arguments$lty
    if(is.null(arguments$vTrans)) vTrans=255 else vTrans=arguments$vTrans
    if(is.null(arguments[['overlay']])) overlay <- FALSE else overlay <- arguments[['overlay']]
    if(is.null(arguments[['overlaySize']])) overlaySize <- 0.5 else overlaySize <- arguments[['overlaySize']]
    if(is.null(arguments[['GLratio']])) GLratio <- 2.5 else GLratio <- arguments[['GLratio']]
    if(is.null(arguments$layoutScale)) layoutScale <- 1 else layoutScale <- arguments$layoutScale
    if(is.null(arguments[['layoutOffset']])) layoutOffset <- 0 else layoutOffset <- arguments[['layoutOffset']]
    
    # Aspect ratio:
    if(is.null(arguments[['aspect']])) aspect=FALSE else aspect=arguments[['aspect']]
    
    # Arguments for directed graphs:
    if(is.null(arguments[['curveDefault']])) curveDefault <- 1 else curveDefault <- arguments[['curveDefault']]
    if(is.null(arguments[['curve']]))
    {
      curve <- NA 
    } else {
      curve <- arguments[['curve']]
      if (length(curve)==1) 
      {
        curveDefault <- curve
        curve <- NA
      }
    }
    if(is.null(arguments[['curveAll']])) curveAll <- FALSE else curveAll <- arguments[['curveAll']]
    if (curveAll)
    {
      curve[is.na(curve)] <- curveDefault
    }
    if(is.null(arguments$arrows)) arrows=TRUE else arrows=arguments$arrows
    #     asize=asize*2.4/height
    if(is.null(arguments$open)) open=FALSE else open=arguments$open
    if(is.null(arguments$bidirectional)) bidirectional=FALSE else bidirectional=arguments$bidirectional
    
    # Arguments for SVG pictures:
    if(is.null(arguments$tooltips)) tooltips=NULL else tooltips=arguments$tooltips
    if(is.null(arguments$SVGtooltips)) SVGtooltips=NULL else SVGtooltips=arguments$SVGtooltips
    if(is.null(arguments$hyperlinks)) hyperlinks=NULL else hyperlinks=arguments$hyperlinks
    
    # Arguments for TEX:
    if(is.null(arguments$standAlone)) standAlone=TRUE else standAlone=arguments$standAlone
    
    ### EASTER EGGS ###
    if(is.null(arguments[['XKCD']])) XKCD <- FALSE else XKCD <- TRUE
    
    # Legend setting 1
    if (is.null(legend))
    {
      if (is.null(groups)) legend=FALSE else legend=TRUE
    }
    #     #if ((legend & filetype!='pdf' & filetype!='eps') | filetype=="svg")
    #     if (legend | (filetype=="svg" & !is.null(tooltips)))
    #     {
    #       width=width*(1+(1/GLratio))
    #     }
    
    if (!DoNotPlot)
    {
      
      # Start output:
      if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width,height=height)
      if (filetype=='R') dev.new(rescale="fixed",width=width,height=height)
      if (filetype=='X11' | filetype=='x11') x11(width=width,height=height)
      if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height,width=width, horizontal=FALSE)
      if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height,width=width)
      if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),units='in',res=res,height=height,width=width)
      if (filetype=='png') png(paste(filename,".png",sep=""),units='in',res=res,height=height,width=width)
      if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),units='in',res=res,height=height,width=width)
      if (filetype=="svg")
      {
        if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
        require("RSVGTipsDevice")
        devSVGTips(paste(filename,".svg",sep=""),width=width,height=height,title=filename)
      }
      if (filetype=="tex")
      {
        # 	# Special thanks to Charlie Sharpsteen for supplying these tikz codes on stackoverflow.com !!!
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
    #if (!filetype%in%c('pdf','png','jpg','jpeg','svg','R','eps','tiff')) warning(paste("File type",filetype,"is not supported")) 
    
    
    # Specify background:
    background <- par("bg")
    if (isColor(bg)) background <- bg
    # Remove alpha:
    background <- col2rgb(background)
    background <- rgb(background[1],background[2],background[3],maxColorValue=255)
    
    if (isTRUE(edge.label.bg)) edge.label.bg <- background
    if(is.null(arguments[['label.color']])) {
      if(is.null(arguments$lcolor)) lcolor <- ifelse(mean(col2rgb(background)/255) > 0.5,"black","white") else lcolor <- arguments$lcolor
    } else lcolor <- arguments[['label.color']]
    
    # Legend setting 2
    if (legend & !is.null(scores))
    {
      layout(t(1:2),widths=c(GLratio,1))
    }
    
    # Weighted settings:
    if (is.null(weighted))
    {
      if (edgelist)
      {
        if (ncol(input)==2) weighted=FALSE else weighted=TRUE
      }
      if (!edgelist)
      {
        if (length(unique(c(input)))>2) weighted=TRUE else weighted=FALSE
      }
    }		
    if (!weighted) cut=0
    
    # par settings:
    #parOrig <- par(no.readonly=TRUE)
    par(pty=pty)
    
    if (!edgelist)
    {
      if (!is.logical(directed)) if (is.null(directed))
      {
        if (!all(input==t(input))) directed=TRUE else directed=FALSE
      }
    }
    
    
    # Set default edge width:
    if(is.null(arguments[["esize"]])) 
    {
      if (weighted)
      {
        esize <- max((-1/72)*(nNodes)+5.35,1) 
      } else {
        esize <- 2
      }
      if (any(directed)) esize <- max(esize/2,1)
    } else esize <- arguments$esize
    
    # asize default:
    if(is.null(arguments[["asize"]]))
    {
      #       asize <- max((-1/10)*(nNodes)+4,1)
      asize <- ifelse(nNodes>10,2,3)
    } else asize <- arguments[["asize"]]
    
    ## arrowAngle default:
    if(is.null(arguments[["arrowAngle"]])) 
    {
      if (weighted) arrowAngle <- pi/4 else arrowAngle <- pi/8
    } else {
      arrowAngle <- arguments[["arrowAngle"]]
    }
    
    ## diag default:
    if(is.null(arguments[['diag']])) 
    {
      if (edgelist) diag <- FALSE  else diag <- length(unique(diag(input))) > 1
    } else { 
      diag <- arguments$diag
    }
    
    # Partial graph:
    if (partial) 
    {
      if (edgelist) stop("Concentration graph requires correlation matrix")
      mi=solve(input)
      for (i in 1:nrow(input)) 
      {
        for (j in 1:nrow(input)) 
        {
          input[i,j]=-1*mi[i,j]/sqrt(mi[i,i]*mi[j,j]) 
        }
      } 
      input=round(input,7) 
    }
    
    # Diag:
    diagCols=FALSE
    diagWeights=0
    if (is.character(diag)) 
    {
      if (diag=="col" & !edgelist)
      {
        diagWeights=diag(input)
        diagCols=TRUE
        diag=FALSE
      }
    }
    if (is.numeric(diag))
    {
      if (length(diag)==1) diag=rep(diag,nNodes)
      if (length(diag)!=nNodes) stop("Numerical assignment of the 'diag' argument must be if length equal to the number of nodes")
      diagWeights=diag
      diagCols=TRUE
      diag=FALSE
    }
    if (is.logical(diag)) if (!diag & !edgelist) diag(input)=0
    
    # CREATE EDGELIST:
    
    E <- list()
    
    # Remove nonfinite weights:
    if (any(!is.finite(input)))
    {
      input[!is.finite(input)] <- 0
      warning("Non-finite weights are omitted")
    }
    
    if (edgelist)
    {
      E$from=input[,1]
      E$to=input[,2]
      if (ncol(input)>2) E$weight=input[,3] else E$weight=rep(1,length(E$from))
      if (length(directed)==1) directed=rep(directed,length(E$from))
      if (graph %in% c("sig","significance"))
      {
        if (sigSign)
        {
          E$weight <- sign(E$weight) * fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
        } else E$weight <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
      }
      if (bonf)
      {
        if (mode=="sig") 
        {
          E$weight <- E$weight * length(E$weight)
          E$weight[E$weight > 1] <- 1
          E$weight[E$weight < -1] <- -1
        } else warning("Bonferonni correction is only applied if mode='sig'")
      }
      if (mode=="sig" & any(E$weight < -1 | E$weight > 1))
      {
        warning("Weights under -1 set to -1 and weights over 1 set to 1")
        E$weight[E$weight< -1] <- -1
        E$weight[E$weight>1] <- 1
      }
      
      if (mode=="sig") 
      {
        Pvals <- E$weight
        E$weight <- sign(E$weight) * sigScale(abs(E$weight))
      }
      if (OmitInsig)
      {
        if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?")
        if (mode != "sig") Pvals <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
        E$weight[abs(Pvals) > alpha[length(alpha)]] <- 0
      }
      
    } else
    {
      if (is.matrix(directed))
      {
        incl <- directed|upper.tri(input,diag=TRUE)
      } else
      {
        if (length(directed)>1) 
        {
          stop("'directed' must be TRUE or FALSE or a matrix containing TRUE or FALSE for each element of the input matrix") 
        } else
        { 
          if (directed)
          {
            incl <- matrix(TRUE,nNodes,nNodes)
          } else
          {
            if (all(input==t(input))) 
            {
              
              incl <- upper.tri(input,diag=TRUE)
            } else 
            {
              incl <- matrix(TRUE,nNodes,nNodes)
            }
          }  
          directed <- matrix(directed,nNodes,nNodes)
        }
      }
      directed <- directed[incl]
      
      E$from=numeric(0)
      E$to=numeric(0)
      E$weight=numeric(0)
      
      E$from=rep(1:nrow(input),times=nrow(input))
      E$to=rep(1:nrow(input),each=nrow(input))
      E$weight=c(input)
      
      
      E$from <- E$from[c(incl)]
      E$to <- E$to[c(incl)]
      E$weight <- E$weight[c(incl)]
      if (graph %in% c("sig","significance"))
      {
        if (sigSign)
        {
          E$weight <- sign(E$weight) * fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
        } else E$weight <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
      }
      if (bonf)
      {
        if (mode=="sig") 
        {
          E$weight <- E$weight * length(E$weight)
          E$weight[E$weight > 1] <- 1
          E$weight[E$weight < -1] <- -1
        } else warning("Bonferonni correction is only applied if mode='sig'")
      }
      if (mode=="sig" & any(E$weight < -1 | E$weight > 1))
      {
        warning("Weights under -1 inputusted to -1 and weights over 1 inputusted to 1")
        E$weight[E$weight < -1] <- -1
        E$weight[E$weight > 1] <- 1
      }
      
      if (mode=="sig") 
      {
        Pvals <- E$weight
        E$weight <- sign(E$weight) * sigScale(abs(E$weight))
      }
      
      if (OmitInsig)
      {
        if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?")
        if (mode != "sig") Pvals <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
        E$weight[abs(Pvals) > alpha[length(alpha)]] <- 0
      }	
      if (is.list(knots))
      {
        knotList <- knots
        knots <- matrix(0,nNodes,nNodes)
        for (k in seq_along(knotList))
        {
          knots[knotList[[k]]] <- k
        }
        # If undirected, symmetrize:
        if (all(incl[upper.tri(incl,diag=TRUE)]) & !any(incl[lower.tri(incl)]))
        {
          knots <- pmax(knots,t(knots))
        }
      }
      if (is.matrix(knots))
      {
        knots <- knots[c(incl)]
        knots <- knots[E$weight!=0]
      }
      if (is.matrix(curve))
      {
        curve <- curve[c(incl)]
        curve <- curve[E$weight!=0]
      }
      if (is.matrix(bidirectional))
      {
        bidirectional <- bidirectional[c(incl)]
        bidirectional <- bidirectional[E$weight!=0]
      }
      if (is.matrix(residEdge))
      {
        residEdge <- residEdge[c(incl)]
        residEdge <- residEdge[E$weight!=0]
      }
      if (is.matrix(edge.labels))
      {
        edge.labels <- edge.labels[c(incl)]
        edge.labels <- edge.labels[E$weight!=0]
      }
      if (is.matrix(edge.color))
      {
        edge.color <- edge.color[c(incl)]
        edge.color <- edge.color[E$weight!=0]
      }
      if (is.matrix(edge.label.bg))
      {
        edge.label.bg <- edge.label.bg[c(incl)]
        edge.label.bg <- edge.label.bg[E$weight!=0]
      }
      if (!is.null(ELcolor))
      {
        if (is.matrix(ELcolor))
        {
          ELcolor <- ELcolor[c(incl)]
          ELcolor <- ELcolor[E$weight!=0]
        }      
      }
      
      if (!is.null(edge.color)) if (length(edge.color) == length(E$weight)) edge.color <- edge.color[E$weight!=0]
      
      if (is.matrix(lty))
      {
        lty <- lty[c(incl)]
        lty <- lty[E$weight!=0]
      }
    }	
    keep <- E$weight!=0
    
    if (length(loopRotation)==1) loopRotation <- rep(loopRotation,nNodes)
    
    if (length(directed)==1) 
    {
      directed <- rep(directed,length(E$from))
    }
    directed <- directed[keep]
    
    if (!is.null(edge.color)) 
    {
      edge.color <- rep(edge.color,length=length(E$from))
      if (length(edge.color) != length(keep)) stop("'edge.color' is wrong length")
      edge.color <- edge.color[keep]
    }
    
    if (!is.logical(edge.labels))
    {
      edge.labels <- rep(edge.labels,length=length(E$from))
    }
    
    #     if (is.logical(edge.label.bg))
    #     {
    #       edge.label.bg <- "white"
    #     }
    if (length(edge.label.bg) == 1) edge.label.bg <- rep(edge.label.bg,length(E$from))
    if (length(edge.label.bg) != length(keep)) stop("'edge.label.bg' is wrong length")
    if (length(edge.label.bg)==length(keep)) edge.label.bg <- edge.label.bg[keep]
    
    if (!is.null(ELcolor))
    {
      ELcolor <- rep(ELcolor,length = length(E$from))
      ELcolor <- ELcolor[keep]    
    }
    
    
    if (is.list(knots))
    {
      knotList <- knots
      knots <- rep(0,length(E$from))
      for (k in seq_along(knotList))
      {
        knots[knotList[[k]]] <- k
      }
    }
    if (length(knots)==length(keep)) knots <- knots[keep]
    
    if (length(bidirectional)==1) 
    {
      bidirectional <- rep(bidirectional,length(E$from))
    }
    if (length(bidirectional)==length(keep)) bidirectional <- bidirectional[keep]
    if (length(residEdge)==1) 
    {
      residEdge <- rep(residEdge,length(E$from))
    }
    if (length(residEdge)==length(keep)) residEdge <- residEdge[keep]    
    
    if (!is.logical(edge.labels))
    {
      if (length(edge.labels)==length(keep))
      {
        edge.labels <- edge.labels[keep]
      }
    }
    
    E$from=E$from[keep]
    E$to=E$to[keep]
    if (mode=="sig") Pvals <- Pvals[keep]
    E$weight=E$weight[keep]
    
    if (length(E$from) > 0)
    {
      maximum=max(abs(c(maximum,max(abs(E$weight)),cut,abs(diagWeights))))
    } else maximum = 1
    if (cut==0)
    {
      avgW=(abs(E$weight)-minimum)/(maximum-minimum)
    } else if (maximum>cut) avgW=(abs(E$weight)-cut)/(maximum-cut) else avgW=rep(0,length(E$from))
    avgW[avgW<0]=0
    
    
    edgesort=sort(abs(E$weight),index.return=TRUE)$ix
    edge.width=rep(1,length(E$weight))
    
    
    # lty and curve settings:
    if (is.null(lty))
    {
      lty=rep(1,length(E$from))
    } else 
    {
      if (length(lty)==1) lty=rep(lty,length(E$from))
    }
    
    # Make bidirectional vector:
    if (length(bidirectional)==1) bidirectional=rep(bidirectional,length(E$from))
    if (length(bidirectional)!=length(E$from)) stop("Bidirectional vector must be of legth 1 or equal to the number of edges")
    
    srt <- cbind(pmin(E$from,E$to), pmax(E$from,E$to) , knots)
    if (!curveAll)
    {
      dub <- duplicated(srt)|duplicated(srt,fromLast=TRUE)
      if (length(curve)==1) curve <- rep(curve,length(E$from))
      curve <- ifelse(is.na(curve),ifelse(knots==0&dub&!bidirectional&is.na(curve),ifelse(E$from==srt[,1],1,-1) * ave(1:nrow(srt),srt[,1],srt[,2],bidirectional,FUN=function(x)seq(curveDefault,-curveDefault,length=length(x))),0),curve)
      rm(dub)
    }
    
    # Layout settings:
    if (nNodes == 1)
    {
      layout <- matrix(0,1,2)
    } else {
      if (is.null(layout)) layout="default"
      if (!is.numeric(layout))
      {
        # If function, assume igraph function (todo: check this)
        if (is.function(layout))
        {
          Graph <- graph.edgelist(as.matrix(cbind(E$from,E$to)), any(directed))
          E(Graph)$weight <- E$weight
          layout <- do.call(layout,c(list(graph = Graph),layout.par))
        } else {
          
          if (length(layout) > 1) stop("Incorrect specification of layout.")
          if (layout=="default" & (any(directed) | !weighted)) layout="spring"
          if (layout=="default" | layout=="circular" | layout=="circle" | layout=="groups") 
          {
            if (is.null(groups) | layout == "circle")
            {
              layout=matrix(0,nrow=nNodes,ncol=2)
              tl=nNodes+1
              layout[,1]=sin(seq(0,2*pi, length=tl))[-tl]
              layout[,2]=cos(seq(0,2*pi, length=tl))[-tl] 
            } else
            {
              if (is.null(rotation)) rotation=rep(0,length=length(groups))
              
              l1=matrix(0,nrow=length(groups),ncol=2)
              tl=nrow(l1)+1
              l1[,1]=sin(seq(0,2*pi, length=tl))[-tl]
              l1[,2]=cos(seq(0,2*pi, length=tl))[-tl]
              l1=l1*length(groups)*layout.control
              
              layout=matrix(0,nrow=nNodes,ncol=2)
              for (i in 1:length(groups)) 
              {
                tl=length(groups[[i]])+1
                layout[groups[[i]],1]=sin(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,1]
                layout[groups[[i]],2]=cos(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,2] 
              }
            }
          } else if (layout=="spring")
          {
            if (length(E$weight) > 0)
            {
              if (mode != "sig")
              {
                layout=qgraph.layout.fruchtermanreingold(cbind(E$from,E$to),abs(E$weight/max(abs(E$weight)))^2,nNodes,rotation=rotation,layout.control=layout.control,
                                                         niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
                                                         constraints=layout.par$constraints)
              } else
              {
                layout=qgraph.layout.fruchtermanreingold(cbind(E$from,E$to),abs(E$weight),nNodes,rotation=rotation,layout.control=layout.control,
                                                         niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
                                                         constraints=layout.par$constraints)
              }
            } else
            {
              if (mode != "sig")
              {
                layout=qgraph.layout.fruchtermanreingold(cbind(E$from,E$to),numeric(0),nNodes,rotation=rotation,layout.control=layout.control,
                                                         niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
                                                         constraints=layout.par$constraints)
              } else
              {
                layout=qgraph.layout.fruchtermanreingold(cbind(E$from,E$to),numeric(0),nNodes,rotation=rotation,layout.control=layout.control,
                                                         niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
                                                         constraints=layout.par$constraints)
              }
            }
          } 
        }
      }
      # Layout matrix:
      if (is.matrix(layout)) if (ncol(layout)>2)
      {
        Lmat=layout
        LmatX=seq(-1,1,length=ncol(Lmat))
        LmatY=seq(1,-1,length=nrow(Lmat))
        layout=matrix(0,nrow=nNodes,ncol=2)
        
        loc <- t(sapply(1:nNodes,function(x)which(Lmat==x,arr.ind=T)))
        layout <- cbind(LmatX[loc[,2]],LmatY[loc[,1]])
        
      }
    }
    
    # Rescale layout:
    l=original.layout=layout
    if (rescale) {
      if (aspect)
      {
        # center:
        l[,1] <- l[,1] - mean(l[,1])
        l[,2] <- l[,2] - mean(l[,2])
        lTemp <- l
        
        if (length(unique(lTemp[,1]))>1)
        {
          l[,1]=(lTemp[,1]-min(lTemp))/(max(lTemp)-min(lTemp))*2-1
        } else l[,1] <- 0
        if (length(unique(lTemp[,2]))>1)
        {
          l[,2]=(lTemp[,2]-min(lTemp))/(max(lTemp)-min(lTemp))*2-1 
        } else l[,2] <- 0
        
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
        
        layout=l    
      } else
      {
        if (length(unique(l[,1]))>1)
        {
          l[,1]=(l[,1]-min(l[,1]))/(max(l[,1])-min(l[,1]))*2-1
        } else l[,1] <- 0
        if (length(unique(l[,2]))>1)
        {
          l[,2]=(l[,2]-min(l[,2]))/(max(l[,2])-min(l[,2]))*2-1 
        } else l[,2] <- 0
        layout=l
      }
    }
    
    ## Offset and scale:
    if (length(layoutScale) == 1) layoutScale <- rep(layoutScale,2)
    if (length(layoutOffset) == 1) layoutOffset <- rep(layoutOffset,2)
    layout[,1] <- layout[,1] * layoutScale[1] + layoutOffset[1]
    layout[,2] <- layout[,2] * layoutScale[2] + layoutOffset[2]
    l <- layout
    
    
    # Set Edge widths:
    if (mode=="direct")
    {
      edge.width <- abs(E$weight)
    } else
    {
      if (weighted)
      {
        edge.width <- avgW*(esize-1)+1
        edge.width[edge.width<1]=1
      } else {
        edge.width <- rep(esize,length(E$weight))
      }
    }
    
    #     # Set edge colors:
    #     if (is.null(edge.color) || (any(is.na(edge.color)) || fade))
    #     {
    #       if (!is.null(edge.color))
    #       {
    #         repECs <- TRUE
    #         ectemp <- edge.color
    #       } else  repECs <- FALSE
    #       
    #       col <- rep(1,length(E$from))
    #       
    #       if (weighted) 
    #       {
    #         #Edge color:
    #         edge.color=rep("#00000000",length(E$from))
    #         
    #         
    #         if (mode=="strength"|mode=="direct")
    #         {
    #           if (cut==0) 
    #           {
    #             col=(abs(E$weight)-minimum)/(maximum-minimum)
    #           } else 
    #           {
    #             col=(abs(E$weight)-minimum)/(cut-minimum)
    #           }
    #           col[col>1]=1
    #           col[col<0]=0
    #           if (!gray)
    #           {
    #             if (transparency) 
    #             {
    #               col=col^(2)
    #               neg=col2rgb(rgb(0.75,0,0))/255
    #               pos=col2rgb(rgb(0,0.6,0))/255
    #               
    #               # Set colors for edges over cutoff:
    #               edge.color[E$weight< -1* minimum] <- rgb(neg[1],neg[2],neg[3],col[E$weight< -1*minimum])
    #               edge.color[E$weight> minimum] <- rgb(pos[1],pos[2],pos[3],col[E$weight> minimum])
    #             } else 
    #             {
    #               edge.color[E$weight>minimum]=rgb(1-col[E$weight > minimum],1-(col[E$weight > minimum]*0.25),1-col[E$weight > minimum])
    #               edge.color[E$weight< -1*minimum]=rgb(1-(col[E$weight < (-1)*minimum]*0.25),1-col[E$weight < (-1)*minimum],1-col[E$weight < (-1)*minimum])
    #             }	
    #           } else
    #           {
    #             if (transparency) 
    #             {
    #               col=col^(2)
    #               neg="gray10"
    #               pos="gray10"
    #               
    #               # Set colors for edges over cutoff:
    #               edge.color[E$weight< -1* minimum] <- rgb(neg[1],neg[2],neg[3],col[E$weight< -1*minimum])
    #               edge.color[E$weight> minimum] <- rgb(pos[1],pos[2],pos[3],col[E$weight> minimum])
    #             } else 
    #             {
    #               edge.color[E$weight>minimum]=rgb(1-col[E$weight > minimum],1-(col[E$weight > minimum]),1-col[E$weight > minimum])
    #               edge.color[E$weight< -1*minimum]=rgb(1-(col[E$weight < (-1)*minimum]),1-col[E$weight < (-1)*minimum],1-col[E$weight < (-1)*minimum])
    #             }
    #           }
    #         }
    #         if (mode == "sig")
    #         {	
    #           
    #           if (!gray)
    #           {
    #             
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 3) edge.color[Pvals > 0 & Pvals < alpha[4]  & E$weight > minimum] <- "cadetblue1"	
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 2) edge.color[Pvals > 0 & Pvals < alpha[3]  & E$weight > minimum] <- "#6495ED"
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 1) edge.color[Pvals > 0 & Pvals < alpha[2]  & E$weight > minimum] <- "blue"				
    #             # Set colors for edges over sig < 0.01 :
    #             edge.color[Pvals > 0 & Pvals < alpha[1]  & E$weight > minimum] <- "darkblue"
    #             
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 3) edge.color[Pvals < 0 & Pvals > (-1 * alpha[4])  & E$weight < -1 * minimum] <- rgb(1,0.8,0.4) 	
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 2) edge.color[Pvals < 0 & Pvals > (-1 * alpha[3])  & E$weight < -1 * minimum] <- "orange"
    #             # Set colors for edges over sig > 0.01 :
    #             if (length(alpha) > 1) edge.color[Pvals < 0 & Pvals > (-1 * alpha[2])  & E$weight < -1 * minimum] <- "darkorange"				
    #             # Set colors for edges over sig < 0.01 :
    #             edge.color[Pvals < 0 & Pvals > (-1 * alpha[1])  & E$weight < -1 * minimum] <- "darkorange2"
    #             
    #             
    #             
    #             
    #           } else
    #           {
    #             Pvals <- abs(Pvals)
    #             # Set colors for edges over sig < 0.01 :
    #             if (length(alpha) > 3) edge.color[Pvals > 0 & Pvals < alpha[4]  & E$weight > minimum] <- rgb(0.7,0.7,0.7)
    #             if (length(alpha) > 2) edge.color[Pvals > 0 & Pvals < alpha[3]  & E$weight > minimum] <- rgb(0.5,0.5,0.5)
    #             if (length(alpha) > 1) edge.color[Pvals > 0 & Pvals < alpha[2]  & E$weight > minimum] <- rgb(0.3,0.3,0.3)
    #             edge.color[Pvals > 0 & Pvals < alpha[1]  & E$weight > minimum] <- "black"
    #             
    #           }
    #         }
    #         if (cut!=0)
    #         {
    #           if (!gray & (mode=="strength"|mode=="direct"))
    #           {
    #             # Set colors for edges over cutoff:
    #             edge.color[E$weight<= -1*cut] <- "red"
    #             edge.color[E$weight>= cut] <- "darkgreen"
    #           } else if (gray)
    #           {
    #             # Set colors for edges over cutoff:
    #             edge.color[E$weight<= -1*cut] <- "black"
    #             edge.color[E$weight>= cut] <- "black"
    #             
    #           }
    #         }
    #         
    #       } else
    #       {
    #         if (!is.logical(transparency)) Trans=transparency else Trans=1
    #         edge.color=rep(rgb(0.5,0.5,0.5,Trans),length(edgesort))
    #       }
    #       if (repECs)
    #       {
    #         ## Add trans:
    #         if (fade & any(!is.na(ectemp)))
    #         {
    #           if (!is.logical(transparency)) col <- rep(transparency,length(col))
    #           edge.color[!is.na(ectemp)] <- addTrans(ectemp[!is.na(ectemp)],round(255*col[!is.na(ectemp)]))
    #         } else {
    #           edge.color[!is.na(ectemp)] <- ectemp[!is.na(ectemp)]
    #         }
    #         rm(ectemp)
    #       }
    #     } else {
    #       if (length(edge.color) == 1) edge.color <- rep(edge.color,length(E$from))
    #       if (length(edge.color) != length(E$from)) stop("Number of edge colors not equal to number of edges")
    #     }
    
    
    # Set edge colors:
    if (is.null(edge.color) || (any(is.na(edge.color)) || fade))
    {
      if (!is.null(edge.color))
      {
        repECs <- TRUE
        ectemp <- edge.color
      } else  repECs <- FALSE
      
      # col vector will contain relative strength:
      col <- rep(1,length(E$from))
      
      if (weighted) 
      {
        # Dummmy vector containing invisible edges:
        edge.color <- rep("#00000000",length(E$from))
        
        # Normal color scheme (0 is invisible, stronger is more visible)
        if (mode=="strength"|mode=="direct")
        {
          # Set relative strength:
          if (cut==0) 
          {
            col <- (abs(E$weight)-minimum)/(maximum-minimum)
          } else 
          {
            col <- (abs(E$weight)-minimum)/(cut-minimum)
          }
          col[col>1] <- 1
          col[col<0] <- 0
          col <- col^colFactor      
          
          # Set edges between minimum and cut:
          if (fade)
          {
            if (isTRUE(transparency))
            {
              edge.color[E$weight > minimum] <- addTrans(posCol[1],round(col[E$weight > minimum]*255))
              edge.color[E$weight < -1*minimum] <- addTrans(negCol[1],round(col[E$weight < -1*minimum]*255))
            } else {
              edge.color[E$weight > minimum] <- Fade(posCol[1],col[E$weight > minimum], background)
              edge.color[E$weight < -1*minimum] <- Fade(negCol[1],col[E$weight < -1*minimum], background)
            }
          } else {
            edge.color[E$weight > minimum] <- posCol[1]
            edge.color[E$weight < -1*minimum] <- negCol[1]
          }
          
          # Set colors over cutoff if cut != 0:
          if (cut!=0)
          {
            if (posCol[1]!=posCol[2]) edge.color[E$weight >= cut] <- posCol[2]
            if (negCol[1]!=negCol[2]) edge.color[E$weight <= -1*cut] <- negCol[2]
          }
        } 
        
        if (mode == "sig")
        {	
          if (!gray)
          {
            
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 3) edge.color[Pvals > 0 & Pvals < alpha[4]  & E$weight > minimum] <- "cadetblue1"	
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 2) edge.color[Pvals > 0 & Pvals < alpha[3]  & E$weight > minimum] <- "#6495ED"
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 1) edge.color[Pvals > 0 & Pvals < alpha[2]  & E$weight > minimum] <- "blue"				
            # Set colors for edges over sig < 0.01 :
            edge.color[Pvals > 0 & Pvals < alpha[1]  & E$weight > minimum] <- "darkblue"
            
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 3) edge.color[Pvals < 0 & Pvals > (-1 * alpha[4])  & E$weight < -1 * minimum] <- rgb(1,0.8,0.4) 	
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 2) edge.color[Pvals < 0 & Pvals > (-1 * alpha[3])  & E$weight < -1 * minimum] <- "orange"
            # Set colors for edges over sig > 0.01 :
            if (length(alpha) > 1) edge.color[Pvals < 0 & Pvals > (-1 * alpha[2])  & E$weight < -1 * minimum] <- "darkorange"				
            # Set colors for edges over sig < 0.01 :
            edge.color[Pvals < 0 & Pvals > (-1 * alpha[1])  & E$weight < -1 * minimum] <- "darkorange2"
            
            
            
            
          } else
          {
            Pvals <- abs(Pvals)
            # Set colors for edges over sig < 0.01 :
            if (length(alpha) > 3) edge.color[Pvals > 0 & Pvals < alpha[4]  & E$weight > minimum] <- rgb(0.7,0.7,0.7)
            if (length(alpha) > 2) edge.color[Pvals > 0 & Pvals < alpha[3]  & E$weight > minimum] <- rgb(0.5,0.5,0.5)
            if (length(alpha) > 1) edge.color[Pvals > 0 & Pvals < alpha[2]  & E$weight > minimum] <- rgb(0.3,0.3,0.3)
            edge.color[Pvals > 0 & Pvals < alpha[1]  & E$weight > minimum] <- "black"
            
          }
        }
      } else
      {
        if (!is.logical(transparency)) Trans <- transparency else Trans <- 1
        edge.color <- rep(addTrans(unCol,round(255*Trans)),length(edgesort))
      }
      if (repECs)
      {
        ## Add trans:
        if (fade & any(!is.na(ectemp)))
        {
          if (!is.logical(transparency)) col <- rep(transparency,length(col))
          if (isTRUE(transparency))
          {
            edge.color[!is.na(ectemp)] <- addTrans(ectemp[!is.na(ectemp)],round(255*col[!is.na(ectemp)]))
          } else {
            edge.color[!is.na(ectemp)] <- Fade(ectemp[!is.na(ectemp)],col[!is.na(ectemp)], background)
          }
        } else {
          edge.color[!is.na(ectemp)] <- ectemp[!is.na(ectemp)]
        }
        rm(ectemp)
      }
    } else {
      if (length(edge.color) == 1) edge.color <- rep(edge.color,length(E$from))
      if (length(edge.color) != length(E$from)) stop("Number of edge colors not equal to number of edges")
    }
    
    
    # Vertex color:
    if (is.null(color) & !is.null(groups))
    {
      if (!gray) color <- rainbow(length(groups))
      if (gray) color <- sapply(seq(0.2,0.8,length=length(groups)),function(x)rgb(x,x,x))
    }
    if (is.null(color))	color <- "background"  
    vertex.colors <- rep(color, length=nNodes)
    if (!is.null(groups)) {
      for (i in 1:length(groups)) vertex.colors[groups[[i]]]=color[i] }
    if (length(color)==nNodes) vertex.colors <- color
    vertex.colors[vertex.colors=="background"] <- background
    
    # Dummy groups list:
    if (is.null(groups)) groups <- list(1:nNodes)
    
    # Scores:
    if (!is.null(scores)) 
    {
      if (length(scores)!=nNodes)
      {
        warning ("Length of scores is not equal to nuber of items")
      } else
      {
        bcolor <- vertex.colors
        if (is.null(scores.range)) scores.range=c(min(scores),max(scores))
        scores[is.na(scores)]=scores.range[1]
        rgbmatrix=1-t(col2rgb(vertex.colors)/255)
        for (i in 1:nNodes) rgbmatrix[i,]=rgbmatrix[i,] * (scores[i]-scores.range[1] ) / (scores.range[2]-scores.range[1] )
        vertex.colors=rgb(1-rgbmatrix)
      }
    }
    
    if (diagCols)
    {
      if (diagCols & !is.null(scores)) stop("Multiple modes specified for vertex colors (diag and scores)")
      if (diagCols & weighted)
      {
        if (is.null(bcolor) & !all(vertex.colors=="white")) bcolor=vertex.colors
        if (cut==0) 
        {
          colV=(abs(diagWeights)-minimum)/(maximum-minimum)
        } else 
        {
          colV=(abs(diagWeights)-minimum)/(cut-minimum)
        }
        colV[colV>1]=1
        colV[colV<0]=0
        
        if (transparency) 
        {
          vertex.colors=rep("#00000000",nNodes)
          colV=colV^(2)
          neg=col2rgb(rgb(0.75,0,0))/255
          pos=col2rgb(rgb(0,0.6,0))/255
          
          # Set colors for edges over cutoff:
          vertex.colors[diagWeights< -1* minimum] <- rgb(neg[1],neg[2],neg[3],colV[diagWeights< -1*minimum])
          vertex.colors[diagWeights> minimum] <- rgb(pos[1],pos[2],pos[3],colV[diagWeights> minimum])
        } else 
        {
          vertex.colors=rep("white",nNodes)
          vertex.colors[diagWeights>minimum]=rgb(1-colV[diagWeights > minimum],1-(colV[diagWeights> minimum]*0.25),1-colV[diagWeights > minimum])
          vertex.colors[diagWeights< -1*minimum]=rgb(1-(colV[diagWeights< (-1)*minimum]*0.25),1-colV[diagWeights < (-1)*minimum],1-colV[diagWeights < (-1)*minimum])
        }
        if (cut!=0)
        {
          # Set colors for edges over cutoff:
          vertex.colors[diagWeights<= -1*cut] <- "red"
          vertex.colors[diagWeights>= cut] <- "darkgreen"
        }
      }
    }
    if (is.null(bcolor))
    {
      bcolor <- rep(ifelse(mean(col2rgb(background)/255)>0.5,"black","white"),nNodes)
    } else {
      bcolor <- rep(bcolor,length=nNodes)
    }
    
    if (vTrans<255)
    {
      # Transparance in vertex colors:
      num2hex <- function(x)
      {
        hex=unlist(strsplit("0123456789ABCDEF",split=""))
        return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
      }
      
      colHEX <- rgb(t(col2rgb(vertex.colors)/255))
      
      vertex.colors <- paste(sapply(strsplit(colHEX,split=""),function(x)paste(x[1:7],collapse="")),num2hex(vTrans),sep="")
    }
    
    
    # Vertex size:
    if (length(vsize)==1) vsize=rep(vsize,nNodes)
    if (length(vsize2)==1) vsize2=rep(vsize2,nNodes)
    if (!edgelist) Vsums=rowSums(abs(input))+colSums(abs(input))
    if (edgelist)
    {
      Vsums=numeric(0)
      for (i in 1:nNodes) Vsums[i]=sum(c(input[,1:2])==i)
    }
    if (length(vsize)==2 & nNodes>2 & length(unique(Vsums))>1) vsize=vsize[1] + (vsize[2]-vsize[1]) * (Vsums-min(Vsums))/(max(Vsums)-min(Vsums))
    if (length(vsize)==2 & nNodes>2 & length(unique(Vsums))==1) vsize=rep(mean(vsize),nNodes)
    
    if (length(vsize2)==2 & nNodes>2 & length(unique(Vsums))>1) vsize2=vsize2[1] + (vsize2[2]-vsize2[1]) * (Vsums-min(Vsums))/(max(Vsums)-min(Vsums))
    if (length(vsize2)==2 & nNodes>2 & length(unique(Vsums))==1) vsize2=rep(mean(vsize2),nNodes)
    
    # Vertex shapes:
    if (length(shape)==1) shape=rep(shape,nNodes)
    
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
      if (!shape[i]%in%c("circle","square","triangle","diamond","rectangle")) stop(paste("Shape",shape[i],"is not supported"))
    }
    
    
    # Super cool background:
    
    if (is.logical(bg)) if (bg) {
      
      colarray=array(dim=c(bgres,bgres,length(groups)))
      
      seq=seq(-1.2,1.2,length=bgres+1)
      
      for (G in 1:length(groups)) {
        
        Xg=l[groups[[G]],1]
        Yg=l[groups[[G]],2]
        
        for (i in 1:bgres) {
          for (j in 1:bgres) {
            
            Xp=mean(seq[i:(i+1)])
            Yp=mean(seq[j:(j+1)])
            
            colarray[i,j,G]=min(sqrt( (Xp-Xg)^2 + (Yp-Yg)^2)) }}}
      
      colarray=((2.2-colarray)/2.2)^bgcontrol
      
      colarray2=array(dim=c(3,bgres,bgres))
    }
    
    # Arrow sizes:
    if (length(asize)==1) asize=rep(asize,length(E$from))
    
    if (length(asize)!=length(E$from)) warning("Length of 'asize' is not equal to the number of edges")
    
    
    
    # Edge labels:
    # Make labels:
    if (is.logical(edge.labels))
    {
      if (edge.labels)
      {
        edge.labels=round(E$weight,2)
      }
    }
    
    
    if (!is.logical(edge.labels))
    {
      #       edge.labels=as.character(edge.labels)
      if (length(edge.labels)!=length(E$from))
      {
        warning("Number of edge labels did not correspond to number of edges, edge labes have been ommited")
        edge.labels <- FALSE
      }
      midX=numeric(0)
      midY=numeric(0)
      
      if (length(edge.labels) > 0 & is.character(edge.labels))
      {
        ## Set fonts (symbol):
        strsplE=strsplit(edge.labels,"")
        
        greekE=sapply(strsplE,function(x)any(x=="*"))
        edge.labels=sapply(strsplE,function(x)paste(x[x!="*"],collapse=""))
        
        edge.font=rep(1,length(E$from))
        edge.font[greekE]=5
        
        edge.labels[edge.labels=="NA"]=""
      } else edge.font <- 1
    }
    
    # Compute alpha of each node:
    vAlpha <- col2rgb(vertex.colors,TRUE)[4,]
    
    if (!DoNotPlot)
    {
      
      # PLOT:
      marOrig <- par("mar")
      bgOrig <- par("bg")
      if (plot)
      {
        par(mar=c(0,0,0,0), bg=background)
        plot(1, ann = FALSE, axes = FALSE, xlim = c(-1 - mar[2], 1 + mar[4] + (((legend&is.null(scores))|(filetype=="svg")) * 2.4/GLratio)), ylim = c(-1 - mar[1] ,1 + mar[3]),type = "n", xaxs = "i", yaxs = "i")
      }
      
      # if (PlotOpen) 
      # {
      width <- par('pin')[1]
      height <- par('pin')[2]
      
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
      
      
      if (is.logical(bg)) if (bg){
        
        
        for (i in 1:bgres) {
          for (j in 1:bgres) {
            
            for (C in 1:3) {
              
              colarray2[C,i,j]=min(c(1,max(colarray[i,j,]*(col2rgb(color)[C,]/255))))  }
            
            polygon(c(seq[i],seq[i+1],seq[i+1],seq[i]),c(seq[j],seq[j],seq[j+1],seq[j+1]),
                    col=rgb(colarray2[1,i,j],colarray2[2,i,j],colarray2[3,i,j]),border=NA)
            
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
rasterImage(read.jpeg("%s"), 0,0,1,1, interpolate=FALSE)', images[i]))
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
            # Replace destination of edge to edge of node if needed:
            if (is.logical(arrows) | vAlpha[E$to[i]] < 255) if ((arrows & directed[i]) | vAlpha[E$to[i]] < 255)
            {
              # 				xd=x2-x1
              # 				yd=y2-y1
              # 				d2=sqrt(sum(xd^2+yd^2))
              # 				if (shape[E$to[i]]!="square")
              # 				{
              # 					x2=x2-xd*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2]/d2)
              # 					y2=y2-yd*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2]/d2)
              # 				}
              # 				if (shape[E$to[i]]=="square")
              # 				{
              # 					x2=x2-xd*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
              # 					y2=y2-yd*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
              # 				}
              NewPoints <- Cent2Edge(x2,y2,ifelse(residEdge[i],loopRotation[E$to[i]],atan2usr2in(x1-x2,y1-y2)),vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,0))
              x2 <- NewPoints[1]
              y2 <- NewPoints[2]
              
              # Replace source of edge to edge of node if needed:
              if ((any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i]) | vAlpha[E$from[i]] < 255)
              {
                # 					xd=x2-x1
                # 					yd=y2-y1
                # 					d2=sqrt(sum(xd^2+yd^2))
                # 					if (shape[E$from[i]]!="square")
                # 					{
                # 						x1=x1+xd*(0.5*vsize[E$from[i]]*0.130*(7/width)*par("cin")[2]/d2)
                # 						y1=y1+yd*(0.5*vsize[E$from[i]]*0.130*(7/height)*par("cin")[2]/d2)
                # 					}
                # 					if (shape[E$from[i]]=="square")
                # 					{
                # 						x1=x1+xd*(0.5*vsize[E$from[i]]*0.130*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
                # 						y1=y1+yd*(0.5*vsize[E$from[i]]*0.130*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
                # 					}
                
                NewPoints <- Cent2Edge(x1,y1,ifelse(residEdge[i],loopRotation[E$from[i]],atan2usr2in(x2-x1,y2-y1)),vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,0))
                x1 <- NewPoints[1]
                y1 <- NewPoints[2]  
                
              }
            }
            if (!is.logical(edge.labels))
            {
              midX[i]=mean(c(x1,x2))
              midY[i]=mean(c(y1,y2))
            }
            
            lines(c(x1,x2),c(y1,y2),lwd=edge.width[i],col=edge.color[i],lty=lty[i])
            if (directed[i])
            {
              if (!is.logical(arrows))
              {
                Ax=seq(x1,x2,length=arrows+2)
                Ay=seq(y1,y2,length=arrows+2)
                for (a in 1:arrows+1)
                {
                  #                   qgraph.arrow(Ax[a],Ay[a],x1,y1,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                  #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                  DrawArrow(Ax[a],Ay[a],atan2usr2in(Ax[a]-x1,Ay[a]-y1),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                }
              }
              else if (arrows)
              {
                #                 qgraph.arrow(x2,y2,x1,y1,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                #                              col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                DrawArrow(x2,y2,atan2usr2in(x2-x1,y2-y1),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                
                if (any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i])
                {
                  #                   qgraph.arrow(x1,y1,x2,y2,length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                  #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                  DrawArrow(x1,y1,atan2usr2in(x1-x2,y1-y2),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                }
              }
            }
          } else {
            if (E$from[i]==E$to[i])
            {
              #               loopX=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2])
              #               spx=c(x1+loopX,x1,x1-loopX)
              #               loopY=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2])
              #               spy=c(y1,y1+loopY,y1)
              #               spl <- spl2 <- xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=FALSE)
              if (DefLoopRot)
              {
                centX <- mean(layout[,1])
                centY <- mean(layout[,2])
                for (g in 1:length(groups))
                {
                  if (E$from[i]%in%groups[[g]])
                  {
                    centX <- mean(layout[groups[[g]],1])
                    centY <- mean(layout[groups[[g]],2])
                  }
                }
                rot <- atan2usr2in(x1-centX,y1-centY)
                if (shape[E$from[i]]=="square")
                {
                  rot <- c(0,0.5*pi,pi,1.5*pi)[which.min(abs(c(0,0.5*pi,pi,1.5*pi)-rot%%(2*pi)))]
                }
              } else rot <- loopRotation[E$from[i]]
              spl <- SelfLoop(x1,y1,rot,vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],residuals,residScale)
              
            } else 
            {
              midx <- (x1 + x2)/2
              midy <- (y1 + y2)/2
              #spx <- midx - curve[i] * (y2 - y1)/2
              #spy <- midy + curve[i] * (x2 - x1)/2
              #               curvemid <- Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
             if (knots[i]!=0)
             {
               spl <- xspline(c(x1,knotLayout[knots[i],1],x2),c(y1,knotLayout[knots[i],2],y2),0,draw=FALSE)
             } else {               
               curvemid <- PerpMid(c(midx,midy),c(x2,y2),cex=curve[i]) 
               
               #                 Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
               spx <- curvemid[1]
               spy <- curvemid[2]
               spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=FALSE) 
             }
              
            }	
            if (E$from[i]!=E$to[i])
            {
              # Replace destination of edge to edge of node if needed:
              if (is.logical(arrows)| vAlpha[E$to[i]] < 255) if (arrows & directed[i]| vAlpha[E$to[i]] < 255)
              {
                # 				xd=x2-spl$x[length(spl$x)-1]
                # 				yd=y2-spl$y[length(spl$y)-1]
                # 				d2=sqrt(sum(xd^2+yd^2))
                # 				if (shape[E$to[i]]!="square")
                # 				{
                # 					x2=x2-xd*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2]/d2)
                # 					y2=y2-yd*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2]/d2)
                # 				}
                # 				if (shape[E$to[i]]=="square")
                # 				{
                # 					x2=x2-xd*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
                # 					y2=y2-yd*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
                # 				}
                
                NewPoints <- Cent2Edge(x2,y2,ifelse(residEdge[i],loopRotation[E$to[i]],atan2usr2in(spl$x[length(spl$x)-1]-x2,spl$y[length(spl$y)-1]-y2)),vsize[E$to[i]],vsize2[E$to[i]],shape[E$to[i]],ifelse(residEdge[i],residScale,0))
                x2 <- NewPoints[1]
                y2 <- NewPoints[2]
                
                #               if (E$from[i]==E$to[i])
                #               {
                #                 spx=c(x1+loopX,x1,x1-loopX)
                #                 spy=c(y1,y1+loopY,y1)
                #                 spl=xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=FALSE)
                #               } else 
                #               {
                midx <- (x1 + x2)/2
                midy <- (y1 + y2)/2
                #spx <- midx - curve[i] * (y2 - y1)/2
                #spy <- midy + curve[i] * (x2 - x1)/2
                #                 curvemid <- Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
                if (knots[i]!=0)
                {
                  spl <- xspline(c(x1,knotLayout[knots[i],1],x2),c(y1,knotLayout[knots[i],2],y2),0,draw=FALSE)
                } else {               
                  curvemid <- PerpMid(c(midx,midy),c(x2,y2),cex=curve[i]) 
                  
                  spx <- curvemid[1]
                  spy <- curvemid[2]
                  spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=FALSE) 
                }
                #               }
                # Replace source of edge to edge of node if needed:
                if ((any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i])| vAlpha[E$from[i]] < 255)
                {
                  # 					xd= spl$x[2] - x1
                  # 					yd= spl$y[2] - y1
                  # 					d2=sqrt(sum(xd^2+yd^2))
                  # 					if (shape[E$from[i]]!="square")
                  # 					{
                  # 						x1=x1+xd*(0.5*vsize[E$from[i]]*0.130*(7/width)*par("cin")[2]/d2)
                  # 						y1=y1+yd*(0.5*vsize[E$from[i]]*0.130*(7/height)*par("cin")[2]/d2)
                  # 					}
                  # 					if (shape[E$from[i]]=="square")
                  # 					{
                  # 						x1=x1+xd*(0.5*vsize[E$from[i]]*0.130*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
                  # 						y1=y1+yd*(0.5*vsize[E$from[i]]*0.130*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
                  # 					}
                  
                  NewPoints <- Cent2Edge(x1,y1,ifelse(residEdge[i],loopRotation[E$from[i]],atan2usr2in(spl$x[2]-x1,spl$y[2]-y1)),vsize[E$from[i]],vsize2[E$from[i]],shape[E$from[i]],ifelse(residEdge[i],residScale,0))
                  x1 <- NewPoints[1]
                  y1 <- NewPoints[2]
                  
                }
                #               if (E$from[i]==E$to[i])
                #               {
                #                 loopX=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/width)*par("cin")[2])
                #                 spx=c(layout[E$from[i],1]+loopX,layout[E$from[i],1],layout[E$from[i],1]-loopX)
                #                 loopY=loop*3*(0.5*vsize[E$to[i]]*0.130*(7/height)*par("cin")[2])
                #                 spy=c(layout[E$from[i],2],layout[E$from[i],2]+loopY,layout[E$from[i],2])
                #                 spl <- spl2 <- xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=FALSE)
                #                 # Rotate spline:
                #                 spl$x <- layout[E$from[i],1] + (spl2$x - layout[E$from[i],1]) * cos(loopRotation[i]) - (spl2$y - layout[E$from[i],2]) * sin(loopRotation[i])
                #                 spl$y <- layout[E$from[i],2] + (spl2$x - layout[E$from[i],1]) * sin(loopRotation[i]) + (spl2$y - layout[E$from[i],2]) * cos(loopRotation[i])
                #                 
                #               } else 
                #               {
                midx <- (x1 + x2)/2
                midy <- (y1 + y2)/2
                #spx <- midx - curve[i] * (y2 - y1)/2
                #spy <- midy + curve[i] * (x2 - x1)/2
                #                 curvemid <- Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
                if (knots[i]!=0)
                {
                  spl <- xspline(c(x1,knotLayout[knots[i],1],x2),c(y1,knotLayout[knots[i],2],y2),0,draw=FALSE)
                } else {               
                  curvemid <- PerpMid(c(midx,midy),c(x2,y2),cex=curve[i]) 
                  
                  #                 Cent2Edge(midx,midy,atan2usr2in(x2-x1,y2-y1)-sign(curve[i])*pi/2,abs(curve[i])*5*2,"circle")
                  spx <- curvemid[1]
                  spy <- curvemid[2]
                  spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=FALSE) 
                }
                #               }
              }
              #               } else if (E$from[i]==E$to[i])
              #               {
              #                 # Rotate spline:
              #                 spl$x <- layout[E$from[i],1] + (spl2$x - layout[E$from[i],1]) * cos(loopRotation[i]) - (spl2$y - layout[E$from[i],2]) * sin(loopRotation[i])
              #                 spl$y <- layout[E$from[i],2] + (spl2$x - layout[E$from[i],1]) * sin(loopRotation[i]) + (spl2$y - layout[E$from[i],2]) * cos(loopRotation[i])        
              #               }
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
            
            if (!is.logical(edge.labels))
            {
              midX[i]=spl$x[floor(length(spl$x)/2)]
              midY[i]=spl$y[floor(length(spl$y)/2)]
            }
            lines(spl,lwd=edge.width[i],col=edge.color[i],lty=lty[i])        
            
            if (directed[i])
            {
              if (!is.logical(arrows))
              {
                Ax=seq(1,length(spl$x),length=arrows+2)
                Ay=seq(1,length(spl$y),length=arrows+2)
                for (a in 2:(arrows+1))
                {
                  #                   qgraph.arrow(spl$x[Ax[a]+1],spl$y[Ay[a]+1],spl$x[Ax[a]],spl$y[Ay[a]],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                  #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                  
                  DrawArrow(spl$x[Ax[a]+1],spl$y[Ay[a]+1],atan2usr2in(spl$x[Ax[a]+1]-spl$x[Ax[a]],spl$y[Ay[a]+1]-spl$y[Ay[a]]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                }
              }
              else if (arrows)
              {
                #                 qgraph.arrow(spl$x[length(spl$x)],spl$y[length(spl$y)],spl$x[length(spl$x)-1],spl$y[length(spl$y)-1],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                #                              col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                DrawArrow(spl$x[length(spl$x)],spl$y[length(spl$y)],atan2usr2in(spl$x[length(spl$x)]-spl$x[length(spl$x)-1],spl$y[length(spl$y)]-spl$y[length(spl$y)-1]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                
                if (any(E$from==E$to[i] & E$to==E$from[i]) & bidirectional[i])
                {
                  #                   qgraph.arrow(spl$x[1],spl$y[1],spl$x[2],spl$y[2],length=asize[i],angle=30*pi/180,lwd=max(edge.width[i]/2,1),
                  #                                col=edge.color[i],open=open,Xasp=width/height,lty=lty[i])
                  DrawArrow(spl$x[1],spl$y[1],atan2usr2in(spl$x[1]-spl$x[2],spl$y[1]-spl$y[2]),angle=arrowAngle,cex=asize[i],open=open,lwd=max(edge.width[i]/2,1),lty=lty[i],edge.color[i])
                }
              }
              
            }
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
        edgesort2 <- edgesort2[!(duplicated(srt[edgesort2,])&bidirectional[edgesort2]) & (!duplicated(knots[edgesort2])|knots[edgesort2]==0)]
        if (length(edge.label.cex)==1) edge.label.cex <- rep(edge.label.cex,length(E$from))
        
        if (plotELBG)
        {
          for (i in edgesort2)
          {
            labwd <- strwidth(edge.labels[i],cex=edge.label.cex[i])
            labht <- strheight(edge.labels[i],cex=edge.label.cex[i])
            polygon(c(midX[i]-labwd/2,midX[i]+labwd/2,midX[i]+labwd/2,midX[i]-labwd/2),
                    c(midY[i]-labht/2,midY[i]-labht/2,midY[i]+labht/2,midY[i]+labht/2),
                    border=NA,
                    col=edge.label.bg[i])
          }
        }
        
        text(midX[edgesort2],midY[edgesort2],edge.labels[edgesort2],font=edge.font[edgesort2],cex=edge.label.cex[edgesort2],col=ELcolor[edgesort2])
      }			
      
      
      #if (nNodes==1) layout=matrix(0,1,2)
      # Plot nodes:
      
      # scale border width:
      #       border.width <- border.width * normC
      
      if(length(borders) == 1) borders <- rep(borders,nNodes)
      if (!XKCD)
      {
        # Check if nodes need to be plotted in for loop:
        if (!is.null(subplots) || any(shape=="rectangle"))
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
              xOff <- Cent2Edge(x,y,pi/2,vsize[i],vsize2[i],shape[i])[1] - x
              yOff <- Cent2Edge(x,y,0,vsize[i],vsize2[i],shape[i])[2] - y
              
              usr <- par("usr")
              # Plot background:
              rect(max(usr[1],x-xOff),max(usr[3],y-yOff),min(usr[2],x+xOff),min(usr[4],y+yOff),col=background,border=NA)
              # Plot subplot:
              subplot(eval(subplots[[i]]),c(max(usr[1],x-xOff),min(usr[2],x+xOff)), c(max(usr[3],y-yOff),min(usr[4],y+yOff)))  
              # Plot border:
              if (borders[i]) rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor[i],lwd=border.width)
            } else if (shape[i]!="rectangle")
            {
              points(layout[i,,drop=FALSE],cex=vsize[i],col=vertex.colors[i],lwd=border.width,pch=pch1[i])
              if (borders[i])
              {
                points(layout[i,,drop=FALSE],cex=vsize[i],col=bcolor[i],lwd=border.width,pch=pch2[i])
              }
            } else {
              xOff <- Cent2Edge(x,y,pi/2,vsize[i],vsize2[i],shape[i])[1] - x
              yOff <- Cent2Edge(x,y,0,vsize[i],vsize2[i],shape[i])[2] - y
              
              # Plot background:
              rect(x-xOff,y-yOff,x+xOff,y+yOff,col=vertex.colors[i],border=NA)
              if (borders[i])
              {
                rect(x-xOff,y-yOff,x+xOff,y+yOff,border=bcolor[i],lwd=border.width)
              }              
            }
          }      
        } else {
          bordVec <- unlist(lapply(order(vsize,decreasing=FALSE),function(x)rep(x,1+borders[x])))
          points(layout[bordVec,],cex=vsize[bordVec],col=ifelse(duplicated(bordVec),bcolor[bordVec],vertex.colors[bordVec]),lwd=border.width,pch=ifelse(duplicated(bordVec),pch2[bordVec],pch1[bordVec]))  
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
          pts <- lapply(circ,function(r)Cent2Edge(layout[i,1],layout[i,2],r,vsize[i],vsize2[i],shape[i]))
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
      
      # Make labels:
      if (is.logical(labels))
      {
        if (labels)
        {
          labels=1:nNodes
        }
      }
      
      if (!is.logical(labels))
      {
        #         labels=as.character(labels)
        # Vertex label symbols:
        # Set symbol font:
        if (is.character(labels))
        {
          strsplV=strsplit(labels,"")
          greekV=logical(0)
          for (i in 1:length(strsplV)) 
          {
            greekV[i]=any(strsplV[[i]]=="*")
            labels[i]=paste(strsplV[[i]][which(strsplV[[i]]!="*")],collapse="") 
          }
          V.font=rep(1,length(E$from))
          V.font[greekV]=5
        } else V.font <- 1
        
        if (is.null(label.cex)) label.cex <- pmax(1,vsize)
        # Rescale labels:
        if (label.scale)
        {
          VWidths <- sapply(mapply(Cent2Edge,cex=vsize,cex2=vsize2,shape=shape,MoreArgs=list(x=0,y=0,r=pi/2),SIMPLIFY=FALSE),'[',1) * 2
          VHeights <- sapply(mapply(Cent2Edge,cex=vsize,cex2=vsize2,shape=shape,MoreArgs=list(x=0,y=0,r=0),SIMPLIFY=FALSE),'[',2) * 2          
          LWidths <- pmax(sapply(label.cex,function(x)strwidth(label.norm,cex=x)),mapply(strwidth, s=labels, cex=label.cex))
          LHeights <- pmax(sapply(label.cex,function(x)strheight(label.norm,cex=x)),mapply(strheight, s=labels, cex=label.cex))
          
          label.cex <- label.cex * label.prop * pmin(VWidths/LWidths,VHeights/LHeights)
          #           label.cex[nchar(labels)>1]=label.cex[nchar(labels)>1]*2/nchar(labels[nchar(labels)>1],"width")
        }
        
        # Plot labels:
        text(layout[,1],layout[,2],labels,cex=label.cex,col=lcolor,font=V.font)
        
        # Set Tooltips:
        for (i in 1:nNodes) 
        {
          if (!is.null(tooltips)) if (!is.na(tooltips[i]))
          {
            if (filetype=='svg') setSVGShapeToolTip(desc=tooltips[i])
          }
          if (!is.null(SVGtooltips)) if (!is.na(SVGtooltips[i]))
          {
            setSVGShapeToolTip(desc=SVGtooltips[i])
          }
          #           text(layout[i,1],layout[i,2],labels[i],cex=label.cex[i],col=lcolor,font=V.font[i])
          # 		if (filetype=='tex' & !is.null(tooltips)) if (!is.na(tooltips[i])) place_PDF_tooltip(layout[i,1],layout[i,2],tooltips[i])
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
        
        for (i in 1:length(groups)) polygon(ellipse(cov(l[groups[[i]],]),centre=colMeans(l[groups[[i]],]),level=overlaySize),border=color[i],col=fillCols[i])
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
            legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n') 
            if (gray)
            {
              legend(1.2 + 0.5 * 2.4/GLratio,-0.5,paste("p <",alpha[length(alpha):1]),
                     col = c(rgb(0.7,0.7,0.7),rgb(0.5,0.5,0.5),rgb(0.3,0.3,0.3),"black")[(5-length(alpha)):4],
                     lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            } else
            {
              if (any(Pvals < 0))
              {
                legend(1.2 + 0.25 * 2.4/GLratio,-0.5,paste("p <",alpha[length(alpha):1]),
                       col = c("cadetblue1","#6495ED","blue","darkblue")[(5-length(alpha)):4],
                       lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
                
                legend(1.2 + 0.75 * 2.4/GLratio,-0.5,paste("p <",alpha[length(alpha):1]),
                       col = c(rgb(1,0.8,0.4) ,"orange","darkorange","darkorange2")[(5-length(alpha)):4],
                       lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
                
              } else
              {
                legend(1.2 + 0.5 * 2.4/GLratio,-0.5,paste("p <",alpha[length(alpha):1]),
                       col = c("cadetblue1","#6495ED","blue","darkblue")[(5-length(alpha)):4],
                       lty=1, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
              }
            }
          } else
          {
            legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
            legend (1.2 + 0.5 * 2.4/GLratio,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n') 
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
      
      
      if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff','tex')) 
      {
        message(paste("Output stored in ",getwd(),"/",filename,".",filetype,sep=""))
        dev.off()
      }
      par(mar=marOrig, bg=bgOrig)
    }
    # Make output list:
    #returnval=list(input=input, layout=layout, cut=cut, maximum=maximum, minimum=minimum, groups=groups, weighted=weighted, rescale=rescale, labels=labels, directed=directed, legend=legend, plot=plot, rotation=rotation, layout.control=layout.control, layout.par=layout.par, filetype=filetype, filename=filename, width=width, height=height, pty=pty, res=res, vsize=vsize, esize=esize, color=color, bg=bg, bgcontrol=bgcontrol, bgres=bgres, transparency=transparency, lcolor=lcolor, loop=loop, legend.cex=legend.cex, borders=borders, curve=curve, arrows=arrows, diag=diag, tooltips=tooltips, hyperlinks=hyperlinks)
    
    # Arguments, some changed due to removal edges (included in edgelist)
    returnval=arguments
    returnval$layout=layout
    returnval$weighted <- weighted
    returnval$layout.orig=original.layout
    returnval$nNodes <- nNodes
    returnval$directed <- NULL
    returnval$bidirectional <- NULL
    returnval$background <- background
    
    # Graph attributes (only used for igraph exportation):
    graphAttributes <- list(
      Nodes = list(
        border.color = bcolor,
        borders = borders,
        label.cex = label.cex,
        label.color = lcolor,
        labels = labels,
        loopRotation = loopRotation,
        shape = shape,
        color = vertex.colors,
        width = vsize,
        height = vsize2,
        stringsAsFactors=FALSE
      ),
      Edges = list(
        curve = curve,
        color = edge.color,
        labels = edge.labels,
        label.cex = edge.label.cex,
        label.color = ELcolor,
        width = edge.width,
        lty = lty,
        stringsAsFactors=FALSE
      ),
      edgesort = edgesort
    )
    
    # Edgelist:
    E$directed <- directed
    E$bidir <- bidirectional
    E <- as.data.frame(E)
    class(E) <- "qgraphEdgelist"
    
    returnval$qgraphEdgelist <- E
    returnval$graphAttributes <- graphAttributes
    
    class(returnval)="qgraph"
    #par(parOrig)
    
    invisible(returnval)
  }
}

