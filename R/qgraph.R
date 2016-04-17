# Create qgraph model:

qgraph <- function( input, ... )
{
  
  # OTHER INPUT MODES: 
  if (any(class(input)=="factanal") )
  {
    return(qgraph.efa(input,...))
  } else if (any(class(input)=="principal") )
  {
    return(qgraph.pca(input,...))
  } else if (any(class(input)=="lavaan"))
  {
    return(qgraph.lavaan(input,edge.labels=TRUE,include=8,filetype="",...))
  } else if (any(class(input)=="sem"))
  {
    return(qgraph.sem(input,edge.labels=TRUE,include=6,filetype="",...))
  } else if (any(class(input)=="loadings"))
  {
    return(qgraph.loadings(input,...))
  }  else if (any(class(input)=="semmod"))
  {
    return(qgraph.semModel(input,...))
  } else if (is.list(input) && identical(names(input),c("Bhat", "omega", "lambda1", "lambda2")))
  {
    layout(t(1:2))
    
    Q1 <- qgraph((input$omega + t(input$omega) ) / 2,...)
    Q2 <- qgraph(input$Bhat,...)
    
    return(list(Bhat = Q1, omega = Q2))
  }
  
  
  ### EMPTY QGRAPH OBJECT ####
  qgraphObject <- list(
    Edgelist = list(),
    Arguments = list(),
    plotOptions = list(),
    graphAttributes = list(
      Nodes = list(),
      Edges = list(),
      Graph = list()
    ),
    layout = matrix(),
    layout.orig = matrix()
  )
  
  class(qgraphObject) <- "qgraph"
  
  ### Extract nested arguments ###
  if ("qgraph"%in%class(input)) qgraphObject$Arguments <- list(...,input) else qgraphObject$Arguments <- list(...)
  
  if (isTRUE(qgraphObject$Arguments[['gui']]) | isTRUE(qgraphObject$Arguments[['GUI']])) 
  {
    qgraphObject$Arguments$gui <- qgraphObject$Arguments$GUI <- FALSE
    return(invisible(do.call(qgraph.gui,c(list(input=input),qgraphObject$Arguments))))
  }
  
  if(!is.null(qgraphObject$Arguments$adj))
  {
    stop("'adj' argument is no longer supported. Please use 'input'")
  }
  
  # Import qgraphObject$Arguments:
  if (length(qgraphObject$Arguments) > 0) qgraphObject$Arguments <- getArgs(qgraphObject$Arguments)
  
  # Import default arguments:
  def <-  getOption("qgraph")
  if (!is.null(def$qgraph)) class(def$qgraph) <- "qgraph"
  if (any(sapply(def,function(x)!is.null(x))))
  {
    qgraphObject$Arguments <- getArgs(c(qgraphObject$Arguments,def))
  }
  
  # If qgraph object is used as input, recreate edgelist input:
  if ("qgraph"%in%class(input)) 
  {
    if (is.null(qgraphObject$Arguments$directed)) qgraphObject$Arguments$directed <- input$Edgelist$directed
    if (is.null(qgraphObject$Arguments$bidirectional)) qgraphObject$Arguments$bidirectional <- input$Edgelist$bidirectional
    if (is.null(qgraphObject$Arguments$nNodes)) qgraphObject$Arguments$nNodes <- input$graphAttributes$Graph$nNodes
    
    
    if(input[['graphAttributes']][['Graph']][['weighted']])
    {
      input <- cbind(input$Edgelist$from,input$Edgelist$to,input$Edgelist$weight)
    } else
    {
      input <- cbind(input$Edgelist$from,input$Edgelist$to)
    }
    qgraphObject$Arguments$edgelist <- TRUE
  }
  
  ### PCALG AND GRAPHNEL ###
  if (any(c("graphNEL","pcAlgo")  %in% class(input)  ))
  {
    if (class(input) == "pcAlgo") graphNEL <- input@graph else graphNEL <- input
    qgraphObject$Arguments$directed <- graphNEL@graphData$edgemode == "directed"
    qgraphObject$Arguments$bidirectional <- TRUE
    TempLabs  <- graphNEL@nodes
    if (is.null(qgraphObject$Arguments$labels)) qgraphObject$Arguments$labels  <- graphNEL@nodes
    weights <- sapply(graphNEL@edgeData@data,'[[','weight')
    
    EL <- laply(strsplit(names(weights),split="\\|"),'[',c(1,2))
    #       EL <- apply(EL,2,as.numeric)
    EL[,1] <- match(EL[,1],TempLabs)
    EL[,2] <- match(EL[,2],TempLabs)
    mode(EL) <- "numeric"
    # Create mixed graph if pcAlgo:
    if ("pcAlgo" %in% class(input))
    {
      srtInput <- aaply(EL,1,sort)
      qgraphObject$Arguments$directed <- !(duplicated(srtInput)|duplicated(srtInput,fromLast=TRUE))
      rm(srtInput)
    }
    input <- EL
    rm(EL)
    if (any(weights!=1)) input <- cbind(input,weights)
    qgraphObject$Arguments$edgelist <- TRUE
  }
  ### bnlearn ###
  if (is(input,"bn"))
  {
    bnobject <- input
    input <- as.matrix(bnobject$arcs)
    TempLabs  <- names(bnobject$nodes)
    if (is.null(qgraphObject$Arguments$labels)) qgraphObject$Arguments$labels  <- TempLabs
    
    input[] <- as.numeric(match(c(input), TempLabs))
    mode(input) <- "numeric"
    
    srtInput <- aaply(input,1,sort)
    input <- input[!duplicated(srtInput),]
    qgraphObject$Arguments$directed <- !(duplicated(srtInput)|duplicated(srtInput,fromLast=TRUE))
    qgraphObject$Arguments$directed <- qgraphObject$Arguments$directed[!duplicated(srtInput)]
  }
  if (is(input,"bn.strength"))
  {
    
    bnobject <- input
    input <- as.matrix(bnobject[c("from","to","strength")])
    TempLabs  <- unique(c(bnobject$from,bnobject$to))
    if (is.null(qgraphObject$Arguments$labels)) qgraphObject$Arguments$labels  <- TempLabs
    
    input[,1:2] <- as.numeric(match(c(input[,1:2]), TempLabs))
    
    input <- as.matrix(input)
    mode(input) <- "numeric"
    
    if (is.null(qgraphObject$Arguments$directed))
    {
      if (is.null(bnobject$direction) || all(bnobject$direction %in% c(0,0.5)))
      { 
        qgraphObject$Arguments$directed <- FALSE
      } else qgraphObject$Arguments$directed <- TRUE
    }
    
    if (!is.null(bnobject$direction))
    {
      input[,3] <- input[,3] * ( 1 - qgraphObject$Arguments$directed * (1- bnobject$direction ))
    }
    
    # remove undirect duplicates:
    srt <- cbind( pmin(input[,1],input[,2]), pmax(input[,1],input[,2]))
    input <- input[!(duplicated(srt)&!qgraphObject$Arguments$directed),  ]
    rm(srt)
    
    #     srtInput <- aaply(input,1,sort)
    #     input <- input[!duplicated(srtInput),]
    #     qgraphObject$Arguments$directed <- !(duplicated(srtInput)|duplicated(srtInput,fromLast=TRUE))
    #     qgraphObject$Arguments$directed <- qgraphObject$Arguments$directed[!duplicated(srtInput)]
    
    qgraphObject$Arguments$directed <- TRUE
    
    qgraphObject$Arguments$probabilityEdges <- TRUE
    
    if (is.null( qgraphObject$Arguments$parallelEdge))  qgraphObject$Arguments$parallelEdge <- TRUE
    
  }
  
  ### BDgraph ####
  if (is(input,"bdgraph"))
  {
    
    stop("BDgraph support has temporarily been removed")
    
    #     if(is.null(qgraphObject$Arguments[['BDgraph']])) BDgraph=c("phat","Khat") else BDgraph=qgraphObject$Arguments[['BDgraph']]
    #     if (all(c("Khat","phat")%in%BDgraph)) layout(t(1:2))
    #     
    #     if(is.null(qgraphObject$Arguments[['BDtitles']])) BDtitles <- TRUE else BDtitles <- qgraphObject$Arguments[['BDtitles']]
    #     
    #     
    #     Res <- list()
    #     
    #     if (isTRUE(which(BDgraph == "phat") < which(BDgraph == "Khat")))
    #     {
    #       # phat:
    #       W <- phat(input)
    #       W <- W + t(W)
    #       Res[["phat"]] <- do.call(qgraph,c(list(input=W,probabilityEdges = TRUE),qgraphObject$Arguments))
    #       L <- Res[["phat"]]$layout
    #       
    #       if (BDtitles) text(mean(par('usr')[1:2]),par("usr")[4] - (par("usr")[4] - par("usr")[3])/40,"Posterior probabilities", adj = c(0.5,1))     
    #       
    #       # Khat:
    #       W <- input$Khat
    #       diag(W) <- -1*diag(W)
    #       W <-  - W / sqrt(diag(W)%o%diag(W))
    #       Res[["Khat"]] <- do.call(qgraph,c(list(input = W,layout = L), qgraphObject$Arguments))
    #       L <- Res[["Khat"]]$layout
    #       if (BDtitles) text(mean(par('usr')[1:2]),par("usr")[4] - (par("usr")[4] - par("usr")[3])/40,"Mean partial correlations", adj = c(0.5,1))
    #       
    #     } else 
    #     {
    #       if ("Khat" %in% BDgraph)
    #       {
    #         W <- input$Khat
    #         diag(W) <- -1*diag(W)
    #         W <-  - W / sqrt(diag(W)%o%diag(W))
    #         Res[["Khat"]] <- do.call(qgraph,c(list(input=W),qgraphObject$Arguments))
    #         L <- Res[["Khat"]]$layout
    #         if (BDtitles) text(mean(par('usr')[1:2]),par("usr")[4],"Mean partial correlations", adj = c(0.5,1))
    #       } else L <- qgraphObject$Arguments$layout
    #       
    #       if ("phat" %in% BDgraph)
    #       {
    #         W <- phat(input)
    #         W <- W + t(W)
    #         Res[["phat"]] <- do.call(qgraph,c(list(input = W,layout = L,probabilityEdges= TRUE), qgraphObject$Arguments))
    #         if (BDtitles) text(mean(par('usr')[1:2]),par("usr")[4],"Posterior probabilities", adj = c(0.5,1))
    #       }
    #     }  
    #     
    #     if (length(Res)==1) Res <- Res[[1]]
    #     return(Res)
    
  }
  
  
  ### GLASSO ###
  # glasso has no class but is a list with elements w, wi, loglik, errflag, approx, del and niter:
  if (is(input, "list") && all(c('w', 'wi', 'loglik','errflag', 'approx', 'del',  'niter' ) %in% names(input)))
  {
    input <- wi2net(input$wi)
  }
  
  if(is.null(qgraphObject$Arguments[['verbose']]))
  {
    verbose <- FALSE
  } else verbose <- qgraphObject$Arguments[['verbose']] 
  
  if(is.null(qgraphObject$Arguments[['tuning']]))
  {
    tuning <- 0.5
  } else tuning <- qgraphObject$Arguments[['tuning']]  
  
  
  if(is.null(qgraphObject$Arguments[['FDRcutoff']]))
  {
    FDRcutoff <- 0.9
  } else FDRcutoff <- qgraphObject$Arguments[['FDRcutoff']]  
  
  
  ### HUGE (select via EBIC):
  if (is(input,"huge"))
  {
    if (input$method != "glasso") stop("Only 'glasso' method is supported")
    input <- huge.select(input, "ebic", ebic.gamma = tuning)
  }
  
  ### HUGE select ###
  if (is(input,"select"))
  {
    if (input$method != "glasso") stop("Only 'glasso' method is supported")
    input <- wi2net(forceSymmetric(input$opt.icov))
  }
  
  # Coerce input to matrix:
  input <- as.matrix(input)
  
  # Set mode:
  sigSign <- FALSE
  if(is.null(qgraphObject$Arguments[['graph']])) graph <- "default" else graph=qgraphObject$Arguments[['graph']]
  if (graph == "cor")
  {
    graph <- "assosciation"
  }
  if (graph == "pcor")
  {
    graph <- "concentration"
  }
  if (graph == "fdr")
  {
    graph <- "fdr.cor"
  }
  
  # Reset graph for replotting:
  qgraphObject$Arguments[['graph']] <- NULL
  
  if (graph %in% c("sig2","significance2"))
  {
    graph <- "sig"
    sigSign <- TRUE
  }
  if (graph %in% c("sig","significance"))
  {
#     if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?") 
    qgraphObject$Arguments[['mode']] <- "sig"
  }
  
### SIGNIFICANCE GRAPH ARGUMENTS ###
  if(is.null(qgraphObject$Arguments[['mode']])) mode <- "strength" else mode <- qgraphObject$Arguments[['mode']]
  if(is.null(qgraphObject$Arguments$sigScale)) sigScale <- function(x)0.7*(1-x)^(log(0.4/0.7,1-0.05)) else sigScale <- qgraphObject$Arguments$sigScale
  if (!mode%in%c("strength","sig","direct")) stop("Mode must be 'direct', 'sig' or 'strength'")	
  if(is.null(qgraphObject$Arguments$bonf)) bonf=FALSE else bonf=qgraphObject$Arguments$bonf
  if(is.null(qgraphObject$Arguments$OmitInsig)) OmitInsig=FALSE else OmitInsig <- qgraphObject$Arguments$OmitInsig
if(is.null(qgraphObject$Arguments[['alpha']]))
{
  if (mode != "sig")
  {
    alpha <- 0.05
  } else alpha <- c(0.0001,0.001,0.01,0.05) 
} else alpha <- qgraphObject$Arguments[['alpha']]
if (length(alpha) > 4) stop("`alpha' can not have length > 4")


#####
  # Settings for the edgelist
  if(is.null(qgraphObject$Arguments$edgelist)) 
  {
    if (nrow(input)!=ncol(input)) {
      # Check if it is an edgelist or break:
      if (ncol(input) %in% c(2,3) && ((is.character(input[,1]) || is.factor(input[,1])) || all(input[,1] %% 1 == 0)) &&
            ((is.character(input[,2]) || is.factor(input[,2])) || all(input[,2] %% 1 == 0))){
        edgelist <- TRUE
      } else {
        stop("Input is not a weights matrix or an edgelist.")
      }
    } else edgelist <- FALSE
  } else edgelist=qgraphObject$Arguments$edgelist
  
  if(is.null(qgraphObject$Arguments[['edgeConnectPoints']])) edgeConnectPoints <- NULL else edgeConnectPoints <- qgraphObject$Arguments[['edgeConnectPoints']]
  
  
  if(is.null(qgraphObject$Arguments$labels))
  {
    labels <- TRUE
    if (!edgelist && !is.null(colnames(input)))
    {
      #       if (nrow(input) <= 20 & all(colnames(input)==rownames(input)))
      #       {
      labels <- iconv(abbreviate(colnames(input),3))
      if (any(is.na(labels))){
        warning("Some labels where not abbreviatable.")
        labels <- ifelse(is.na(labels), colnames(input), labels)
      }
      #       }
    }
  } else labels <- qgraphObject$Arguments$labels

  if (is.expression(labels)) labels <- as.list(labels)
  
  if(is.null(qgraphObject$Arguments[['background']])) background <- NULL else background <- qgraphObject$Arguments[['background']]
  if(is.null(qgraphObject$Arguments[['label.prop']])) label.prop <- 0.9 else label.prop <- qgraphObject$Arguments[['label.prop']]
  if(is.null(qgraphObject$Arguments[['label.norm']])) label.norm <- "OOO" else label.norm <- qgraphObject$Arguments[['label.norm']]
  if(is.null(qgraphObject$Arguments[['label.cex']])) label.cex <- NULL else label.cex <- qgraphObject$Arguments[['label.cex']]
  
  if(is.null(qgraphObject$Arguments[['font']])) font <- 1 else font <- qgraphObject$Arguments[['font']]
  
  if(is.null(qgraphObject$Arguments[['label.font']])) label.font <- font else label.font <- qgraphObject$Arguments[['label.font']]
  
  if(is.null(qgraphObject$Arguments[['nodeNames']])) nodeNames <- NULL else nodeNames <- qgraphObject$Arguments[['nodeNames']]
  
  if(is.null(qgraphObject$Arguments[['subplots']])) subplots <- NULL else subplots <- qgraphObject$Arguments[['subplots']]
  if(is.null(qgraphObject$Arguments[['subpars']])) subpars <- list(mar=c(0,0,0,0)) else subpars <- qgraphObject$Arguments[['subpars']]
  
  if(is.null(qgraphObject$Arguments[['subplotbg']])) subplotbg <- NULL else subplotbg <- qgraphObject$Arguments[['subplotbg']]
  
  if(is.null(qgraphObject$Arguments[['images']])) images <- NULL else images <- qgraphObject$Arguments[['images']]

if(is.null(qgraphObject$Arguments[['noPar']])) noPar <- FALSE else noPar <- qgraphObject$Arguments[['noPar']]
  
  # Knots:
  if(is.null(qgraphObject$Arguments[['knots']])) knots <- list() else knots <- qgraphObject$Arguments[['knots']]
  if(is.null(qgraphObject$Arguments[['knot.size']])) knot.size <- 1 else knot.size <- qgraphObject$Arguments[['knot.size']]
  if(is.null(qgraphObject$Arguments[['knot.color']])) knot.color <- NA else knot.color <- qgraphObject$Arguments[['knot.color']]
  if(is.null(qgraphObject$Arguments[['knot.borders']])) knot.borders <- FALSE else knot.borders <- qgraphObject$Arguments[['knot.borders']]
  if(is.null(qgraphObject$Arguments[['knot.border.color']])) knot.border.color <- "black" else knot.border.color <- qgraphObject$Arguments[['knot.border.color']]
  if(is.null(qgraphObject$Arguments[['knot.border.width']])) knot.border.width <- 1 else knot.border.width <- qgraphObject$Arguments[['knot.border.width']]
  
  if (edgelist)
  {
    if (is.character(input))
    {
      if(!is.logical(labels)) allNodes <- labels else allNodes <- unique(c(input[,1:2]))
      input[,1:2] <- match(input[,1:2],allNodes)
      input <- as.matrix(input)
      mode(input) <- "numeric"
      if (is.logical(labels) && labels) labels <- allNodes
    }
  }
  
  if(is.null(qgraphObject$Arguments$nNodes)) 
  {
    if (edgelist)
    {
      if (!is.logical(labels)) nNodes <- length(labels) else nNodes <- max(c(input[,1:2])) 
    } else nNodes=nrow(input)
  } else nNodes=qgraphObject$Arguments$nNodes
  
  
  if(is.null(qgraphObject$Arguments[['usePCH']])) 
  {
    if (nNodes > 50) usePCH <- TRUE else usePCH <- NULL 
  } else usePCH <- qgraphObject$Arguments[['usePCH']]
  
  if(is.null(qgraphObject$Arguments[['node.resolution']])) node.resolution <- 100 else node.resolution <- qgraphObject$Arguments[['node.resolution']]
  
  
  # Default for fact cut and groups
  if (graph=="factorial") fact=TRUE else fact=FALSE
  if (fact & edgelist) stop('Factorial graph needs a correlation matrix')
  #     if (graph=="concentration") partial=TRUE else partial=FALSE
  
  #   if(is.null(qgraphObject$Arguments$cutQuantile)) cutQuantile <- 0.9 else cutQuantile <- qgraphObject$Arguments$cutQuantile
  defineCut <- FALSE
  if(is.null(qgraphObject$Arguments[['cut']])) 
  {
    cut=0
    #       if (nNodes<50) 
    if (nNodes>=20 | fact) 
    {
      cut=0.3
      defineCut <- TRUE
    }
    if (mode=="sig") cut <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)-1]),sigScale(alpha[length(alpha)]))
  } else if (mode != "sig") cut <- ifelse(is.na(qgraphObject$Arguments[['cut']]),0,qgraphObject$Arguments[['cut']]) else cut <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)-1]),sigScale(alpha[length(alpha)]))
  
  if(is.null(qgraphObject$Arguments$groups)) groups=NULL else groups=qgraphObject$Arguments$groups
  
  if (is.factor(groups) | is.character(groups)) groups <- tapply(1:length(groups),groups,function(x)x)
  
  

  
  
  # Factorial graph:
  if(is.null(qgraphObject$Arguments$nfact))
  {
    nfact=NULL
  } else nfact=qgraphObject$Arguments$nfact
  
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
  
  # Glasso arguments:
  if(is.null(qgraphObject$Arguments[['sampleSize']]))
  {
    sampleSize <- NULL
  } else sampleSize <- qgraphObject$Arguments[['sampleSize']]
  
  if(is.null(qgraphObject$Arguments[['countDiagonal']]))
  {
    countDiagonal <- FALSE
  } else countDiagonal <- qgraphObject$Arguments[['countDiagonal']]
  
  

  # SET DEFAULT qgraphObject$Arguments:
  # General qgraphObject$Arguments:
  if(is.null(qgraphObject$Arguments$DoNotPlot)) DoNotPlot=FALSE else DoNotPlot=qgraphObject$Arguments$DoNotPlot
  if(is.null(qgraphObject$Arguments[['layout']])) layout=NULL else layout=qgraphObject$Arguments[['layout']]
  if(is.null(qgraphObject$Arguments$maximum)) maximum=0 else maximum=qgraphObject$Arguments$maximum
  if(is.null(qgraphObject$Arguments$minimum))
  {
#     if (nNodes<50)  minimum=0
#     if (nNodes>=50)  minimum=0.1
    minimum <- 0
    if (mode=="sig") minimum <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)]),0)
  } else 
  {
    if (mode!="sig") minimum=qgraphObject$Arguments$minimum else minimum <- ifelse(length(alpha)>1,sigScale(alpha[length(alpha)]),0)
    if (is.character(minimum))
    {
      if (grepl("sig",minimum,ignore.case = TRUE))
      {
        if (is.null(sampleSize))
        {
          stop("'sampleSize' argument must be assigned to use significance as minimum")
        }
        if (graph == "default")
        {
          warning("'graph' argument did not specify type of graph. Assuming correlation graph (graph = 'assosciation')")
          graph <- "assosciation"
        }
        if (graph %in% c("assosciation","concentration")) {
          # Find threshold for significance!
          # difference between cor and pcor is in df:
          if (graph == "assosciation")
          {
            df <- sampleSize - 2
          } else {
            df <- sampleSize - 2 - (nNodes - 2)
          }
          siglevel <- max(alpha)/2
          if (bonf)
          {
            siglevel <- siglevel / (nNodes*(nNodes-1)/2)
          }
          t <- abs(qt(siglevel, df, lower.tail=TRUE))
          minimum <- t/sqrt(t^2+df) 
        } else stop("minimum = 'sig' is not supported with this 'graph' argument")
        
      } else stop("Minimum is specified a string which is not 'sig'.")
    }
  }
  if (minimum < 0)
  {
    warning("'minimum' set to absolute value")
    minimum <- abs(minimum)
  }

  # Threshold argument removes edges from network:
  if(is.null(qgraphObject$Arguments[['threshold']]))
  {
    threshold <- 0
  } else {
    threshold <- qgraphObject$Arguments[['threshold']]
  }

  if(is.null(qgraphObject$Arguments$weighted)) weighted=NULL else weighted=qgraphObject$Arguments$weighted
  if(is.null(qgraphObject$Arguments$rescale)) rescale=TRUE else rescale=qgraphObject$Arguments$rescale
  if(is.null(qgraphObject$Arguments[['edge.labels']])) edge.labels=FALSE else edge.labels=qgraphObject$Arguments[['edge.labels']]
  if(is.null(qgraphObject$Arguments[['edge.label.font']])) edge.label.font=font else edge.label.font=qgraphObject$Arguments[['edge.label.font']]
  if(is.null(qgraphObject$Arguments[['edge.label.bg']])) edge.label.bg=TRUE else edge.label.bg=qgraphObject$Arguments[['edge.label.bg']]
  if (identical(FALSE,edge.label.bg)) plotELBG <- FALSE else plotELBG <- TRUE
  
  if(is.null(qgraphObject$Arguments[['posCol']])) posCol <- c("#009900","darkgreen") else posCol <- qgraphObject$Arguments[['posCol']]
  if (length(posCol)==1) posCol <- rep(posCol,2)
  if (length(posCol)!=2) stop("'posCol' must be of length 1 or 2.")
  
  if(is.null(qgraphObject$Arguments[['negCol']])) negCol <- c("#BF0000","red") else negCol <- qgraphObject$Arguments[['negCol']]
  if (length(negCol)==1) negCol <- rep(negCol,2)
  if (length(negCol)!=2) stop("'negCol' must be of length 1 or 2.")
  
  if(is.null(qgraphObject$Arguments[['probCol']])) probCol <- "blue" else probCol <- qgraphObject$Arguments[['probCol']]
  if(!is.null(qgraphObject$Arguments[['probabilityEdges']])) 
  {
    if (isTRUE(qgraphObject$Arguments[['probabilityEdges']]))
    {
      posCol <- probCol
    }
  }
  
  
  if(is.null(qgraphObject$Arguments[['unCol']])) unCol <- "#808080" else unCol <- qgraphObject$Arguments[['unCol']] 
  
  if(is.null(qgraphObject$Arguments[['colFactor']])) colFactor <- 1 else colFactor <- qgraphObject$Arguments[['colFactor']]
  
  if(is.null(qgraphObject$Arguments[['edge.color']])) edge.color <- NULL else edge.color=qgraphObject$Arguments[['edge.color']]
  if(is.null(qgraphObject$Arguments[['edge.label.cex']])) edge.label.cex=1 else edge.label.cex=qgraphObject$Arguments[['edge.label.cex']]
  if(is.null(qgraphObject$Arguments[['edge.label.position']])) edge.label.position <- 0.5 else edge.label.position=qgraphObject$Arguments[['edge.label.position']]
  
  
  if(is.null(qgraphObject$Arguments$directed))
  {
    if (edgelist) directed=TRUE else directed=NULL 
  } else directed=qgraphObject$Arguments$directed
  if(is.null(qgraphObject$Arguments[['legend']]))
  {
    if ((!is.null(groups) & !is.null(names(groups))) | !is.null(nodeNames)) legend <- TRUE else legend <- FALSE
  } else legend <- qgraphObject$Arguments[['legend']]
  
  stopifnot(is.logical(legend))
  
  #     if (is.null(groups)) legend <- FALSE
  if(is.null(qgraphObject$Arguments$plot)) plot=TRUE else plot=qgraphObject$Arguments$plot
  if(is.null(qgraphObject$Arguments$rotation)) rotation=NULL else rotation=qgraphObject$Arguments$rotation
  if(is.null(qgraphObject$Arguments[['layout.control']])) layout.control=0.5 else layout.control=qgraphObject$Arguments[['layout.control']]
  
  # repulsion controls the repulse.rad argument
  if(is.null(qgraphObject$Arguments[['repulsion']])) repulsion=1 else repulsion=qgraphObject$Arguments[['repulsion']]
  if(is.null(qgraphObject$Arguments[['layout.par']])) {
    if (is.null(layout) || identical(layout,"spring")) layout.par <- list(repulse.rad = nNodes^(repulsion * 3))  else layout.par <- list()
    } else layout.par=qgraphObject$Arguments[['layout.par']]
  if(is.null(qgraphObject$Arguments$details)) details=FALSE else details=qgraphObject$Arguments$details
  if(is.null(qgraphObject$Arguments$title)) title <- NULL else title <- qgraphObject$Arguments$title
  if(is.null(qgraphObject$Arguments$preExpression)) preExpression <- NULL else preExpression <- qgraphObject$Arguments$preExpression
  if(is.null(qgraphObject$Arguments$postExpression)) postExpression <- NULL else postExpression <- qgraphObject$Arguments$postExpression
  
  
  # Output qgraphObject$Arguments:
  if(is.null(qgraphObject$Arguments$bg)) bg <- FALSE else bg <- qgraphObject$Arguments$bg
  
  if(is.null(qgraphObject$Arguments[['edge.label.color']])) ELcolor <- NULL else ELcolor <- qgraphObject$Arguments[['edge.label.color']]
  
  if(is.null(qgraphObject$Arguments[['border.color']])) {
    if(is.null(qgraphObject$Arguments[['border.colors']])) bcolor <- NULL else bcolor <- qgraphObject$Arguments[['border.colors']]
  } else bcolor <- qgraphObject$Arguments[['border.color']]
  
  if(is.null(qgraphObject$Arguments[['border.width']])) border.width <- 1 else border.width <- qgraphObject$Arguments[['border.width']]
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
  
  if(is.null(qgraphObject$Arguments$filetype)) filetype="default" else filetype=qgraphObject$Arguments$filetype
  if(is.null(qgraphObject$Arguments$filename)) filename="qgraph" else filename=qgraphObject$Arguments$filename
  if(is.null(qgraphObject$Arguments$width)) width <- 7 else width <- qgraphObject$Arguments[['width']]
  if(is.null(qgraphObject$Arguments$height)) height <- 7 else height <- qgraphObject$Arguments[['height']]
  if(is.null(qgraphObject$Arguments$pty)) pty='m' else pty=qgraphObject$Arguments$pty
  if(is.null(qgraphObject$Arguments$res)) res=320 else res=qgraphObject$Arguments$res
  if(is.null(qgraphObject$Arguments[['normalize']])) normalize <- TRUE else normalize <- qgraphObject$Arguments[['normalize']]
  
  # Graphical qgraphObject$Arguments
  #     defNodeSize <- max((-1/72)*(nNodes)+5.35,1) ### Default node size, used as standard unit.
  if(is.null(qgraphObject$Arguments[['mar']])) mar <- c(3,3,3,3)/10 else mar <- qgraphObject$Arguments[["mar"]]/10
  if(is.null(qgraphObject$Arguments[['vsize']])) 
  {
    vsize <- 8*exp(-nNodes/80)+1
    #     vsize <- max((-1/72)*(nNodes)+5.35,1)
    if(is.null(qgraphObject$Arguments[['vsize2']])) vsize2 <- vsize else vsize2 <- vsize * qgraphObject$Arguments[['vsize2']]
  } else {
    vsize <- qgraphObject$Arguments[['vsize']]
    if(is.null(qgraphObject$Arguments[['vsize2']])) vsize2 <- vsize else vsize2 <- qgraphObject$Arguments[['vsize2']]
  }
  
  if(!is.null(qgraphObject$Arguments[['node.width']])) 
  {
    vsize <- vsize * qgraphObject$Arguments[['node.width']]
  }
  
  if(!is.null(qgraphObject$Arguments[['node.height']])) 
  {
    vsize2 <- vsize2 * qgraphObject$Arguments[['node.height']]
  }
  
  if(is.null(qgraphObject$Arguments$color)) color=NULL else color=qgraphObject$Arguments$color
  
  if(is.null(qgraphObject$Arguments[['gray']])) gray <- FALSE else gray <- qgraphObject$Arguments[['gray']]
  
  if (gray) posCol <- negCol <- c("gray10","black")
  
  if(is.null(qgraphObject$Arguments[['pastel']])) pastel <- FALSE else pastel <- qgraphObject$Arguments[['pastel']]
  if(is.null(qgraphObject$Arguments[['rainbowStart']])) rainbowStart <- 0 else rainbowStart <- qgraphObject$Arguments[['rainbowStart']]
  
  if(is.null(qgraphObject$Arguments$bgcontrol)) bgcontrol=6 else bgcontrol=qgraphObject$Arguments$bgcontrol
  if(is.null(qgraphObject$Arguments$bgres)) bgres=100 else bgres=qgraphObject$Arguments$bgres
  if(is.null(qgraphObject$Arguments[['trans',exact=FALSE]])) transparency <- NULL else transparency <- qgraphObject$Arguments[['trans',exact=FALSE]]
  if (is.null(transparency))
  {
    if (isTRUE(bg)) transparency <- TRUE else transparency <- FALSE
  }
  if(is.null(qgraphObject$Arguments[['fade']])) fade <- TRUE else fade <- qgraphObject$Arguments[['fade']]
  if(is.null(qgraphObject$Arguments[['loop']])) loop=1 else loop=qgraphObject$Arguments[['loop']]
  if(is.null(qgraphObject$Arguments[['loopRotation']]))
  {
    loopRotation <- NA
  } else {
    loopRotation=qgraphObject$Arguments[['loopRotation']]
  }
  
  if(is.null(qgraphObject$Arguments[['residuals']])) residuals=FALSE else residuals=qgraphObject$Arguments[['residuals']]
  if(is.null(qgraphObject$Arguments[['residScale']])) residScale=1 else residScale=qgraphObject$Arguments[['residScale']]
  if(is.null(qgraphObject$Arguments[['residEdge']])) residEdge=FALSE else residEdge=qgraphObject$Arguments[['residEdge']]
  
  if(is.null(qgraphObject$Arguments[['bars']])) bars <- list() else bars <- qgraphObject$Arguments[['bars']]
  if(is.null(qgraphObject$Arguments[['barSide']])) barSide <- 1 else barSide <- qgraphObject$Arguments[['barSide']]
  if(is.null(qgraphObject$Arguments[['barLength']])) barLength <- 0.5 else barLength <- qgraphObject$Arguments[['barLength']]
  if(is.null(qgraphObject$Arguments[['barColor']])) barColor <- 'border' else barColor <- qgraphObject$Arguments[['barColor']]
  if(is.null(qgraphObject$Arguments[['barsAtSide']])) barsAtSide <- FALSE else barsAtSide <- qgraphObject$Arguments[['barsAtSide']]
  
  # Means and SDs:
  if(is.null(qgraphObject$Arguments[['means']])) means <- NA else means <- qgraphObject$Arguments[['means']]
  if(is.null(qgraphObject$Arguments[['SDs']])) SDs <- NA else SDs <- qgraphObject$Arguments[['SDs']]
  if(is.null(qgraphObject$Arguments[['meanRange']])) {
      if (all(is.na(means))) meanRange <- c(NA,NA) else meanRange <- range(means,na.rm=TRUE) 
    }else meanRange <- qgraphObject$Arguments[['meanRange']]
  
  
  if (!is.list(bars)) bars <- as.list(bars)
  
  if(is.null(qgraphObject$Arguments[['CircleEdgeEnd']])) CircleEdgeEnd=FALSE else CircleEdgeEnd=qgraphObject$Arguments[['CircleEdgeEnd']]
  if(is.null(qgraphObject$Arguments[['loopAngle']])) loopangle=pi/2 else loopAngle=qgraphObject$Arguments[['loopAngle']]
  if(is.null(qgraphObject$Arguments[['legend.cex']])) legend.cex=0.6 else legend.cex=qgraphObject$Arguments[['legend.cex']]
  if(is.null(qgraphObject$Arguments[['legend.mode']]))
  {
    if (!is.null(nodeNames) && !is.null(groups)){
      legend.mode <- "style1" # or style2
    } else if (!is.null(nodeNames)) legend.mode <- "names" else legend.mode <- "groups"
  }  else legend.mode=qgraphObject$Arguments[['legend.mode']]
  
  if(is.null(qgraphObject$Arguments$borders)) borders=TRUE else borders=qgraphObject$Arguments$borders
  if(is.null(qgraphObject$Arguments$shape)) shape="circle" else shape=qgraphObject$Arguments$shape
  
  
  ### Polygon lookup list:
  polygonList = list(
    ellipse = ELLIPSEPOLY,
    heart  = HEARTPOLY,
    star = STARPOLY
  )
  
  if(!is.null(qgraphObject$Arguments[['polygonList']])) polygonList  <- c( polygonList, qgraphObject$Arguments[['polygonList']])
  
  # Rescale to -1 - 1 and compute radians per point:
  for (i in seq_along(polygonList))
  {
    polygonList[[i]]$x <- (polygonList[[i]]$x - min(polygonList[[i]]$x)) / (max(polygonList[[i]]$x) - min(polygonList[[i]]$x)) * 2 - 1
    polygonList[[i]]$y <- (polygonList[[i]]$y - min(polygonList[[i]]$y)) / (max(polygonList[[i]]$y) - min(polygonList[[i]]$y)) * 2 - 1
  }
  
  if(is.null(qgraphObject$Arguments$label.scale)) label.scale=TRUE else label.scale=qgraphObject$Arguments$label.scale
  if(is.null(qgraphObject$Arguments$scores)) scores=NULL else scores=qgraphObject$Arguments$scores
  if(is.null(qgraphObject$Arguments$scores.range)) scores.range=NULL else scores.range=qgraphObject$Arguments$scores.range
  if(is.null(qgraphObject$Arguments$lty)) lty=1 else lty=qgraphObject$Arguments$lty
  if(is.null(qgraphObject$Arguments$vTrans)) vTrans=255 else vTrans=qgraphObject$Arguments$vTrans
  if(is.null(qgraphObject$Arguments[['overlay']])) overlay <- FALSE else overlay <- qgraphObject$Arguments[['overlay']]
  if(is.null(qgraphObject$Arguments[['overlaySize']])) overlaySize <- 0.5 else overlaySize <- qgraphObject$Arguments[['overlaySize']]
  if(is.null(qgraphObject$Arguments[['GLratio']])) GLratio <- 2.5 else GLratio <- qgraphObject$Arguments[['GLratio']]
  if(is.null(qgraphObject$Arguments$layoutScale)) layoutScale <- 1 else layoutScale <- qgraphObject$Arguments$layoutScale
  if(is.null(qgraphObject$Arguments[['layoutOffset']])) layoutOffset <- 0 else layoutOffset <- qgraphObject$Arguments[['layoutOffset']]
  
  # Aspect ratio:
  if(is.null(qgraphObject$Arguments[['aspect']])) aspect=FALSE else aspect=qgraphObject$Arguments[['aspect']]
  
  # qgraphObject$Arguments for directed graphs:
  if(is.null(qgraphObject$Arguments[['curvePivot']])) curvePivot <- FALSE else curvePivot <- qgraphObject$Arguments[['curvePivot']]
  if (isTRUE(curvePivot)) curvePivot <- 0.1
  if(is.null(qgraphObject$Arguments[['curveShape']])) curveShape <- -1 else curveShape <- qgraphObject$Arguments[['curveShape']]
  if(is.null(qgraphObject$Arguments[['curvePivotShape']])) curvePivotShape <- 0.25 else curvePivotShape <- qgraphObject$Arguments[['curvePivotShape']]
  if(is.null(qgraphObject$Arguments[['curveScale']])) curveScale <- TRUE else curveScale <- qgraphObject$Arguments[['curveScale']]
  
  if(is.null(qgraphObject$Arguments[['parallelEdge']])) parallelEdge <- FALSE else parallelEdge <- qgraphObject$Arguments[['parallelEdge']]
  
  if(is.null(qgraphObject$Arguments[['parallelAngle']])) parallelAngle <- NA else parallelAngle <- qgraphObject$Arguments[['parallelAngle']]
  
  if(is.null(qgraphObject$Arguments[['parallelAngleDefault']])) parallelAngleDefault <- pi/6 else parallelAngleDefault <- qgraphObject$Arguments[['parallelAngleDefault']]
  
  if(is.null(qgraphObject$Arguments[['curveDefault']])) curveDefault <- 1 else curveDefault <- qgraphObject$Arguments[['curveDefault']]
  
  if(is.null(qgraphObject$Arguments[['curve']]))
  {
    if (any(parallelEdge))
    { 
      curve <- ifelse(parallelEdge,0,NA)
    } else curve <- NA 
  } else {      
    curve <- qgraphObject$Arguments[['curve']]
    if (length(curve)==1) 
    {
      curveDefault <- curve
      curve <- NA
    }
  }
  if(is.null(qgraphObject$Arguments[['curveAll']])) curveAll <- FALSE else curveAll <- qgraphObject$Arguments[['curveAll']]
  if (curveAll)
  {
    curve[is.na(curve)] <- curveDefault
  }
  if(is.null(qgraphObject$Arguments$arrows)) arrows=TRUE else arrows=qgraphObject$Arguments$arrows
  #     asize=asize*2.4/height
  if(is.null(qgraphObject$Arguments$open)) open=FALSE else open=qgraphObject$Arguments$open
  if(is.null(qgraphObject$Arguments$bidirectional)) bidirectional=FALSE else bidirectional=qgraphObject$Arguments$bidirectional
  
  # qgraphObject$Arguments for SVG pictures:
  if(is.null(qgraphObject$Arguments$tooltips)) tooltips=NULL else tooltips=qgraphObject$Arguments$tooltips
  if(is.null(qgraphObject$Arguments$SVGtooltips)) SVGtooltips=NULL else SVGtooltips=qgraphObject$Arguments$SVGtooltips
  if(is.null(qgraphObject$Arguments$hyperlinks)) hyperlinks=NULL else hyperlinks=qgraphObject$Arguments$hyperlinks
  
  # qgraphObject$Arguments for TEX:
  if(is.null(qgraphObject$Arguments$standAlone)) standAlone=TRUE else standAlone=qgraphObject$Arguments$standAlone
  
  ### EASTER EGGS ###
  if(is.null(qgraphObject$Arguments[['XKCD']])) XKCD <- FALSE else XKCD <- TRUE
  
  #     # Legend setting 1
  #     if (is.null(legend))
  #     {
  #       if (is.null(groups)) legend=FALSE else legend=TRUE
  #     }
  #if ((legend & filetype!='pdf' & filetype!='eps') | filetype=="svg")
  if ((legend&is.null(scores))|(identical(filetype,"svg")))
  {
    width=width*(1+(1/GLratio))
  }
  
  #     if (!DoNotPlot)
  #     {
  #       
  #       # Start output:
  #       if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width,height=height)
  #       if (filetype=='R') dev.new(rescale="fixed",width=width,height=height)
  #       if (filetype=='X11' | filetype=='x11') x11(width=width,height=height)
  #       if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height,width=width, horizontal=FALSE)
  #       if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height,width=width)
  #       if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),units='in',res=res,height=height,width=width)
  #       if (filetype=='png') png(paste(filename,".png",sep=""),units='in',res=res,height=height,width=width)
  #       if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),units='in',res=res,height=height,width=width)
  #       if (filetype=="svg")
  #       {
  #         if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
  #         require("RSVGTipsDevice")
  #         devSVGTips(paste(filename,".svg",sep=""),width=width,height=height,title=filename)
  #       }
  #       if (filetype=="tex")
  #       {
  #         # 	# Special thanks to Charlie Sharpsteen for supplying these tikz codes on stackoverflow.com !!!
  #         # 	
  #         # 	if (!suppressPackageStartupMessages(require(tikzDevice,quietly=TRUE))) stop("tikzDevice must be installed to use filetype='tex'")
  #         # 	opt= c( 
  #         # 	getOption('tikzLatexPackages'),  
  #         #     "\\def\\tooltiptarget{\\phantom{\\rule{1mm}{1mm}}}",
  #         #     "\\newbox\\tempboxa\\setbox\\tempboxa=\\hbox{}\\immediate\\pdfxform\\tempboxa \\edef\\emptyicon{\\the\\pdflastxform}",
  #         #     "\\newcommand\\tooltip[1]{\\pdfstartlink user{/Subtype /Text/Contents  (#1)/AP <</N \\emptyicon\\space 0 R >>}\\tooltiptarget\\pdfendlink}"
  #         # 	)
  #         # 	
  #         # 	place_PDF_tooltip <- function(x, y, text)
  #         # 	{
  #         # 
  #         # 		# Calculate coordinates
  #         # 		tikzX <- round(grconvertX(x, to = "device"), 2)
  #         # 		tikzY <- round(grconvertY(y, to = "device"), 2)
  #         # 		# Insert node
  #         # 		tikzAnnotate(paste(
  #         # 		"\\node at (", tikzX, ",", tikzY, ") ",
  #         # 		"{\\tooltip{", text, "}};",
  #         # 		sep = ''
  #         # 		))
  #         # 	  invisible()
  #         # 	}
  #         # 	
  #         # 	print("NOTE: Using 'tex' as filetype will take longer to run than other filetypes")
  #         # 	
  #         # 	tikzDevice:::tikz(paste(filename,".tex",sep=""), standAlone = standAlone, width=width, height=height, packages=opt)
  #         
  #         stop("Tikz device no longer supported due to removal from CRAN. Please see www.sachaepskamp.com/qgraph for a fix")
  #       }
  #     }	
  #if (!filetype%in%c('pdf','png','jpg','jpeg','svg','R','eps','tiff')) warning(paste("File type",filetype,"is not supported")) 
  
  # Specify background:
  if (is.null(background)){
    background <- par("bg")
    if (background == "transparent") background <- "white"    
  }
  if (isColor(bg)) background <- bg
  # Remove alpha:
  background <- col2rgb(background, alpha = TRUE)
  background <- rgb(background[1],background[2],background[3],background[4],maxColorValue=255)
  
  if (is.null(subplotbg)) subplotbg <- background
  
  if (isTRUE(edge.label.bg)) edge.label.bg <- background
  if(is.null(qgraphObject$Arguments[['label.color']])) {
    if(is.null(qgraphObject$Arguments$lcolor)) lcolor <- ifelse(mean(col2rgb(background)/255) > 0.5,"black","white") else lcolor <- qgraphObject$Arguments$lcolor
  } else lcolor <- qgraphObject$Arguments[['label.color']]
  
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
      if (all(unique(c(input)) %in% c(0,1)) & !grepl("sig",mode)) weighted <- FALSE else weighted <- TRUE
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
      if (!isSymmetric(unname(input))) directed=TRUE else directed=FALSE
    }
  }
  
  
  # Set default edge width:
  if(is.null(qgraphObject$Arguments[["esize"]])) 
  {
    if (weighted)
    {
      #       esize <- max((-1/72)*(nNodes)+5.35,2) 
      esize <- 15*exp(-nNodes/90)+1
    } else {
      esize <- 2
    }
    if (any(directed)) esize <- max(esize/2,1)
  } else esize <- qgraphObject$Arguments$esize
  
  # asize default:
  if(is.null(qgraphObject$Arguments[["asize"]]))
  {
    #       asize <- max((-1/10)*(nNodes)+4,1)
    #     asize <- ifelse(nNodes>10,2,3)
    asize <- 2*exp(-nNodes/20)+2
  } else asize <- qgraphObject$Arguments[["asize"]]
  
  if(!is.null(qgraphObject$Arguments[["edge.width"]]))
  {
    esize <- esize * qgraphObject$Arguments[["edge.width"]]
    asize <- asize * sqrt(qgraphObject$Arguments[["edge.width"]])
  }
  
  ## arrowAngle default:
  if(is.null(qgraphObject$Arguments[["arrowAngle"]])) 
  {
    if (weighted) arrowAngle <- pi/6 else arrowAngle <- pi/8
  } else {
    arrowAngle <- qgraphObject$Arguments[["arrowAngle"]]
  }
  

  ########### GRAPHICAL MODEL SELECTION #######
  
  if (graph == "assosciation") {
    if(!all(eigen(input)$values > 0))  {
      warning("Correlation/covariance matrix is not positive definite. Finding nearest positive definite matrix")
      
      input <- as.matrix(Matrix::nearPD(input, keepDiag = TRUE, ensureSymmetry = TRUE)$mat)
    }
  }
  
  
  # Partial graph:
  if (graph != "default")
  {
    if (edgelist) stop("Graph requires correlation or covariance matrix")
    
    # Check for symmetric matrix:
    if (!isSymmetric(input))
    {
      stop("Input matrix is not symmetric, thus can not be a correlation or covariance matrix.")
    }
    
    # Check for positive definiteness (glasso does its own check):
    if (graph != "glasso")
    {
      if(!all(eigen(input)$values > 0))  {
        warning("Correlation/covariance matrix is not positive definite. Finding nearest positive definite matrix")
        
        input <- as.matrix(Matrix::nearPD(input, keepDiag = TRUE, ensureSymmetry = TRUE)$mat)
      }
    }
    
    # Association graph:
    if (graph == "assosciation")
    {
      if (!all(diag(input) == 1)){
        input <- cov2cor(input)
      }
    }
    
    # Concentration graph:
    if (graph=="concentration") 
    {
      coln <- colnames(input)
      rown <- rownames(input)
      input <- cor2pcor(input)
      rownames(input) <- rown
      colnames(input) <- coln
    } 
    
#     # FDR:
#     if (tolower(graph)=="fdr.cor") 
#     {
#       if (!all(diag(input) == 1)){
#         input <- cov2cor(input)
#       }
#       input <- FDRnetwork(input, FDRcutoff)
#     } 
#     
#     if (tolower(graph)=="fdr.pcor") 
#     {
#       input <- cor2pcor(input)
#       input <- FDRnetwork(input, FDRcutoff)
#     } 
#     
#     if (tolower(graph) == "fdr")
#     {
#       input <- cor2pcor(input)
#       testResult <- GeneNet::ggm.test.edges(input, fdr = TRUE, plot = FALSE)
#       net <- GeneNet::extract.network(testResult)
#       input <- matrix(0, nrow(input), ncol(input))
#       for (i in seq_len(nrow(net)))
#       {
#         input[net$node1[i],net$node2[i]] <- input[net$node2[i],net$node1[i]] <- net$pcor[i]
#       }
#     }
    
    # Glasso graph:
    if (graph == "glasso")
    {
      if (edgelist) stop("Concentration graph requires correlation matrix")
      if (is.null(sampleSize)) stop("'sampleSize' argument is needed for glasso estimation")
      input <- EBICglasso(input, sampleSize, gamma = tuning)
    }
    
    diag(input) <- 1
    input <- as.matrix(forceSymmetric(input))
  }

  
  ## Thresholding ####
  if (is.character(threshold))
  {    
    if (graph == "default")
    {
      if (verbose) message("'threshold' is assigned a string but 'graph' is not assigned. Detecting if input could be a correlation matrix.")
      # Detect if graph could be correlations or covariance matrix:
      
      # Check if input was a matrix:
      if (!is.matrix(input) | edgelist) stop(paste0("'",threshold,"' threshold requires a (partial) correlation/covariance matrix as input"))
      
      # Check if input is square matrix:
      if (!isSymmetric(input)) stop(paste0("'",threshold,"' threshold requires a (partial) correlation/covariance matrix as input: input was not a square matrix."))
      
      # Check if input is positive semi definite:
      if (any(eigen(input)$values < 0)) stop(paste0("'",threshold,"' threshold requires a (partial) correlation/covariance matrix as input: input was not a positive semi-definite matrix"))
      
      # If these checks are passed assume matrix is correlation or covariance: 
    } else {
      if (!graph %in% c("assosciation","concentration"))
      {
        stop("Thresholding by significance level only supported for graph = 'assosciation' (graph = 'cor') or graph = 'concentration' (graph = 'pcor')")
      }
    }
    
    # Stop for incorrect threshold:
    if (!threshold %in% c('sig','holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none', 'locfdr'))
    {
      stop("'threshold' argument must be number or 'sig','holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none' or 'locfdr'")
    }
    
    # Significance:
    if (threshold != "locfdr")
    {
      if (grepl("sig",threshold,ignore.case=TRUE))
      {
        threshold <- "none"
      }

      if (is.null(sampleSize))
      {
        stop("'sampleSize' argument is needed for all thresholding with significance except 'locfdr'")
      }
      nadj <- sampleSize
      if (graph == "concentration")
      {
        nadj <- nadj - (nNodes - 2)
      }
      
      if (all(diag(input)==1)) 
      {
        pvals <- psych::corr.p(input,n = nadj, adjust = threshold, alpha = max(alpha))$p
      } else {
        pvals <- psych::corr.p(cov2cor(input), n = nadj, adjust = threshold, alpha = max(alpha))$p
      }
      
      # Symmetrize:
      pvals[lower.tri(pvals)] <- t(pvals)[lower.tri(pvals)]
      
      # Remove insignificant edges:
      input <- input * (pvals < max(alpha))
    } else {
      input <- FDRnetwork(input, FDRcutoff)
    }
    
    threshold <- 0
  }
  
  
#######################3

  ## diag default:
  if(is.null(qgraphObject$Arguments[['diag']])) 
  {
    if (edgelist) diag <- FALSE  else diag <- length(unique(diag(input))) > 1
  } else { 
    diag <- qgraphObject$Arguments$diag
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
        E$weight <- sign0(E$weight) * fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
      } else E$weight <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
    }
    if (bonf)
    {
      if (mode=="sig") 
      {
        E$weight <- E$weight * length(E$weight)
        E$weight[E$weight > 1] <- 1
        E$weight[E$weight < -1] <- -1
      } # else warning("Bonferonni correction is only applied if mode='sig'")
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
      E$weight <- sign0(E$weight) * sigScale(abs(E$weight))
    }
    if (OmitInsig)
    {
#       if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?")
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
          if (isSymmetric(unname(input)))
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
        E$weight <- sign0(E$weight) * fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
      } else E$weight <- fdrtool(E$weight,"correlation",plot=FALSE, color.figure=FALSE, verbose=FALSE)$pval
    }
    if (bonf)
    {
      if (mode=="sig") 
      {
        E$weight <- E$weight * length(E$weight)
        E$weight[E$weight > 1] <- 1
        E$weight[E$weight < -1] <- -1
      } # else warning("Bonferonni correction is only applied if mode='sig'")
    }
    if (mode=="sig" & any(E$weight < -1 | E$weight > 1))
    {
      warning("Weights under -1 inputusted to -1 and weights over 1 input adjusted to 1")
      E$weight[E$weight < -1] <- -1
      E$weight[E$weight > 1] <- 1
    }
    
    if (mode=="sig") 
    {
      Pvals <- E$weight
      E$weight <- sign0(E$weight) * sigScale(abs(E$weight))
    }
    
    if (OmitInsig)
    {
#       if (!require("fdrtool")) stop("`fdrtool' package not found, is it installed?")
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
#       knots <- knots[E$weight!=0]
    }
    if (is.matrix(curve))
    {
      curve <- curve[c(incl)]
#       curve <- curve[E$weight!=0]
    }
    if (is.matrix(parallelEdge))
    {
      parallelEdge <- parallelEdge[c(incl)]
#       parallelEdge <- parallelEdge[E$weight!=0]
    }
    if (is.matrix(parallelAngle))
    {
      parallelAngle <- parallelAngle[c(incl)]
#       parallelAngle <- parallelAngle[E$weight!=0]
    }
    if (is.matrix(bidirectional))
    {
      bidirectional <- bidirectional[c(incl)]
#       bidirectional <- bidirectional[E$weight!=0]
    }
    if (is.matrix(residEdge))
    {
      residEdge <- residEdge[c(incl)]
#       residEdge <- residEdge[E$weight!=0]
    }
    if (is.matrix(CircleEdgeEnd))
    {
      CircleEdgeEnd <- CircleEdgeEnd[c(incl)]
#       CircleEdgeEnd <- CircleEdgeEnd[E$weight!=0]
    }      
    if (is.matrix(edge.labels))
    {
      edge.labels <- edge.labels[c(incl)]
#       edge.labels <- edge.labels[E$weight!=0]
    }
    if (is.matrix(edge.color))
    {
      edge.color <- edge.color[c(incl)]
#       edge.color <- edge.color[E$weight!=0]
    }
    if (is.matrix(edge.label.bg))
    {
      edge.label.bg <- edge.label.bg[c(incl)]
#       edge.label.bg <- edge.label.bg[E$weight!=0]
    }
    if (is.matrix(edge.label.font))
    {
      edge.label.font <- edge.label.font[c(incl)]
#       edge.label.font <- edge.label.font[E$weight!=0]
    }
    if (!is.null(ELcolor))
    {
      if (is.matrix(ELcolor))
      {
        ELcolor <- ELcolor[c(incl)]
#         ELcolor <- ELcolor[E$weight!=0]
      }      
    }
    
    if (!is.null(edge.color)) if (length(edge.color) == length(E$weight)) edge.color <- edge.color[E$weight!=0]
    
    if (is.matrix(lty))
    {
      lty <- lty[c(incl)]
#       lty <- lty[E$weight!=0]
    }
    
    if (!is.null(edgeConnectPoints))
    {
      if (is.array(edgeConnectPoints) && isTRUE(dim(edgeConnectPoints)[3]==2))
      {
        edgeConnectPoints <- matrix(edgeConnectPoints[c(incl,incl)],,2)
#         edgeConnectPoints <- edgeConnectPoints[E$weight!=0,,drop=FALSE]
      }
    }
    
    if (is.matrix(edge.label.position))
    {
      edge.label.position <- edge.label.position[c(incl)]
#       edge.label.position <- edge.label.position[E$weight!=0]
    }
  }	

  keep <- abs(E$weight)>threshold

 ######
  
  if (length(loopRotation)==1) loopRotation <- rep(loopRotation,nNodes)
  
  if (length(directed)==1) 
  {
    directed <- rep(directed,length(E$from))
  }
  directed <- directed[keep]
  
  if (!is.null(edge.color) && length(edge.color) != sum(keep)) 
  {
    edge.color <- rep(edge.color,length=length(E$from))
    if (length(edge.color) != length(keep)) stop("'edge.color' is wrong length")
    edge.color <- edge.color[keep]
  }
  
  if (!is.logical(edge.labels))
  {
    if (length(edge.labels) == 1) edge.labels <- rep(edge.labels,length(E$from))
    if (length(edge.labels) != length(keep) & length(edge.labels) != sum(keep)) stop("'edge.label.bg' is wrong length")
    if (length(edge.labels)==length(keep)) edge.labels <- edge.labels[keep]
    
    # edge.labels <- rep(edge.labels,length=length(E$from))
  }
  
  #     if (is.logical(edge.label.bg))
  #     {
  #       edge.label.bg <- "white"
  #     }
  if (length(edge.label.bg) == 1) edge.label.bg <- rep(edge.label.bg,length(E$from))
  if (length(edge.label.bg) != length(keep) & length(edge.label.bg) != sum(keep)) stop("'edge.label.bg' is wrong length")
  if (length(edge.label.bg)==length(keep)) edge.label.bg <- edge.label.bg[keep]
  
  
  if (length(edge.label.font) == 1) edge.label.font <- rep(edge.label.font,length(E$from))
  if (length(edge.label.font) != length(keep) & length(edge.label.font) != sum(keep)) stop("'edge.label.font' is wrong length")
  if (length(edge.label.font)==length(keep)) edge.label.font <- edge.label.font[keep]
  
  
  if (length(lty) == 1) lty <- rep(lty,length(E$from))
  if (length(lty) != length(keep) & length(lty) != sum(keep)) stop("'lty' is wrong length")
  if (length(lty)==length(keep)) lty <- lty[keep]
  
  if (!is.null(edgeConnectPoints))
  {
    if (length(edgeConnectPoints) == 1) edgeConnectPoints <- matrix(rep(edgeConnectPoints,2*length(E$from)),,2)
    if (nrow(edgeConnectPoints) != length(keep) & nrow(edgeConnectPoints) != sum(keep)) stop("Number of rows in 'edgeConnectPoints' do not match number of edges")
    if (nrow(edgeConnectPoints)==length(keep)) edgeConnectPoints <- edgeConnectPoints[keep,,drop=FALSE]
  }
  
  if (length(edge.label.position) == 1) edge.label.position <- rep(edge.label.position,length(E$from))
  if (length(edge.label.position) != length(keep) & length(edge.label.position) != sum(keep)) stop("'edge.label.position' is wrong length")
  if (length(edge.label.position)==length(keep)) edge.label.position <- edge.label.position[keep]  
  
  
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
  
  if (length(CircleEdgeEnd)==1) 
  {
    CircleEdgeEnd <- rep(CircleEdgeEnd,length(E$from))
  }
  if (length(CircleEdgeEnd)==length(keep)) CircleEdgeEnd <- CircleEdgeEnd[keep]    
  
  if (!is.logical(edge.labels))
  {
    if (length(edge.labels)==length(keep))
    {
      edge.labels <- edge.labels[keep]
    }
  }
  
  if (length(curve)==1) 
  {
    curve <- rep(curve,length(E$from))
  }
  if (length(curve)==length(keep)) curve <- curve[keep]   
  
  
  if (length(parallelEdge)==1) 
  {
    parallelEdge <- rep(parallelEdge,length(E$from))
  }
  if (length(parallelEdge)==length(keep)) parallelEdge <- parallelEdge[keep]    
  
  
  if (length(parallelAngle)==1) 
  {
    parallelAngle <- rep(parallelAngle,length(E$from))
  }
  if (length(parallelAngle)==length(keep)) parallelAngle <- parallelAngle[keep]    
  
  E$from=E$from[keep]
  E$to=E$to[keep]
  if (mode=="sig") Pvals <- Pvals[keep]
  E$weight=E$weight[keep]
  
  
  ## Define cut:
  if (defineCut)
  {
    
    if (length(E$weight) > 3*nNodes)
    {
      #       cut <- median(sort(E$weight,decreasing=TRUE)[seq_len(nNodes)])
      cut <- max(sort(abs(E$weight),decreasing=TRUE)[2*nNodes], quantile(abs(E$weight),0.75))
    } else if (length(E$weight) > 1) cut <- quantile(abs(E$weight),0.75) else cut <- 0
    #     cut <- quantile(abs(E$weight), cutQuantile)
  }
  
  
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
  
  if (length(lty)==1) lty=rep(lty,length(E$from))
  
  if (length(edge.label.position)==1) edge.label.position=rep(edge.label.position,length(E$from))
  
  
  # Make bidirectional vector:
  if (length(bidirectional)==1) bidirectional=rep(bidirectional,length(E$from))
  if (length(bidirectional)!=length(E$from)) stop("Bidirectional vector must be of legth 1 or equal to the number of edges")
  
  srt <- cbind(pmin(E$from,E$to), pmax(E$from,E$to) , knots, abs(E$weight) > minimum)
  
  if (!curveAll | any(parallelEdge))
  {
    dub <- duplicated(srt)|duplicated(srt,fromLast=TRUE)
    
    if (!curveAll)
    {
      if (length(curve)==1) curve <- rep(curve,length(E$from))
      curve <- ifelse(is.na(curve),ifelse(knots==0&dub&!bidirectional&is.na(curve),ifelse(E$from==srt[,1],1,-1) * ave(1:nrow(srt),srt[,1],srt[,2],bidirectional,FUN=function(x)seq(curveDefault,-curveDefault,length=length(x))),0),curve)
    }
    
    if (any(parallelEdge))
    {
      # Set parallelAngle value:   
      parallelAngle <- ifelse(is.na(parallelAngle),ifelse(knots==0&dub&!bidirectional&is.na(parallelAngle),ifelse(E$from==srt[,1],1,-1) * ave(1:nrow(srt),srt[,1],srt[,2],bidirectional,FUN=function(x)seq(parallelAngleDefault,-parallelAngleDefault,length=length(x))),0),parallelAngle) 
    }
    
    rm(dub)
  }
  
  parallelAngle[is.na(parallelAngle)] <- 0
  
  
  # Layout settings:
  if (nNodes == 1)
  {
    layout <- matrix(0,1,2)
  } else {
    if (is.null(layout)) layout="default"
    
    if (!is.matrix(layout))
    {
      # If function, assume igraph function (todo: check this)
      if (is.function(layout))
      {
        Graph <- graph.edgelist(as.matrix(cbind(E$from,E$to)), any(directed))
        E(Graph)$weight <- E$weight
        
        # set roots:
        if (deparse(match.call()[['layout']]) == "layout.reingold.tilford" && is.null(layout.par[['root']]))
        {
          sp <- shortest.paths(Graph, mode = "out")
          diag(sp) <- Inf
          
          # Find root nodes:
          roots <- which(colSums(sp==Inf) == nrow(sp))
          # Find roots with longest outgoing paths:
          maxs <- sapply(roots,function(x)max(sp[x,sp[x,]!=Inf]))
          
          layout.par[['root']] <- roots[maxs==max(maxs)]
        }
        
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
              layout[groups[[i]],1]=repulsion*sin(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,1]
              layout[groups[[i]],2]=repulsion*cos(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,2] 
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
      layout[is.na(layout)] <- 0
      # If character and labels exist, replace:
      if (is.character(layout) && is.character(labels))
      {
        layout[] <- match(layout,labels)
        layout[is.na(layout)] <- 0
        mode(layout) <- 'numeric'
      }
      
      # Check:
      if (!all(seq_len(nNodes) %in% layout)) stop("Grid matrix does not contain a placement for every node.")
      if (any(sapply(seq_len(nNodes),function(x)sum(layout==x))>1)) stop("Grid matrix contains a double entry.")
      
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
          if (length(alpha) > 3) edge.color[Pvals >= 0 & Pvals < alpha[4]  & E$weight > minimum] <- "cadetblue1"	
          # Set colors for edges over sig > 0.01 :
          if (length(alpha) > 2) edge.color[Pvals >= 0 & Pvals < alpha[3]  & E$weight > minimum] <- "#6495ED"
          # Set colors for edges over sig > 0.01 :
          if (length(alpha) > 1) edge.color[Pvals >= 0 & Pvals < alpha[2]  & E$weight > minimum] <- "blue"				
          # Set colors for edges over sig < 0.01 :
          edge.color[Pvals >= 0 & Pvals < alpha[1]  & E$weight > minimum] <- "darkblue"
          
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
    if (!gray) 
    {
      if (pastel)
      {
        color <- rainbow_hcl(length(groups), start = rainbowStart * 360, end = (360 * rainbowStart + 360*(length(groups)-1)/length(groups)))
      } else {
        color <- rainbow(length(groups), start = rainbowStart, end = (rainbowStart + (max(1.1,length(groups)-1))/length(groups)) %% 1)   
      }
    }
    if (gray) color <- sapply(seq(0.2,0.8,length=length(groups)),function(x)rgb(x,x,x))
  }
  if (is.null(color))	color <- "background"  
  vertex.colors <- rep(color, length=nNodes)
  if (!is.null(groups)) 
  {
    vertex.colors <- rep("background", length=nNodes)
    for (i in 1:length(groups)) vertex.colors[groups[[i]]]=color[i] 
  } else vertex.colors <- rep(color, length=nNodes)
  if (length(color)==nNodes) vertex.colors <- color
  if (all(col2rgb(background,TRUE) == col2rgb("transparent",TRUE)))
  {
    vertex.colors[vertex.colors=="background"] <- "white"
  } else  vertex.colors[vertex.colors=="background"] <- background
  
  # Dummy groups list:
  if (is.null(groups)) 
  {
    groups <- list(1:nNodes)
  }
  
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
  
  # means:
  if (length(means)==1) means <- rep(means,nNodes)
  if (length(SDs)==1) SDs <- rep(SDs, nNodes)
  
  #     
  #     pch1=numeric(0)
  #     pch2=numeric(0)
  #     
  #     for (i in 1:length(shape))
  #     {
  #       if (shape[i]=="circle")
  #       {
  #         pch1[i]=16
  #         pch2[i]=1
  #       }
  #       if (shape[i]=="square")
  #       {
  #         pch1[i]=15
  #         pch2[i]=0
  #       }
  #       if (shape[i]=="triangle")
  #       {
  #         pch1[i]=17
  #         pch2[i]=2
  #       }
  #       if (shape[i]=="diamond")
  #       {
  #         pch1[i]=18
  #         pch2[i]=5
  #       }
  #       if (!shape[i]%in%c("circle","square","triangle","diamond","rectangle")) stop(paste("Shape",shape[i],"is not supported"))
  #     }
  #     
  
  
  # Arrow sizes:
  if (length(asize)==1) asize=rep(asize,length(E$from))
  
  if (length(asize)!=length(E$from)) warning("Length of 'asize' is not equal to the number of edges")
  
  
  
  # Edge labels:
  # Make labels:
  
  if (!is.logical(edge.labels))
  {
    #       edge.labels=as.character(edge.labels)
    if (length(edge.labels)!=length(E$from))
    {
      warning("Number of edge labels did not correspond to number of edges, edge labes have been ommited")
      edge.labels <- FALSE
    }
    
    if (length(edge.labels) > 0 & is.character(edge.labels))
    {
      edge.labels[edge.labels=="NA"]=""
    }  
  } else
  {
    if (edge.labels)
    {
      edge.labels= as.character(round(E$weight,2))
    } else edge.labels <- rep('',length(E$from))
  }
  
  if (is.numeric(edge.labels)) edge.labels <- as.character(edge.labels)
  
  # Bars:
  length(bars) <- nNodes
  barSide <- rep(barSide,nNodes)
  barColor <- rep(barColor, nNodes)
  barLength <- rep(barLength, nNodes)
  barColor[barColor == 'border'] <- bcolor[barColor == 'border']
  
  
  # Compute loopRotation:
  for (i in seq_len(nNodes))
  {
    if (is.na(loopRotation[i]))
    {
      centX <- mean(layout[,1])
      centY <- mean(layout[,2])
      for (g in 1:length(groups))
      {
        if (i%in%groups[[g]] & length(groups[[g]]) > 1)
        {
          centX <- mean(layout[groups[[g]],1])
          centY <- mean(layout[groups[[g]],2])
        }
      }
      loopRotation[i] <- atan2usr2in(layout[i,1]-centX,layout[i,2]-centY)
      if (shape[i]=="square")
      {
        loopRotation[i] <- c(0,0.5*pi,pi,1.5*pi)[which.min(abs(c(0,0.5*pi,pi,1.5*pi)-loopRotation[i]%%(2*pi)))]
      }
    } 
  } 
  
  
  # Node names:
  if (is.null(nodeNames)) nodeNames <- labels
  
  
  # Make labels:
  if (is.logical(labels))
  {
    if (labels)
    {
      labels=1:nNodes
    } else 
    {
      labels <- rep('',nNodes)
    }
  }
  
  border.width <- rep(border.width, nNodes)
  
  # Node argument setup:
  borders <- rep(borders,length=nNodes)
  label.font <- rep(label.font,length=nNodes)
  
  ########### SPLIT HERE ###########
  
  ### Fill qgraph object with stuff:
  ## Edgelist:
  qgraphObject$Edgelist$from <- E$from
  qgraphObject$Edgelist$to <- E$to
  qgraphObject$Edgelist$weight <- E$weight
  qgraphObject$Edgelist$directed <- directed
  qgraphObject$Edgelist$bidirectional <- bidirectional
  
  # Nodes:
  qgraphObject$graphAttributes$Nodes$border.color <- bcolor
  qgraphObject$graphAttributes$Nodes$borders <- borders
  qgraphObject$graphAttributes$Nodes$border.width <- border.width
  qgraphObject$graphAttributes$Nodes$label.cex <- label.cex
  qgraphObject$graphAttributes$Nodes$label.font <- label.font
  qgraphObject$graphAttributes$Nodes$label.color <- lcolor
  qgraphObject$graphAttributes$Nodes$labels <- labels
  qgraphObject$graphAttributes$Nodes$names <- nodeNames
  qgraphObject$graphAttributes$Nodes$loopRotation <- loopRotation
  qgraphObject$graphAttributes$Nodes$shape <- shape
  qgraphObject$graphAttributes$Nodes$color <- vertex.colors
  qgraphObject$graphAttributes$Nodes$width <- vsize
  qgraphObject$graphAttributes$Nodes$height <- vsize2
  qgraphObject$graphAttributes$Nodes$subplots <- subplots
  qgraphObject$graphAttributes$Nodes$images <- images
  qgraphObject$graphAttributes$Nodes$tooltips <- tooltips
  qgraphObject$graphAttributes$Nodes$SVGtooltips <- SVGtooltips
  qgraphObject$graphAttributes$Nodes$bars <- bars
  qgraphObject$graphAttributes$Nodes$barSide <- barSide
  qgraphObject$graphAttributes$Nodes$barColor <- barColor
  qgraphObject$graphAttributes$Nodes$barLength <- barLength
  qgraphObject$graphAttributes$Nodes$means <- means
  qgraphObject$graphAttributes$Nodes$SDs <- SDs
  
  # Edges:
  qgraphObject$graphAttributes$Edges$curve <- curve
  qgraphObject$graphAttributes$Edges$color <- edge.color
  qgraphObject$graphAttributes$Edges$labels <- edge.labels
  qgraphObject$graphAttributes$Edges$label.cex <- edge.label.cex
  qgraphObject$graphAttributes$Edges$label.bg <- edge.label.bg
  qgraphObject$graphAttributes$Edges$label.font <- edge.label.font
  qgraphObject$graphAttributes$Edges$label.color <- ELcolor
  qgraphObject$graphAttributes$Edges$width <- edge.width
  qgraphObject$graphAttributes$Edges$lty <- lty
  qgraphObject$graphAttributes$Edges$edge.label.position <- edge.label.position
  qgraphObject$graphAttributes$Edges$residEdge <- residEdge
  qgraphObject$graphAttributes$Edges$CircleEdgeEnd <- CircleEdgeEnd
  qgraphObject$graphAttributes$Edges$asize <- asize
  if (mode == "sig") qgraphObject$graphAttributes$Edges$Pvals <- Pvals else Pvals <- NULL
  qgraphObject$graphAttributes$Edges$parallelEdge <- parallelEdge
  qgraphObject$graphAttributes$Edges$parallelAngle <- parallelAngle
  qgraphObject$graphAttributes$Edges$edgeConnectPoints <- edgeConnectPoints
  
  # Knots:
  qgraphObject$graphAttributes$Knots$knots <- knots
  qgraphObject$graphAttributes$Knots$knot.size <- knot.size
  qgraphObject$graphAttributes$Knots$knot.color <- knot.color
  qgraphObject$graphAttributes$Knots$knot.borders <- knot.borders
  qgraphObject$graphAttributes$Knots$knot.border.color <- knot.border.color
  qgraphObject$graphAttributes$Knots$knot.border.width <- knot.border.width
  
  # Graph:
  qgraphObject$graphAttributes$Graph$nNodes <- nNodes
  qgraphObject$graphAttributes$Graph$weighted <- weighted
  qgraphObject$graphAttributes$Graph$edgesort <- edgesort
  qgraphObject$graphAttributes$Graph$scores <- scores
  qgraphObject$graphAttributes$Graph$scores.range <- scores.range
  qgraphObject$graphAttributes$Graph$groups <- groups
  qgraphObject$graphAttributes$Graph$minimum <- minimum
  qgraphObject$graphAttributes$Graph$maximum <- maximum
  qgraphObject$graphAttributes$Graph$cut <- cut
  qgraphObject$graphAttributes$Graph$polygonList <- polygonList
  qgraphObject$graphAttributes$Graph$mode <- mode
  qgraphObject$graphAttributes$Graph$color <- color
  
  # Layout:
  qgraphObject$layout <- layout
  qgraphObject$layout.orig <- original.layout
  
  # Plot options:
  qgraphObject$plotOptions$filetype <- filetype
  qgraphObject$plotOptions$filename <- filename
  qgraphObject$plotOptions$background <- background
  qgraphObject$plotOptions$bg <- bg
  qgraphObject$plotOptions$normalize <- normalize
  qgraphObject$plotOptions$plot <- plot
  qgraphObject$plotOptions$mar <- mar
  qgraphObject$plotOptions$GLratio <- GLratio
  qgraphObject$plotOptions$legend <- legend
  qgraphObject$plotOptions$legend.cex <- legend.cex
  qgraphObject$plotOptions$pty <- pty
  qgraphObject$plotOptions$XKCD <- XKCD
  qgraphObject$plotOptions$residuals <- residuals
  qgraphObject$plotOptions$residScale <- residScale
  qgraphObject$plotOptions$arrows <- arrows
  qgraphObject$plotOptions$arrowAngle <- arrowAngle
  qgraphObject$plotOptions$open <- open
  qgraphObject$plotOptions$curvePivot <- curvePivot
  qgraphObject$plotOptions$curveShape <- curveShape
  qgraphObject$plotOptions$curveScale <- curveScale
  qgraphObject$plotOptions$curvePivotShape <- curvePivotShape
  qgraphObject$plotOptions$label.scale <- label.scale
  qgraphObject$plotOptions$label.prop <- label.prop
  qgraphObject$plotOptions$label.norm <- label.norm
  qgraphObject$plotOptions$overlay <- overlay
  qgraphObject$plotOptions$details <- details
  qgraphObject$plotOptions$title <- title
  qgraphObject$plotOptions$preExpression <- preExpression
  qgraphObject$plotOptions$postExpression <- postExpression
  qgraphObject$plotOptions$legend.mode <- legend.mode
  qgraphObject$plotOptions$srt <- srt
  qgraphObject$plotOptions$gray <- gray
  qgraphObject$plotOptions$overlaySize <- overlaySize
  qgraphObject$plotOptions$plotELBG <- plotELBG
  qgraphObject$plotOptions$alpha <- alpha
  qgraphObject$plotOptions$width <- width
  qgraphObject$plotOptions$height <- height
  qgraphObject$plotOptions$aspect <- aspect
  qgraphObject$plotOptions$rescale <- rescale
  qgraphObject$plotOptions$barsAtSide <- barsAtSide
  qgraphObject$plotOptions$bgres <- bgres
  qgraphObject$plotOptions$bgcontrol <- bgcontrol
  qgraphObject$plotOptions$resolution <- res
  qgraphObject$plotOptions$subpars <- subpars
  qgraphObject$plotOptions$subplotbg <- subplotbg
  qgraphObject$plotOptions$usePCH <- usePCH
  qgraphObject$plotOptions$node.resolution <- node.resolution
  qgraphObject$plotOptions$noPar <- noPar
  qgraphObject$plotOptions$meanRange <- meanRange

  
  if (!DoNotPlot)
  {
    plot(qgraphObject)
    invisible(qgraphObject)
  } else
  {
    return(qgraphObject)
  }
  
}

