
# init = initial layout in first frame
qgraph.animate <- function(input,ind=NULL,...,constraint=10,growth="order",titles=NULL,sleep=0,smooth = TRUE, plotGraphs = TRUE, progress = TRUE, initLayout)
{
  
  
  # arguments <- list(...)
  # 
  # if (length(arguments)>0)
  # {
  # 	for (i in 1:length(arguments))
  # 	{
  # 		if (class(arguments[[i]])=="qgraph") 
  # 		{
  # 			if (!is.null(names(arguments[[i]])))
  # 			{
  # 				for (j in 1:length(arguments[[i]]))
  # 				{
  # 					if (!(names(arguments[[i]])[j]%in%names(arguments)))
  # 					{
  # 						arguments[length(arguments)+1]=arguments[[i]][j]
  # 						names(arguments)[length(arguments)]=names(arguments[[i]])[j]
  # 					}
  # 				}
  # 			}
  # 		}
  # 	}
  # }

  arguments <- list(...)
  
  # Import arguments:
  if (length(arguments) > 0) arguments <- getArgs(arguments)
  
  # Import default arguments:
  def <- options("qgraph")
  if (!is.null(def$qgraph)) class(def$qgraph) <- "qgraph"
  if (any(sapply(def,function(x)!is.null(x))))
  {
    arguments <- getArgs(c(arguments,def))
  }

  # Extract labels:
  if(is.null(arguments[['labels']])) labels <- NULL else labels <- arguments[['labels']]
  
  # Check if list:
  inputIsList <- is.list(input)
  
  # Check for correct input:
  if (!inputIsList && nrow(input)!=ncol(input)) stop("input must be an inputacency matrix or list of inputacency matrices")
  if (!(growth %in% c("order","degree"))) stop("Incorrect growth")
  
  # Check for dimensions of list:
  if (inputIsList)
  {
    inputList <- input
    if (any(sapply(input,nrow)-sapply(input,ncol) != 0)) stop("If input is a list, it must contain only square matrices of the same dimesions")
  }
  
  
  # Number of nodes:
  if (inputIsList) n <- nrow(input[[1]]) else n <- nrow(input)
  
  # Make labels:
  if (length(labels) == 0) if (is.null(labels)) labels <- 1:n
  
  # Convert ind as matrix:
  if (is.data.frame(ind)) ind <- as.matrix(ind)
  
  # Default growth:
  if (is.null(ind) & inputIsList)
  {
    ind <- matrix(TRUE,length(inputList),n)
  }
  if (is.null(ind) & !inputIsList)
  {
    ind <- matrix(FALSE,n,n)
    
    if (growth == "order")
    {
      ind <- lower.tri(ind,diag=TRUE)
    } else if (growth == "degree")
    {
      degs <- order((rowSums(input)+colSums(input))/2,decreasing=TRUE)
      for (i in seq(n)) ind[i, degs[seq(i)]] <- TRUE
    }
  }
  
  # If ind is logical vector of length n, start with subset and increase normal:
  if (!inputIsList & is.logical(ind) && length(ind)==n)
  {
    sub <- ind
    ind <- matrix(FALSE,n - sum(sub) + 1,n)
    ind[1,] <- sub
    
    if (sum(sub)<n)
    {
      if (growth == "order")
      {
        for (i in 2:(sum(!ind[1,])+1))
        {
          ind[i,] <- ind[i-1,] | (1:10)==which.min(ind[i-1,])
        }
      } else if (growth == "degree")
      {
        degs <- order((rowSums(input)+colSums(input))/2,decreasing=TRUE)
        for (i in 2:(sum(!ind[1,])+1))
        {
          ind[i,] <- ind[i-1,] | (1:10)== degs[!degs%in%which(ind[i-1,])][1]
        }
      }
    }
    # 	sub <- ind
    # 	meanDeg <- (rowSums(input)+colSums(input))/2
    # 	meanDeg[sub] <- -Inf
    # 	degs <- order(meanDeg,decreasing=TRUE)
    # 
    # 	ind <- matrix(FALSE,n-sum(sub)+1,n)
    # 	
    # 	ind[,sub] <- TRUE
    # 
    # 	for (i in seq(n-sum(sub))) ind[i+1, degs[seq(i)]] <- TRUE
  }
  if (inputIsList & is.logical(ind) && length(ind)==n)
  {
    ind <- matrix(ind,1,n)
  }
  
  # If numeric vector, treat as if each node is added:
  if (is.numeric(ind) & length(dim(ind)) == 1) ind <- as.list(ind)
  
  # If list, add nodes in each step:
  if (is.list(ind))
  {
    indList <- ind
    if (!all(sapply(indList,is.numeric))) stop("Indexes must be numeric in a list")
    ind <- matrix(FALSE,length(indList),n)
    for (i in seq(nrow(ind))) ind[seq(i,nrow(ind)),indList[[i]]] <- TRUE
  }
  
  # Checks:
  if (!is.logical(ind)) stop("ind must be logical")
  if (ncol(ind) != n) stop("Number of columns in ind must correspond to total number of nodes")
  if (inputIsList) if (nrow(ind)!=length(inputList)) stop("Number of frames according to length of input different than according to length of ind")
  
  # Graphs list:
  Graphs <- list()
  
  # Start the loop:
  sub <- NULL
  
  
  ### PRogress bar:
  if (progress)
  {
    message("Computing Graphs")
    pb <- txtProgressBar(min = 0, max = nrow(ind), title = "Computing Graphs:", style = 3)
  }
  
  if (!missing(initLayout)){
    Layout <- initLayout
  } else {
    Layout <- "spring"
  }
  for (i in seq(nrow(ind)))
  {
    if (inputIsList) input <- inputList[[i]]
    subOld <- sub
    sub <- ind[i,]
    
    inputSub <- input[sub,sub]
    
    initNew <- matrix(rnorm(2*sum(sub)),sum(sub),2)
    if (!is.null(subOld)) initNew[subOld[sub],] <- init[sub[subOld]]
    
    layout.par <- list(max.delta = rep(n,sum(sub)), area = n^2.3, repulse.rad = n^2.8,
                       init = initNew)
    
    if (!is.null(subOld)) layout.par$max.delta[subOld[sub]] <- n/constraint
    
    arg2 <- list(Arguments = lapply(arguments,function(x)if(length(x)==n)x[sub] else if (is.matrix(x) && ncol(x)==n&&nrow(x)==n)x[sub,sub] else x))
    class(arg2) <- "qgraph"
    
    # Run qgraph:
    if (length(labels) ==n)
    {
      Graphs[[i]] <- qgraph(inputSub,layout=Layout,layout.par=layout.par,arg2,labels=labels[sub],DoNotPlot = TRUE)
      init <- Graphs[[i]]$layout.orig
    } else 
    {
      Graphs[[i]] <- qgraph(inputSub,layout=Layout,layout.par=layout.par,arg2,labels=labels,DoNotPlot = TRUE)
      init <- Graphs[[i]]$layout.orig
    }
    Layout <- "spring"
    
    if (!is.null(titles)) title(titles[i],line=-1)
    
    ### PRogress bar:
    if (progress)
    {
      setTxtProgressBar(pb, i)
    }
    
  }
  ### PRogress bar:
  if (progress)
  {
    close(pb)
  }
  

  if (smooth)
  {
    ### PRogress bar:
    if (progress)
    {
      message("Smoothing Graphs")
      pb <- txtProgressBar(min = 0, max = n, title = "Smoothing Graphs:", style = 3)
    }
    
    # Extract Layouts
    Layouts <- lapply(Graphs,'[[','layout')
           
    # For every node, smooth:
    for (node in 1:n)
    {      
     # Graphs node is present in:
      GraphsWithNode <- which(ind[,node])
      
      # Node ID in each graph
      NodeIDs <- sapply(GraphsWithNode,function(g)sum(ind[g,1:node]))

      # Coordinates:
      Coord <- do.call(rbind,mapply(l = Layouts[GraphsWithNode], i = NodeIDs, FUN = function(l,i) l[i,], SIMPLIFY = FALSE))
      
      if (nrow(Coord) > 3)
      {
        CoordSmooth <- matrix(,nrow(Coord),ncol(Coord))    
        CoordSmooth[,1] <- predict(loess(I(Coord[,1]) ~ I(1:nrow(Coord))))
        CoordSmooth[,2] <- predict(loess(I(Coord[,2]) ~ I(1:nrow(Coord))))        
      } else 
      {
        CoordSmooth <- Coord 
      }

#       
#      plot(Coord[,2])
#      lines(CoordSmooth[,2])
    
      # Write back coordinates:
      for (g in seq_along(GraphsWithNode))
      {
        Graphs[[GraphsWithNode[g]]]$layout[NodeIDs[g],1] <- CoordSmooth[g,1]
        Graphs[[GraphsWithNode[g]]]$layout[NodeIDs[g],2] <- CoordSmooth[g,2]
      }
      
      ### PRogress bar:
      if (progress)
      {
        setTxtProgressBar(pb, node)
      }
    }
    ### PRogress bar:
    if (progress)
    {
      close(pb)
    }
    
  }
  
  # Plot graphs:
    ### PRogress bar:
    if (progress)
    {
      if (plotGraphs)
      {
        message("Plotting Graphs") 
      } else message("Recomputing Graphs")
      
      pb <- txtProgressBar(min = 0, max = length(Graphs), title = "Plotting Graphs:", style = 3)
    }
    
    
    for (graph in seq_along(Graphs))
    {
      Graphs[[graph]] <- qgraph(Graphs[[graph]], DoNotPlot = !plotGraphs, layout = Graphs[[graph]]$layout )
#       plot(Graphs[[graph]])
      Sys.sleep(sleep)
      
      ### PRogress bar:
      if ( progress)
      {
        setTxtProgressBar(pb, graph)
      }
    }
    
    ### PRogress bar:
    if ( progress)
    {
      close(pb)
    }
  
  # Return graphs:
  invisible(Graphs)
}


