

qgraph.animate <- function(input,ind=NULL,...,constraint=10,growth="order",titles=NULL,sleep=0)
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
  
  
  # Start the loop:
  sub <- NULL
  
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
    if (length(labels) ==n) init <- qgraph(inputSub,layout="spring",layout.par=layout.par,arg2,labels=labels[sub])$layout.orig
    else init <- qgraph(inputSub,layout="spring",layout.par=layout.par,arg2,labels=labels)$layout.orig
    
    if (!is.null(titles)) title(titles[i],line=-1)
    Sys.sleep(sleep)
  }
}


