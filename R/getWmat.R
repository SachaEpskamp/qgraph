library("Matrix")

# Computes the weights matrix
getWmat <- function(x,...)
{
  UseMethod("getWmat")
}

# List:
getWmat.list <- function(x,...)
{
  Res <- lapply(x,getWmat,...)
  return(Res)
#   if (is.null(names(x)))
#   {
#     names(x) <- ""
#   }
#   
#   names(x) <- ifelse(names(x)=="",seq_along(names(x)),names(x))
#   
#   # Check if some objects are psynet objects or psynetGraph objects and adjust accordingly:
#   if (any(sapply(x,class) == "psynet","psynetGraph"))
#   {
#     psynets <- which(sapply(x,class) == "psynet")
#     for (g in psynets)
#     {
#       if (length(psynets) > 1)
#       {
#         names(x[[g]]) <- paste0(g,names(x[[g]]))
#       }
#       x <- c(x,x[[g]])
#     }
#     x <- x[-psynets]
#   }
#   
#   if (any(sapply(x,class) == "psynetGraph"))
#   {
#     psynetGraphs <- which(sapply(x,class) == "psynetGraph")
#     for (g in psynetGraphs)
#     {
#       graph <- x[[g]]$graph
#       if (names(x)[g] == as.character(g))
#       {
#         nam <- x[[g]]$method
#       } else nam <- names(x)[g]
#       x[[g]] <- graph
#       names(x)[g] <- nam
#     }
#   }
#   
#   Wmats <- lapply(x, getWmat)
#   
#   return(Wmats)
}

# Matrix:
getWmat.matrix <- function(x,nNodes,labels, directed = TRUE,...)
{
  if (mode(x)!="numeric") stop("Input matrix must be numeric")
  
  # Weights matrix:
  if (length(unique(dim(x))) == 1)
  {
    if (missing(labels))
    {
      if (!is.null(colnames(x)))
      {
        labels <- colnames(x)
      }
    }
    if (!missing(labels))
    {
      if (!all(length(labels)==dim(x))) stop("Length labels must match dimensions of Weights matrix")
      colnames(x) <- rownames(x) <- labels
    }
    return(x)
  }
  
  if (!ncol(x) %in% c(2,3))
  {
    stop("Edgelist must have either 2 or 3 columns")
  }
  
  if (missing(nNodes))
  {
    if (!missing(labels))
    {
      nNodes <- length(labels)
    } else  nNodes <- max(x[,1:2])
  }
  
  if (!missing(labels))
  {
    if (length(labels) != nNodes) stop("Length of labels must match number of nodes")
  }
  
  
  from <- c(x[,1], x[!directed,2])
  to <- c(x[,2] , x[!directed,1])
  if (ncol(x) == 2)
  {
    w <- rep(1,length(from))
  } else 
  {
    w <- c(x[,3], x[!directed,3]) 
  }
  
  
  # Unweighted Edgelist:
  if ( ncol(x)==2 )
  {
    mat <- as.matrix(1*sparseMatrix(from,to, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  } else 
  {
    mat <- as.matrix(1*sparseMatrix(from,to,x=w, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  }
}


# Data frame (edgelist)
getWmat.data.frame <- function(x,nNodes,labels,directed=TRUE,...)
{
  if (!ncol(x) %in% c(2,3))
  {
    stop("Edgelist must have either 2 or 3 columns")
  }
  if (ncol(x) == 3 && !is.numeric(x[,3])) stop("Third column is not numeric")
  
  # Replace labels with nodes:
  if (is.factor(x[,1])) x[,1] <- as.character(x[,1])
  if (is.factor(x[,2])) x[,2] <- as.character(x[,2])
  
  if (is.character(x[,1]) & !is.character(x[,2]) | !is.character(x[,1]) & is.character(x[,2])) stop("Both from and to columns must be either numeric or character")
  
  if (is.character(x[,1]) & is.character(x[,2]))
  {
    if (missing(labels))
    {
      labels <- unique(c(x[,1], x[,2]))
    }
    
    if (any(!c(x[,1],x[,2]) %in% labels)) stop("labels does not contain all node names.")
    
    x[,1] <- match(x[,1], labels)
    x[,2] <- match(x[,2], labels)
  }
  
  if (missing(nNodes))
  {
    if (!missing(labels))
    {
      nNodes <- length(labels)
    } else  nNodes <- max(x[,1:2])
  }
  
  if (!missing(labels))
  {
    if (length(labels) != nNodes) stop("Length of labels must match number of nodes")
  }
  
  from <- c(x[,1], x[!directed,2])
  to <- c(x[,2] , x[!directed,1])
  if (ncol(x) == 2)
  {
    w <- rep(1,length(from))
  } else 
  {
    w <- c(x[,3], x[!directed,3]) 
  }
  
  
  # Unweighted Edgelist:
  if ( ncol(x)==2 )
  {
    mat <- as.matrix(1*sparseMatrix(from,to, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  } else 
  {
    mat <- as.matrix(1*sparseMatrix(from,to,x=w, dims = c(nNodes,nNodes)))
    if (!missing(labels)) rownames(mat) <- colnames(mat) <- labels
    return(mat)
  }
}


### igraph
getWmat.igraph <- function(x, labels,...)
{
  return(as.matrix(get.adjacency(x)))
}


### qgraph:
getWmat.qgraph <- function(x, directed,...)
{
  if (!is.null(x[['graphAttributes']][['Graph']][['weighted']])) if (!x[['graphAttributes']][['Graph']][['weighted']]) x[['Edgelist']][['weight']] <- ifelse(x[['Edgelist']][['weight']]==0,0,1)
  
  E <- x[['Edgelist']]
  n <- x[['graphAttributes']][['Graph']][['nNodes']]
  
  if (!missing(directed)) E$directed <- directed
  
  from <- c(E$from, E$to[!E$directed | E$bidir])
  to <- c(E$to , E$from[!E$directed | E$bidir])
  w <- c(E$weight, E$weight[!E$directed | E$bidir])
  
  mat <- as.matrix(1*sparseMatrix(from,to,x=w, dims = c(n,n)))
  rownames(mat) <- colnames(mat) <- x$graphAttributes$Nodes$labels
  return(mat)
}

