# Inner function to compute next graph:
nextcomb <- function(x)
{
  x <- as.integer(x)
  n <- length(x)
  if (n==0L) stop("'x' needs to have length > 0")
  if (!all(x)%in%c(0L,1L)) stop("'x' needs to contain only zeroes and ones")
  
  # if all one, stop:
  if (all(x==1)) stop("No next combination")
  
  # If all zero, first becomes 1:
  if (all(x==0L))
  {
    x[1] <- 1L
    return(x)
  }
  
  # If from left to right all ones then zeroes. Set to zero and set next to one:
  dif <- diff(x)
  lastOne <- rev(which(x==1L))[1]
  
  if (sum(dif!=0)==1 & dif[lastOne] == -1 & lastOne < n)
  {
    x[] <- 0L
    x[lastOne+1] <- 1L
    return(x)
  } else {
    # Else, rerun algorithm for part up to last one:
    return(c(nextcomb(x[1:(lastOne-1)]),x[lastOne:n])) 
  } 
}

# Innfer function to compute all possible stepups or stepdowns:
stepGraphs <- function(x, step = "up")
{
  inds <- which(x==as.integer(step=="down"))
  lapply(inds,function(i){
    y <- x
    y[i] <- as.integer(step=="up")
    return(y)
  })
}

# inner function to compute EBIC given a graphvec:
ggmEBIC <- function(GraphVec,type,nNodes,S,n,gamma)
{
  Graph <- matrix(0,nNodes,nNodes)
  Graph[upper.tri(Graph)] <- GraphVec
  Graph <- Graph + t(Graph)
  rownames(Graph) <- rownames(S)
  colnames(Graph) <- colnames(S)
  
  if (type == "cor")
  {
    Res <- ggm::fitCovGraph(Graph, S, n)
  } else if (type == "pcor")
  {
    Res <- ggm::fitConGraph(Graph, S, n)
  } else stop("Wrong type specified")
  
  EBIC(S, solve(Res$Shat), n, gamma)
}


# Finds the best fitting graph using brute force. Using the fitCovGraph and fitConGraph functions from GGM:
findGraph <- function(
  S, # Sample covariance or correlation matrix, or data frame
  n, # Number of observations
  type = "cor", # or pcor
  gamma = 0.5, # EBIC tuning parameter
  method = c('stepup','stepdown','brute'), # Method, pick first one
  reverseSteps = TRUE, # Try reversing steps in stepup and stepdown.
  startSig = TRUE # If stepup or stepdown, start with significant edges only according to Holm adjustment?
)
{
  stopifnot(!missing(S)|!missing(n))
  
  if (is.data.frame(S))
  {
    S <- cor_auto(S)
  }
  
  S <- cov2cor(S)
  
  if (type == "pcor"){
    S <- corpcor::cor2pcor(S)
  }
  
  # Check method:
  method <- method[[1]]
  if (!method %in% c('brute','stepup','stepdown'))
  {
    stop("Unsupported method")
  }
  
  # Check if S is proper correlation or covariance marix:
  if (!isSymmetric(S))
  {
    stop("'S' is not symmetric, thus can not be a correlation or covariance matrix.")
  }
  
  # Check for positive definiteness (glasso does its own check):
  if(!all(eigen(S)$values > 0))  {
    stop("Correlation/covariance matrix is not positive definite.")
  }
  
  # Number of nodes:
  nNodes <- ncol(S)
  
  # Number of edges:
  nEdges <- nNodes*(nNodes-1)/2
  
  # Fix names:
  if (is.null(colnames(S)))
  {
    colnames(S) <- 1:nNodes
  }
  if (is.null(rownames(S)))
  {
    rownames(S) <- 1:nNodes
  }
  
  # Number of iterations:
  nIter <- 2^nEdges
  
  
  
  ### BRUTE FORCE TECHNIQUE ###
  if (method == "brute")
  {
    # Initial graph:
    GraphVec <- rep(0L, nEdges)
    
    # Best graph:
    Best <- list(
      graph = GraphVec,
      EBIC = Inf)
    
    pb <- txtProgressBar(min=0,max=nIter,style = 3, title = "Testing models")
    
    it <- 1
    while(it <= nIter)
    {
      
      ebic <- ggmEBIC(GraphVec,type,nNodes,S,n,gamma)
      if (ebic < Best$EBIC)
      {
        Best$EBIC <- ebic
        Best$graph <- GraphVec
      }
      
      if (it < nIter)
      {
        GraphVec <- nextcomb(GraphVec)
        setTxtProgressBar(pb, it)
      }
      it <- it+1
    }
    
    close(pb)
  } else {
    
    step <- ifelse(method == "stepup", "up", "down")
    
    # Initial graph:
    if (startSig)
    {
      nadj <- n
      if (type == "pcor")
      {
        nadj <- nadj - (nNodes - 2)
      }
      
      if (all(diag(S)==1)) 
      {
        pvals <- psych::corr.p(S,n = nadj, adjust = "holm", alpha = 0.05)$p
      } else {
        pvals <- psych::corr.p(cov2cor(S), n = nadj, adjust = "holm", alpha = 0.05)$p
      }
      
      # Symmetrize:
      pvals[lower.tri(pvals)] <- t(pvals)[lower.tri(pvals)]
      
      # Remove insignificant edges:
      GraphVec <- 1*(pvals < 0.05)[upper.tri(pvals)]
    } else GraphVec <- rep(ifelse(step=="up",0L,1L), nEdges)
    
    # Best graph:
    Best <- list(
      graph = GraphVec,
      EBIC = ggmEBIC(GraphVec,type,nNodes,S,n,gamma))
    
    # Start stepping:
    stopsoon <- FALSE
    
    repeat{
      propGraphs <- stepGraphs(GraphVec, step)
      EBICs <- sapply(propGraphs,ggmEBIC,type=type,nNodes=nNodes,S=S,n=n,gamma=gamma)
      
      if (!any(EBICs < Best$EBIC))
      {
        if (reverseSteps & !stopsoon)
        {
          if (step=="up")
          {
            step <- "down"
          } else step <- "up"
          stopsoon <- TRUE
        } else {
         break
        }
      } else {
        i <- which.min(EBICs)
        Best$graph <- GraphVec <-propGraphs[[i]]
        Best$EBIC <- EBICs[i] 
        stopsoon <- FALSE
      }
    }
  }
  
  Graph <- matrix(0,nNodes,nNodes)
  Graph[upper.tri(Graph)] <- Best$graph
  Graph <- Graph + t(Graph)
  rownames(Graph) <- rownames(S)
  colnames(Graph) <- colnames(S)
  
  if (type == "cor")
  {
    Res <- ggm::fitCovGraph(Graph, S, n)
    S <- Res$Shat
    diag(Graph) <- 1
    S <- as.matrix(forceSymmetric(Graph*S))
    return(return(cov2cor(S)))
  } else if (type == "pcor")
  {
    Res <- ggm::fitConGraph(Graph, S, n)
    K <- solve(Res$Shat)
    diag(Graph) <- 1
    K <- as.matrix(forceSymmetric(Graph*K))
    return(wi2net(K))
  } else stop("Wrong type specified")
}
