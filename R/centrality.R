
centrality <- function(graph,alpha=1,posfun=abs)
{
  # Check for correct class:
  if (class(graph) != "qgraph") stop("Must be a 'qgraph' object")
  
  if (!is.null(graph[['weighted']])) if (!graph[['weighted']]) graph[['qgraphEdgelist']][['weight']] <- ifelse(graph[['qgraphEdgelist']][['weight']]==0,0,1)
  
  # Extract edgelist:
  E <- graph[['qgraphEdgelist']]
  
  # Number of nodes:
  n <- graph$nNodes
  
  ## Convert to adjacency:
  W <- matrix(0,n,n)
  for (i in 1:length(E$from))
  {
    if (E$weight[i]!=0)
    {
      W[E$from[i],E$to[i]] <- E$weight[i]
      if (!E$directed[i] | E$bidir[i]) W[E$to[i],E$from[i]] <- E$weight[i]
    }
  }
  
  ## Compute adjacency:
  X <- 1L * (W!=0)
  
  ## Compute default measures:
  UnweightedDegreesOut <- rowSums(X)
  WeightedDegreesOut <- rowSums(W)
  CombinedDegreesOut <- UnweightedDegreesOut^(1-alpha) * WeightedDegreesOut^alpha
  
  UnweightedDegreesIn <- colSums(X)
  WeightedDegreesIn <- colSums(W)
  CombinedDegreesIn <- UnweightedDegreesIn^(1-alpha) * WeightedDegreesIn^alpha
  
  # Compute shortest distance using Dijkstra (code based on pseudo code on Wikipedia)
  # Setup:
  DistMat <- 1/(ifelse(posfun(W)==0,0,posfun(W)^alpha))
  ShortestPaths <- matrix(Inf,n,n)
  ls <- list()
  for (i in 1:n^2) ls[[i]] <- numeric(0)
  Previous <- structure(ls, .Dim = c(n, n))
  
  # Main loop:
  for (source in 1:n)
  {
    dist <- rep(Inf,n) 
    #previous <- integer(n)                # Previous node in optimal path from source
    dist[source] <- 0                     # Distance from source to source
    Q <- 1:n                              # All nodes in the graph are unoptimized - thus are in Q
    while (length(Q) > 0)                 # The main loop
    {
      u <- Q[which.min(dist[Q])]
      if (dist[u] == Inf)  break          # all remaining vertices are inaccessible from source
      Q <- Q[- which(Q==u)]
      for (v in Q)                        # where v has not yet been removed from Q.
      {
        alt <- dist[u] + DistMat[u,v] 
        if (alt < dist[v])                # Relax (u,v,a)
        {
            dist[v] <- alt 
            Previous[[source,v]] <- which(dist + DistMat[,v] == alt)
            #previous[v] <- u 
         #   decrease-key v in Q          # Reorder v in the Queue
        }
      }
    }
    ShortestPaths[source,] <- dist
  }
  
  # Compute Closeness:
  Closeness <- 1/rowSums(ShortestPaths)
  

  
  # Shortest paths function:
  sp <- function(i,j)
  {
    if (length(Previous[[i,j]])==0) return(list())
    if (all(Previous[[i,j]] == i)) return(list(c(i,j)))
    paths <- do.call(c,lapply(Previous[[i,j]],sp,i=i))
    paths <- lapply(paths,function(x)c(x,j))
    return(paths)
  }
  
  # Compute shortest paths:
  Paths <- structure(ls, .Dim = c(n, n))
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      Paths[[i,j]] <- sp(i,j)
    }
  }
  
  # Number of shortest paths:
  NumPaths <- apply(Paths,1:2,sapply,length)
  
  # Betweenness dummy:
  Betweenness <- numeric(n)
  
  Gtot <- apply(Paths,1:2,sapply,length)
  # Compute betweenness:
  for (i in 1:n)
  {
    G <- apply(Paths,1:2,sapply,function(x)sum(i==unlist(x)))
    Grat <- G[-i,-i]/Gtot[-i,-i]
    Betweenness[i] <- sum(Grat[!is.nan(Grat)])
  }
  
  ### RETURN VALUES:
  retval <- list(
    OutDegree = CombinedDegreesOut,
    InDegree = CombinedDegreesIn,
    Closeness = Closeness,
    Betweenness = Betweenness,
    ShortestPathLengths = ShortestPaths,
    ShortestPaths = Paths)
    
    return(retval)  
}