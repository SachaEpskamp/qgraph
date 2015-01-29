qgraphMixed <- function(
  undirected, # Adjacency matrix or edgelist of undirected network
  directed, # Adjacency matrix or edgelist of directed network
  parallel = TRUE, # Sent to parallelEdge, to indicate this function uses different default
  parallelAngle = pi/6, # Used similarly as parallelAngle in qgraph
  diagUndirected = FALSE, # Include diagonal of undirected graph
  diagDirected = TRUE, # include diagonal of directed graph
  ltyUndirected = 1,
  ltyDirected = 1,
  curve = 1,
  ... # qgraph arguments
){

  
  # Test if undirected is adjacency matrix and turn it into edgelist:
  if (nrow(undirected) == ncol(undirected)){
    if ((ncol(undirected) == 3 & nrow(undirected) == 3) | (ncol(undirected) == 2 & nrow(undirected) == 2)){
      message("Treating 2x2 and 3x3 matrices as adjacency matrix")
    }
    incl <- upper.tri(undirected, diag=diagUndirected)
    undirected <- cbind(
      row(undirected)[incl],
      col(undirected)[incl],
      undirected[incl])
  }
  # Check if three columns. If not, append third:
  if (ncol(undirected) == 2){
    undirected <- cbind(undirected,1)
  }
  
  
  # Test if undirected is adjacency matrix and turn it into edgelist:
  if (nrow(directed) == ncol(directed)){
    if ((ncol(directed) == 3 & nrow(directed) == 3) | (ncol(directed) == 2 & nrow(directed) == 2)){
      message("Treating 2x2 and 3x3 matrices as adjacency matrix")
    }
    incl <- matrix(TRUE,nrow(directed), ncol(directed))
    if (!diagDirected){
      diag(incl) <- FALSE
    }
    directed <- cbind(
      row(directed)[incl],
      col(directed)[incl],
      directed[incl])
  }
  # Check if three columns. If not, append third:
  if (ncol(directed) == 2){
    directed <- cbind(directed,1)
  }
  
  # append:
  Edgelist <- rbind(undirected, directed)
  if (all(directed[,3] == 1)){
    Edgelist <- Edgelist[,1:2]
  }
  
  # Create directed vector:
  Directed <- c(rep(FALSE,nrow(undirected)), rep(TRUE, nrow(directed)))
  lty <- c(rep(ltyUndirected,nrow(undirected)), rep(ltyDirected, nrow(directed)))
  
  # Create parallelAngle vector:
  parallelAngle <- ifelse(Directed, -parallelAngle, 0)
  
  # Curve (if not parallel):
  if (parallel){
    Curve <- NULL
  } else {
    Curve <- ifelse(Directed,-curve,0)
  }
  
  # Run qgraph:
  invisible(qgraph(Edgelist, parallelEdge  = parallel, parallelAngle = parallelAngle,
                   directed = Directed, curve = Curve, lty = lty, ...))
}
