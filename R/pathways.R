# Function that highlights shortest paths in a network:

pathways <- function(
  graph,# Qgraph object
  from, # Vector of from indices
  to, # vector of to indices
  fading = 0.25,
  lty = 3
){
  stopifnot(is(graph,"qgraph"))
  
  # Character:
  if (is.character(from)){
    if (!all(from %in% graph$graphAttributes$Nodes$labels)){
      stop("Node label in 'from' argument does not exist")
    }
    from <- match(from,  graph$graphAttributes$Nodes$labels)
  }
  
  if (is.character(to)){
    if (!all(to %in% graph$graphAttributes$Nodes$labels)){
      stop("Node label in 'to' argument does not exist")
    }
    to <- match(to,  graph$graphAttributes$Nodes$labels)
  }

  
  Cent <- centrality(graph,pkg = "igraph",all.shortest.paths = TRUE)
  SP <- Cent$ShortestPaths
  pathList <- matrix(NA,0,2)
  for (i in from){
    for (j in to){
      pathList <- rbind(pathList,do.call(rbind,lapply(SP[[i,j]],function(x)cbind(x[-length(x)],x[-1]))))
    }
  }
  
  highlight <- rep(FALSE,nrow(pathList))
  for (i in seq_len(nrow(pathList))){
    highlight[i] <- which(graph$Edgelist$from %in% pathList[i,] & graph$Edgelist$to %in% pathList[i,])
  }
  graph$graphAttributes$Edges$color <- Fade(graph$graphAttributes$Edges$color, ifelse(seq_along(graph$Edgelist$from) %in% highlight, 1, fading))
  graph$graphAttributes$Edges$lty <- ifelse(seq_along(graph$Edgelist$from) %in% highlight, 1, lty)

  # Change edgesort to plot changed edges first:
  graph$graphAttributes$Graph$edgesort <- c(
    graph$graphAttributes$Graph$edgesort[!graph$graphAttributes$Graph$edgesort %in% highlight],
    graph$graphAttributes$Graph$edgesort[graph$graphAttributes$Graph$edgesort %in% highlight]
  )
  plot(graph)
}