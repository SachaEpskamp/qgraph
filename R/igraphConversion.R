# library("igraph")

as.igraph.qgraph <- function(x,...,attributes=TRUE)
{
  if (!"qgraph"%in%class(x))
  {
    stop("Input must be qgraph x")
  }

  # Extract graph:
  edgesort <- x$graphAttributes$Graph$edgesort
  E <- as.matrix(as.data.frame(x$Edgelist[c("from","to")]))
  E <- E[edgesort,,drop=FALSE]
  srt <- cbind(pmin(E[,1],E[,2]),pmax(E[,1],E[,2]))
  Dir <- x$Edgelist$directed[edgesort]
  Bi <-  x$Edgelist$bidirectional[edgesort]
  
  Graph <- graph.edgelist(E, any(Dir))
  # graph.edgelist only creates vertices up to the highest node number used in
  # an edge, so nodes without edges (or only trailing isolated nodes) would be
  # missing and setting vertex attributes of length nNodes would fail:
  nNodes <- x$graphAttributes$Graph$nNodes
  if (vcount(Graph) < nNodes)
  {
    Graph <- add_vertices(Graph, nNodes - vcount(Graph))
  }
  E(Graph)$weight <- x$Edgelist$weight[edgesort]
  
  # Arrow mode:
  aMode <- ifelse(Dir,2,0)
  # Set duplicated and bidir to doubleheaded:
  aMode <- ifelse(Bi & (duplicated(srt)|duplicated(srt,fromLast=TRUE)), 3, aMode)
  # Store in graph:
  E(Graph)$arrow.mode <- aMode
  
  ## Set attributes:
  if (attributes)
  {
    ## Node attributes:
    V(Graph)$frame.color <- x$graphAttributes$Nodes$border.color
    V(Graph)$frame.color[!x$graphAttributes$Nodes$borders] <- NA
#     V(Graph)$label.cex <- x$graphAttributes$Nodes$label.cex
    V(Graph)$label.color <- x$graphAttributes$Nodes$label.color
    V(Graph)$label <- x$graphAttributes$Nodes$labels
    V(Graph)$shape <- x$graphAttributes$Nodes$shape
    V(Graph)$shape[!V(Graph)$shape%in%c("circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie")] <- "none"
    V(Graph)$color <- x$graphAttributes$Nodes$color
    # qgraph's vsize and igraph's vertex.size use different units; on equally
    # sized devices, node diameters match when vertex.size = 3.38 * vsize - 1.35
    # (measured empirically). The previous formula referenced x$nNodes, which
    # does not exist in a qgraph object, so its denominator silently evaluated
    # to 1 and nodes came out roughly 25% too large:
    V(Graph)$size <- pmax(3.38 * x$graphAttributes$Nodes$width - 1.35, 1)
    V(Graph)$size2 <- pmax(3.38 * x$graphAttributes$Nodes$height - 1.35, 1)
    
    ## Edge attributes:
    E(Graph)$curved <- x$graphAttributes$Edges$curve[edgesort]
    E(Graph)$color <- x$graphAttributes$Edges$color[edgesort]
    if (is.character(x$graphAttributes$Edges$labels)) E(Graph)$label <- x$graphAttributes$Edges$labels[edgesort]
    if (!is.null(x$graphAttributes$Edges$label.color)) E(Graph)$label.color <- x$graphAttributes$Edges$label.color[edgesort]
    E(Graph)$lty <- x$graphAttributes$Edges$lty[edgesort]
    E(Graph)$width <- x$graphAttributes$Edges$width[edgesort]
  }
  
  return(Graph)
}
