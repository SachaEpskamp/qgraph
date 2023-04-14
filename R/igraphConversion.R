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
    V(Graph)$size <- x$graphAttributes$Nodes$width / max((-1/72)*(x$nNodes)+5.35,1) * 4
    V(Graph)$size2 <- x$graphAttributes$Nodes$height / max((-1/72)*(x$nNodes)+5.35,1) * 4
    
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
