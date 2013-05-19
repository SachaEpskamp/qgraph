# library("igraph")

as.igraph.qgraph <- function(object,attributes=TRUE)
{
  if (!"qgraph"%in%class(object))
  {
    stop("Input must be qgraph object")
  }
  
  # Extract graph:
  edgesort <- object$graphAttributes$Graph$edgesort
  E <- as.matrix(as.data.frame(object$Edgelist[c("from","to")]))
  E <- E[edgesort,]
  srt <- cbind(pmin(E[,1],E[,2]),pmax(E[,1],E[,2]))
  Dir <- object$Edgelist$directed[edgesort]
  Bi <-  object$Edgelist$bidirectional[edgesort]
  
  Graph <- graph.edgelist(E, any(Dir))
  E(Graph)$weight <- object$Edgelist$weight[edgesort]
  
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
    V(Graph)$frame.color <- object$graphAttributes$Nodes$border.color
    V(Graph)$frame.color[!object$graphAttributes$Nodes$borders] <- NA
#     V(Graph)$label.cex <- object$graphAttributes$Nodes$label.cex
    V(Graph)$label.color <- object$graphAttributes$Nodes$label.color
    V(Graph)$label <- object$graphAttributes$Nodes$labels
    V(Graph)$shape <- object$graphAttributes$Nodes$shape
    V(Graph)$shape[!V(Graph)$shape%in%c("circle", "square", "csquare", "rectangle", "crectangle", "vrectangle", "pie")] <- "none"
    V(Graph)$color <- object$graphAttributes$Nodes$color
    V(Graph)$size <- object$graphAttributes$Nodes$width / max((-1/72)*(object$nNodes)+5.35,1) * 4
    V(Graph)$size2 <- object$graphAttributes$Nodes$height / max((-1/72)*(object$nNodes)+5.35,1) * 4
    
    ## Edge attributes:
    E(Graph)$curved <- object$graphAttributes$Edges$curve[edgesort]
    E(Graph)$color <- object$graphAttributes$Edges$color[edgesort]
    if (is.character(object$graphAttributes$Edges$labels)) E(Graph)$label <- object$graphAttributes$Edges$labels[edgesort]
    if (!is.null(object$graphAttributes$Edges$label.color)) E(Graph)$label.color <- object$graphAttributes$Edges$label.color[edgesort]
    E(Graph)$lty <- object$graphAttributes$Edges$lty[edgesort]
    E(Graph)$width <- object$graphAttributes$Edges$width[edgesort]
  }
  
  return(Graph)
}
