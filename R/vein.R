flow <- function(
  object, # qgraph object
  from, # Node of origin
  horizontal = TRUE,
  equalize = TRUE,
  minCurve = 1,
  maxCurve = 4,
  unfadeFirst = FALSE,
  fade = TRUE,
  labels, # Same as qgraph
  # sizeOrig = 10,
  # sizeCon = 3,
  # sizeDiscon = 1,
  # fadingStyle = c("gradual","split","default","off"), # proportional fading to distance?
  # maxFade = 0.25,
  # xScale = 1,
  ... # Qgraph arguments
){
  # Test input:
  if (!is(object,"qgraph")){
    warning("Input is not a qgraph object, runnin gqgraph")
    object <- qgraph(object, ..., DoNotPlot = TRUE)
  }
  if (length(from)!=1){
    stop("'from' must be of length 1")
  }
  
  if (missing(labels)){
    labels <- object$graphAttributes$Nodes$labels
  }
  
  # Obtain edgelist:
  E <- as.data.frame(object$Edgelist)
  
  # If not fully connected, stop:
  Adj <- 1*(getWmat(object)!=0)
  diag(Adj) <- 0
  Laplacian <- diag(rowSums(Adj)) - Adj
  evLapl <- round(eigen(Laplacian)$values,10)
  comps <- sum(evLapl == 0)
  if (comps > 1){
    stop("Disconnected graph is not yet supported.")
  }
  
  # ID all edges:
  E$id <- seq_len(nrow(E))
  
  
  # Subset the edgelist as to retain only the simple vein-strucure outward of a node. Recursively, outward from the target node, add edges connected to the current nodes to nodes that are not currently in the vein graph
  
  
  # From as character:
  if (is.character(from)){
    if (missing(labels) || !is.character(labels)) stop("No labels supplied")
    from <- which(labels == from)
  }
  
  # First part:
  part1 <- E[E[,1] == from,c("from","to","weight","id")]
  part2 <- E[E[,2] == from,c("to","from","weight","id")]
  names(part2) <- c("from","to","weight","id")
  VeinEdgelist <- rbind(part1,part2)
  
  # Recurse:
  repeat{
    # Currently in vein graph:
    currentNodes <- unique(c(unlist(VeinEdgelist[,1:2])))
    
    # Connected to current nodes:
    part1 <- E[E[,1] %in% currentNodes & !E[,2] %in% currentNodes,c("from","to","weight","id")]
    part2 <- E[E[,2] %in% currentNodes & !E[,1] %in% currentNodes,c("to","from","weight","id")]
    names(part2) <- c("from","to","weight","id")
    Connected <- rbind(part1,part2)
    
    # If no connected, break:
    if (nrow(Connected) == 0){
      break
    }
    
    # Else, add to veingraph:
    VeinEdgelist <- rbind(VeinEdgelist,Connected)
  }
  
  # Now run through Reingold_Tilford:
  iG <- igraph::graph_from_edgelist(as.matrix(VeinEdgelist[,1:2]),directed = FALSE)
  Layout <- igraph::layout_as_tree(iG, root = from, mode = "all")
  
  # Equalize levels if needed:
  if ( equalize){
    Layout[,1] <- ave(Layout[,1],Layout[,2],FUN=function(x)seq(0,1,length=length(x)+2)[-c(1,length(x)+2)])
  }

  # Set curve as a function of distance:
  dist <- abs(Layout[,1][E[,1]] - Layout[,1][E[,2]])
  minDist <- min(dist[round(dist,10)!=0])
  maxDist <- max(dist[dist!=0])
  curve <- ifelse(Layout[,1][E[,1]] == Layout[,1][E[,2]],
                  0,minCurve +  (dist - minDist) / (maxDist - minDist) * (maxCurve - minCurve) )

  
  # Curve all edges that are on same level:
  Curve <- ifelse(Layout[,2][E[,1]] == Layout[,2][E[,2]],
              # Curve negative or positive:
    ifelse(Layout[,1][E[,1]] > Layout[,1][E[,2]],
                         curve, -curve),
    # Else no curve:
    0)
  
  # 
  # qgraph(VeinEdgelist[,1:3],layout=Layout)
  # qgraph(E[,1:3],layout=Layout, directed = FALSE,
  #        curve = Curve)
  # 
  #ECPs:
  # All curved edges should be connected to right or bottom
  ECP <- matrix(NA,nrow(E),nrow(E))
  ECP[Curve!=0,] <- pi
  
  # Now turn
  if (horizontal){
    Layout <- Layout[,2:1]
    Layout[,1] <- -Layout[,1]
    ECP[Curve!=0,] <- pi/2
  }
  
  if (unfadeFirst){
    fade <- ifelse(E[,1] %in% from | E[,2] %in% from, FALSE, NA)
  }
  
  # Plot:
  # qgraph(object, layout = Layout, curve = Curve, edgeConnectPoints = ECP, curveScale = FALSE,fade=fade,labels=labels, ...)
  
  
  qgraph(as.matrix(E[,1:3]), layout = Layout, 
         curve = Curve, 
         edgeConnectPoints = ECP, 
         curveScale = FALSE,
         fade=fade,
         labels=labels,
         object,
         directed = object$Edgelist$directed,
         bidirectional = object$Edgelist$bidirectional,
         ...)
  
}
