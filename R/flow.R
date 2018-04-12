flow2 <- function(
  object, # qgraph object
  from, # Node of origin
  horizontal = TRUE,
  sizeOrig = 10,
  sizeCon = 3,
  sizeDiscon = 1,
  fadingStyle = c("gradual","split","default","off"), # proportional fading to distance?
  maxFade = 0.25,
  xScale = 1,
  ... # Qgraph arguments
){
  fadingStyle <- match.arg(fadingStyle)
  # Test input:
  if (!is(object,"qgraph")){
    warning("Input is not a qgraph object, runnin gqgraph")
    object <- qgraph(object, ..., DoNotPlot = TRUE)
  }
  if (length(from)!=1){
    stop("'from' must be of length 1")
  }
  
  # Obtain edgelist:
  E <- as.data.frame(object$Edgelist)
  
  # If not fully connected, stop:
  foo <- capture.output(comps <- sna::components(getWmat(object)))
  if (comps > 1){
    stop("Disconnected graph is not yet supported.")
  }
  
  # ID all edges:
  E$id <- seq_len(nrow(E))
  
  # Now we will create new edgelists for every part of the plot. The first will simply connect the target node to its neighbors:
  flowE <- list()
  curPart <- 1
  
  # Enter the first part (relabel later)
  part1 <- E[E[,1] == from,c("from","to","weight","id")]
  part2 <- E[E[,2] == from,c("to","from","weight","id")]
  names(part2) <- c("from","to","weight","id")
  flowE[[curPart]] <- rbind(part1,part2)
  
  # remaining edges:
  remainIDs <- E$id[!E$id %in% flowE[[curPart]]$id]
  
  # While edges remain, add elements
  while (length(remainIDs)>0){
    curPart <- curPart + 1
    part1 <- E[E[,1] %in% flowE[[curPart-1]]$to,c("from","to","weight","id")]
    part2 <-  E[E[,2]  %in% flowE[[curPart-1]]$to,c("to","from","weight","id")]
    names(part2) <- c("from","to","weight","id")
    flowE[[curPart]] <- rbind(part1,part2)
    flowE[[curPart]] <- flowE[[curPart]][flowE[[curPart]]$id %in% remainIDs,]
    
    # remaining edges:
    remainIDs <- remainIDs[!remainIDs %in% flowE[[curPart]]$id]
  }

  
  # Now loop over elements to rename nodes:
  nNodes <- object$graphAttributes$Graph$nNodes
  allNodes <- seq_len(nNodes)
  targetNodes <- allNodes[allNodes!=from]
  nTarget <- length(targetNodes)
  translateNodes <- NULL
  for (c in seq_along(flowE)){
    if (c > 1){
      flowE[[c]]$from <- translateNodes[match(flowE[[c]]$from ,targetNodes)]
    } else {
      flowE[[c]]$from[] <- 1
    }
    translateNodes <- 1 + ((c-1) * nTarget) + seq_len(nTarget)
    flowE[[c]]$to <- translateNodes[match(flowE[[c]]$to ,targetNodes)]
  }
  
  # Compute the layout:
  L <- matrix(0,1,2)
  for (c in seq_along(flowE)){
    L <- rbind(L,
        cbind(seq(-1,1,length=nTarget),c)
    )
  }

  
  # Labels:
  Labels <- object$graphAttributes$Nodes$labels
  newLabs <- c(Labels[from],rep(Labels[-from],length(flowE)))
  
  # Edgelist:
  flowE <- do.call(rbind,flowE)
  
  # Compute sizes:
  allNodes <- 1:translateNodes[length(translateNodes)]
  ConnectedNodes <- unlist(flowE[,1:2])
  vSize <- ifelse(allNodes %in% ConnectedNodes, sizeCon, sizeDiscon)
  vSize[1] <- sizeOrig
  
  # Proportional fading:
  if (fadingStyle == "default"){
    Fade <- TRUE
  } else if (fadingStyle == "off"){
    Fade <- FALSE
  } else if (fadingStyle == "gradual"){
    # Make fading proportional to x location of orin node
    Fade <- 1-(L[flowE[,1],2]/max(L[flowE[,1],2])  * (1-maxFade))
  } else {
    Fade <- ifelse(L[flowE[,1],2]==0,1,maxFade)
  }
  
  # Rescale x:
  L[,2] <- L[,2]^{1/xScale}
  
  # Horizontal:
  if (horizontal){
    L <- L[,2:1]
  }
  
  # Plot:
  qgraph(flowE[,1:3], labels = newLabs, layout = L, directed = FALSE, curve = 0,
         vsize = vSize, object, DoNotPlot = FALSE,fade=Fade,...)
}
