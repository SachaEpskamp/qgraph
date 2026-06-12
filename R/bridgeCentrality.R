# Bridge centrality statistics (Jones, Ma, & McNally, 2021).
#
# Bridge centrality was developed by Payton J. Jones and first implemented in
# the 'networktools' package (Jones, 2017). As 'networktools' is licensed
# under GPL-3, the code below is an independent implementation written for
# qgraph (GPL-2) based on the published definitions, designed to reproduce
# the results of networktools::bridge().

bridgeCentrality <- function(
    graph,                   # A qgraph object, weights matrix, or any object usable by getWmat
    communities,             # Community vector/factor, list, or igraph 'communities' object
    useCommunities = "all",  # Subset of communities to use, or "all"
    directed,                # Logical, detected from symmetry of the weights matrix when missing
    normalize = FALSE,       # Divide each statistic by its highest possible value?
    labels                   # Optional vector of node labels
){
  # Default communities to the groups of a qgraph object:
  if (missing(communities) || is.null(communities)){
    if (is(graph, "qgraph") && !is.null(graph$Arguments$groups)){
      communities <- graph$Arguments$groups
    } else {
      stop("'communities' may not be missing. Assign each node to a community via the 'communities' argument.")
    }
  }

  W <- getWmat(graph)
  if (is.list(W)){
    stop("'bridgeCentrality' does not support multiple graphs. Use a single graph as input.")
  }

  nNodes <- ncol(W)
  if (missing(labels) || is.null(labels)){
    labels <- colnames(W)
    if (is.null(labels)) labels <- paste0("V", seq_len(nNodes))
  }
  rownames(W) <- colnames(W) <- labels

  if (any(is.na(W))){
    W[is.na(W)] <- 0
    message("Note: NA edge weights are treated as 0.")
  }

  if (missing(directed) || is.null(directed)){
    directed <- !isSymmetric(unname(W))
  }

  # Self-loops are not bridges and are not included:
  diag(W) <- 0

  # igraph 'communities' object:
  if (inherits(communities, "communities")){
    communities <- as.character(igraph::membership(communities))
  }
  # List with node indices per community:
  if (is.list(communities)){
    if (is.null(names(communities)) || any(names(communities) == "")){
      stop("All elements of a 'communities' list must be named.")
    }
    comVec <- rep(NA_character_, nNodes)
    for (i in seq_along(communities)){
      comVec[communities[[i]]] <- names(communities)[i]
    }
    if (any(is.na(comVec))){
      stop("Invalid 'communities' list: every node must be assigned to a community.")
    }
    communities <- comVec
  }
  communities <- as.character(communities)
  if (length(communities) != nNodes){
    stop("Length of 'communities' does not match the number of nodes.")
  }

  fullLabels <- labels
  fullCommunities <- communities

  # Restrict the network to a subset of communities:
  keep <- rep(TRUE, nNodes)
  if (!identical(useCommunities, "all")){
    if (!all(useCommunities %in% communities)){
      stop("'useCommunities' contains communities that are not present in 'communities'.")
    }
    keep <- communities %in% useCommunities
    W <- W[keep, keep, drop = FALSE]
    labels <- labels[keep]
    communities <- communities[keep]
  }

  if (length(unique(communities)) < 2){
    stop("At least two communities are needed to compute bridge centrality.")
  }

  nUsed <- ncol(W)
  # crossComm[i,j] is TRUE if nodes i and j belong to different communities:
  crossComm <- outer(communities, communities, "!=")

  ### Bridge strength: sum of absolute edge weights connecting a node to other communities.
  absW <- abs(W)
  bridgeOut <- rowSums(absW * crossComm)
  bridgeIn <- colSums(absW * crossComm)
  bridgeStrength <- if (directed) bridgeIn + bridgeOut else bridgeOut

  ### Shortest-path based measures use distances equal to inverse edge weights.
  g <- igraph::graph_from_adjacency_matrix(W, mode = ifelse(directed, "directed", "upper"),
                                           diag = FALSE, weighted = TRUE)
  igraph::E(g)$weight <- 1 / igraph::E(g)$weight
  # Negative edges are not defined for shortest paths and are excluded:
  gPos <- g
  if (igraph::ecount(gPos) > 0 && any(igraph::E(gPos)$weight < 0)){
    gPos <- igraph::delete_edges(gPos, which(igraph::E(gPos)$weight < 0))
  }

  ### Bridge betweenness: number of times a node lies on a shortest path between
  ### two nodes from different communities.
  bridgeBetweenness <- numeric(nUsed)
  for (i in seq_len(nUsed)){
    paths <- suppressWarnings(igraph::all_shortest_paths(
      gPos, from = i, to = which(crossComm[i, ]), mode = "out"))$res
    for (p in paths){
      p <- as.integer(p)
      if (length(p) > 2){
        mid <- p[-c(1, length(p))]
        bridgeBetweenness[mid] <- bridgeBetweenness[mid] + 1
      }
    }
  }
  # In undirected networks every pair was visited in both directions:
  if (!directed) bridgeBetweenness <- bridgeBetweenness / 2

  ### Bridge closeness: inverse of the average distance from a node to all nodes
  ### outside its community.
  D <- try(igraph::distances(g, mode = "all"), silent = TRUE)
  if (inherits(D, "try-error")){
    # Negative edge weights present; distances are based on positive edges only:
    D <- igraph::distances(gPos, mode = "all")
  }
  bridgeCloseness <- sapply(seq_len(nUsed), function(i){
    d <- D[i, crossComm[i, ]]
    1 / mean(d[is.finite(d)])
  })

  ### Bridge expected influence (1-step): sum of signed edge weights connecting
  ### a node to other communities (outgoing edges in directed networks).
  bridgeEI1 <- rowSums(W * crossComm)

  ### Bridge expected influence (2-step): adds the indirect influence on other
  ### communities via intermediate nodes, weighted by the first edge weight.
  comms <- unique(communities)
  # commInfluence[k,c]: summed edge weights from node k to nodes in community c:
  commInfluence <- W %*% sapply(comms, function(cc) as.numeric(communities == cc))
  totalInfluence <- rowSums(W)
  bridgeEI2 <- bridgeEI1
  for (c in seq_along(comms)){
    idx <- communities == comms[c]
    # Influence of each intermediate node on all communities other than that of node i:
    bridgeEI2[idx] <- bridgeEI1[idx] +
      as.vector(W[idx, , drop = FALSE] %*% (totalInfluence - commInfluence[, c]))
  }

  names(bridgeIn) <- names(bridgeOut) <- names(bridgeStrength) <- labels
  names(bridgeBetweenness) <- names(bridgeCloseness) <- labels
  names(bridgeEI1) <- names(bridgeEI2) <- labels

  # Pad nodes from unused communities with NA:
  if (!all(keep)){
    pad <- function(x){
      out <- rep(NA_real_, nNodes)
      names(out) <- fullLabels
      out[keep] <- x
      out
    }
    bridgeIn <- pad(bridgeIn)
    bridgeOut <- pad(bridgeOut)
    bridgeStrength <- pad(bridgeStrength)
    bridgeBetweenness <- pad(bridgeBetweenness)
    bridgeCloseness <- pad(bridgeCloseness)
    bridgeEI1 <- pad(bridgeEI1)
    bridgeEI2 <- pad(bridgeEI2)
  }

  # Normalize by the highest possible value of each statistic, given the
  # number of nodes outside each node's community (assuming maximal edge
  # weights of 1; same normalization as networktools::bridge):
  if (normalize){
    nFull <- length(fullCommunities)
    p <- sapply(fullCommunities, function(cc) sum(fullCommunities != cc))
    bridgeIn <- bridgeIn / p
    bridgeOut <- bridgeOut / p
    bridgeStrength <- bridgeStrength / p
    bridgeEI1 <- bridgeEI1 / p
    bridgeEI2 <- bridgeEI2 / (p + (nFull - 1) * (p - p / (nFull - 1)))
    bridgeBetweenness <- bridgeBetweenness / ((nFull - 1) * p)
  }

  if (directed){
    res <- list(
      "Bridge Indegree" = bridgeIn,
      "Bridge Outdegree" = bridgeOut,
      "Bridge Strength" = bridgeStrength,
      "Bridge Betweenness" = bridgeBetweenness,
      "Bridge Closeness" = bridgeCloseness,
      "Bridge Expected Influence (1-step)" = bridgeEI1,
      "Bridge Expected Influence (2-step)" = bridgeEI2,
      communities = fullCommunities)
  } else {
    res <- list(
      "Bridge Strength" = bridgeStrength,
      "Bridge Betweenness" = bridgeBetweenness,
      "Bridge Closeness" = bridgeCloseness,
      "Bridge Expected Influence (1-step)" = bridgeEI1,
      "Bridge Expected Influence (2-step)" = bridgeEI2,
      communities = fullCommunities)
  }

  class(res) <- "bridgeCentrality"
  return(res)
}

print.bridgeCentrality <- function(x, ...){
  df <- as.data.frame(unclass(x)[names(x) != "communities"], check.names = FALSE)
  df <- cbind(community = x$communities, df)
  print(df, ...)
  invisible(x)
}
