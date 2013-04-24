## Merges graph2 into graph1 at one node only.

qgraph.rack <- function(
  graph1, # Original graph
  graph2, # Graph to include in original graph
  link = c(1,1), # integer vector of two indicating which node links the graphs, first element is node number in node 1 and second element the node number in node 2
  scale = 0.5, # Scale of graph 2 relative to graph 1
  rotation = 0, # rotation of graph 2, in radian
  plot = TRUE # Plot the results?
  )
{

  # Numbers of nodes:
  n1 <- graph1$graphAttributes$Graph$nNodes
  n2 <- graph2$graphAttributes$Graph$nNodes
  
  # Number of edges:
  e1 <- length(graph1$Edgelist$from)
  e2 <- length(graph2$Edgelist$from)
  
  # New IDs in graph 2:
  oldID <- 1:n2
  newNodes <- oldID[oldID != link[2]]
  newID <- integer(n2)
  newID[link[2]] <- link[1]
  newID[-link[2]] <- n1 + seq_len(n2-1)
  
  ### Update graph 2 and add to 1 ###
  # Edgelist:
  graph2$Edgelist$from <- newID[graph2$Edgelist$from]
  graph1$Edgelist$from <- c(graph1$Edgelist$from, graph2$Edgelist$from)
  
  graph2$Edgelist$to <- newID[graph2$Edgelist$to]
  graph1$Edgelist$to <- c(graph1$Edgelist$to, graph2$Edgelist$to)
  
  graph1$Edgelist$weight <- c(graph1$Edgelist$weight, graph2$Edgelist$weight)
  
  graph1$Edgelist$directed <- c(graph1$Edgelist$directed, graph2$Edgelist$directed)
  
  graph1$Edgelist$bidirectional <- c(graph1$Edgelist$bidirectional, graph2$Edgelist$bidirectional)
  
  # Arguments
  # Nodes:
  for (a in unique(c(names(graph1$Arguments), names(graph2$Arguments))))
  {
    if (!is.null(graph1$Arguments[[a]]) & !is.null(graph2$Arguments[[a]]))
    {
      # Length of nodes:
      if (length(graph1$Arguments[[a]]) == n1 & length(graph2$Arguments[[a]]) == n2)
      {
        graph1$Arguments[[a]] <- c(graph1$Arguments[[a]], graph2$Arguments[[a]][newNodes] )
      }
      
      # Length of edges:
      if (length(graph1$Arguments[[a]]) == e1 & length(graph2$Arguments[[a]]) == e2)
      {
        graph1$Arguments[[a]] <- c(graph1$Arguments[[a]], graph2$Arguments[[a]])
      }
    }
  }
  

  # graphAttributes:
  # Nodes:
  for (a in seq_along(graph1$graphAttributes$Nodes))
  {
    if (length(graph1$graphAttributes$Nodes[[a]]) == n1 & length(graph2$graphAttributes$Nodes[[a]]) == n2)
    {
      graph1$graphAttributes$Nodes[[a]] <- c(graph1$graphAttributes$Nodes[[a]], graph2$graphAttributes$Nodes[[a]][newNodes] )
    }
  }
  
  # Edges:
  for (a in seq_along(graph1$graphAttributes$Edges))
  {
    if (length(graph1$graphAttributes$Edges[[a]]) == e1 & length(graph2$graphAttributes$Edges[[a]]) == e2)
    {
      graph1$graphAttributes$Edges[[a]] <- c(graph1$graphAttributes$Edges[[a]], graph2$graphAttributes$Edges[[a]])
    }
  }
  
  # Knots:
  graph2$graphAttributes$Knots$knots[graph2$graphAttributes$Knots$knots > 0] <- graph2$graphAttributes$Knots$knots[graph2$graphAttributes$Knots$knots > 0] + max(graph1$graphAttributes$Knots$knots)
  graph1$graphAttributes$Knots$knots <- c(graph1$graphAttributes$Knots$knots, graph2$graphAttributes$Knots$knots)
  
  # Graph:
  graph1$graphAttributes$Graph$nNodes <- n1 + n2 - 1
  graph1$graphAttributes$Graph$edgesort <- sort(abs(graph1$Edgelist$weight),index.return=TRUE)$ix
  
  for (g in seq_along(graph2$graphAttributes$Graph$groups))
  {
    graph2$graphAttributes$Graph$groups[[g]] <- newID[graph2$graphAttributes$Graph$groups[[g]]]
  }
  graph1$graphAttributes$Graph$groups <- c(graph1$graphAttributes$Graph$groups, graph2$graphAttributes$Graph$groups)
  graph1$graphAttributes$Graph$color <- c(graph1$graphAttributes$Graph$color, graph2$graphAttributes$Graph$color)
 
  
  ### ROTATE AND SCALE LAYOUT ###
  L1 <- graph1$layout
  L2 <- graph2$layout
  # Center layout 2:
  L2[,1] <- L2[,1] - L2[link[2],1]
  L2[,2] <- L2[,2] - L2[link[2],2]
  
  # Scale:
  L2 <- L2 * scale
  
  # Rotate:
  RotMat <- function(d) matrix(c(cos(-d),sin(-d),-sin(-d),cos(-d)),2,2)
  L2 <- t(RotMat( rotation ) %*% t(L2))
  
  # Center to L1:
  L2[,1] <- L2[,1] + L1[link[1],1]
  L2[,2] <- L2[,2] + L1[link[1],2]
  
  # Combine:
  graph1$layout <- rbind(L1, L2[newNodes,])
  
  
  if (plot)
  {
    plot(graph1)
    invisible(graph1)
  } else 
  {
    return(graph1)
  }
}