centralityPlot <- function(..., labels, relative = TRUE)
{
  Long <- centralityTable(..., labels=labels, relative=relative)
  
  # Ordereing by node name to make nice paths:
  Long <- Long[order(Long$node),] 
  # PLOT:
  if (length(unique(Long$graph)) > 1)
  {
    g <- ggplot(Long, aes(x = value, y = node, group = graph, colour = graph)) + geom_path() + 
      facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point() 
  } else {
    g <- ggplot(Long, aes(x = value, y = node, group = graph)) + geom_path() + 
      facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point()
  }

  return(g)  
}


