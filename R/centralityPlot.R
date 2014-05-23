centralityPlot <- function(..., labels, relative = TRUE, include)
{
  # Some dummies to get rid of NOTES:
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL
  
  Long <- centralityTable(..., labels=labels, relative=relative)

  # If not missing, include only include vars:
  if (!missing(include))
  {
    Long <- subset(Long, measure %in% include)
  }
  
  # Ordereing by node name to make nice paths:
  Long <- Long[order(Long$node),] 
  # PLOT:
  if (length(unique(Long$type)) > 1)
  {
    g <- ggplot(Long, aes(x = value, y = node, group = type, colour = type))
  } else {
    g <- ggplot(Long, aes(x = value, y = node, group = type)) 
  }
  
  g <- g +  geom_path() +  xlab("") + ylab("") + geom_point() 
  
  if (length(unique(Long$graph)) > 1)
  {
    g <- g + facet_grid(graph ~ measure, scales = "free") 
  } else 
  {
    g <- g + facet_grid( ~ measure, scales = "free") 
  }

  return(g)  
}


