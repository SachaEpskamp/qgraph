clusteringPlot <- function(..., labels,standardized=TRUE,  relative = FALSE, include , signed = FALSE, theme_bw = TRUE, print = TRUE)
{
  # Some dummies to get rid of NOTES:
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL

  
  Long <- clusteringTable(..., labels=labels, standardized=standardized, relative=relative, signed=signed)
  Long$value[!is.finite(Long$value)] <- 0
  
  # If not missing, include only include vars:
  if (!missing(include))
  {
    Long <- subset(Long, measure %in% include)
  }
  
  # Ordereing by node name to make nice paths:
  Long <- Long[gtools::mixedorder(Long$node),] 
  Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  
  
  
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
  
  
  if (theme_bw){
    g <- g + theme_bw()
  }
  
  
  if (print){
    print(g)
    invisible(g)
  } else {
    return(g)
  }
}



# clusteringPlot <- function(..., labels, signed = FALSE, relative = TRUE)
# {
#   Long <- clusteringTable(..., labels=labels, signed=signed, relative=relative)
#   
#   # Ordereing by node name to make nice paths:
#   Long <- Long[order(Long$node),] 
#   # PLOT:
#   if (length(unique(Long$graph)) > 1)
#   {
#     g <- ggplot(Long, aes(x = value, y = node, group = graph, colour = graph)) + geom_path() + 
#       facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point() 
#   } else {
#     g <- ggplot(Long, aes(x = value, y = node, group = graph)) + geom_path() + 
#       facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point()
#   }
#   
#   return(g)  
# }