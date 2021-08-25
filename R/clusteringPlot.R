clusteringPlot <- function(..., scale = c("raw0","raw","z-scores", "relative"), labels, include , signed = FALSE, theme_bw = TRUE, print = TRUE,
                           verbose = TRUE, standardized, relative,
                           orderBy = "default", # Can also be one of the measures
                           decreasing = FALSE)
{
  scale <- match.arg(scale)
  
  if (!missing(standardized)){
    warning("'standardized' argument is deprecated and will be removed.")
  } else {
    standardized <- scale == "z-scores"
  }
  
  if (!missing(relative)){
    warning("'relative' argument is deprecated and will be removed.")    
  } else {
    relative <- scale == "relative"
  }
  
  if (scale == "z-scores"){
    message("Note: z-scores are shown on x-axis rather than raw centrality indices.")
  }
  if (scale == "relative"){
    message("Note: relative centrality indices are shown on x-axis rather than raw centrality indices.")
  }
  
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
    Long$measure <- factor(Long$measure,levels = include)
  }
  
  # Ordereing by node name to make nice paths:
  # Long <- Long[gtools::mixedorder(Long$node),] 
  # Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  if (orderBy == "default"){
    nodeLevels <- unique(gtools::mixedsort(as.character(Long$node), decreasing = decreasing))
  } else {
    nodeLevels <- names(sort(tapply(Long$value[Long$measure == orderBy],Long$node[Long$measure == orderBy],mean), decreasing=decreasing))
  }
  Long$node <- factor(as.character(Long$node), levels = nodeLevels)
  Long <- Long[gtools::mixedorder(Long$node),]
  
  
  
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
  
  if (scale == "raw0"){
    g <-g + xlim(0,NA)
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