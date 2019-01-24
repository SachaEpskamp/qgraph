centralityPlot <- function(..., labels, scale = c("z-scores", "raw", "raw0","relative"), 
                           include = c("Degree","Strength","OutDegree","InDegree","OutStrength","InStrength"), 
                           theme_bw = TRUE, print = TRUE,
                           verbose = TRUE, standardized, relative, weighted = TRUE, signed = TRUE,
                           orderBy = "default", # Can also be one of the measures
                           decreasing = FALSE
)
{
  if (any(include=="all") | any(include=="All")){
    include <- c("Degree","Strength","OutDegree","InDegree","OutStrength","InStrength","Closeness","Betweenness",
                 "ExpectedInfluence","OutExpectedInfluence","InExpectedInfluence")
  }
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
    if (verbose) message("Note: z-scores are shown on x-axis rather than raw centrality indices.")
  }
  if (scale == "relative"){
    if (verbose) message("Note: relative centrality indices are shown on x-axis rather than raw centrality indices.")
  }
  
  # Some dummies to get rid of NOTES:
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL
  
  ## I realize I should have used a more appropriate programmatic way of doing this. My
  ## programming is bad and I fo feel bad.
  
  Long <- centralityTable(..., standardized=standardized, labels=labels, relative=relative, weighted = weighted, signed = signed)

  # If not missing, include only include vars:
  # if (!missing(include))
  # {
  Long <- subset(Long, measure %in% include)
  # }
  
  # Re-order:
  Long$measure <- factor(Long$measure,levels = include)
  
  # Ordereing by node name to make nice paths:
  if (orderBy == "default"){
    nodeLevels <- unique(gtools::mixedsort(as.character(Long$node), decreasing = decreasing))
  } else {
    nodeLevels <- names(sort(tapply(Long$value[Long$measure == orderBy],Long$node[Long$measure == orderBy],mean), decreasing=decreasing))
  }
  Long$node <- factor(as.character(Long$node), levels = nodeLevels)
  Long <- Long[gtools::mixedorder(Long$node),]
  # 
  #      Long <- Long[gtools::mixedorder(Long$node),] 
  #      Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  #    } else {
  # 
  #      Long <- Long[gtools::mixedorder(Long[[orderBy]]),] 
  #      Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  #    }
  
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


