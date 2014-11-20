centralityPlot <- function(..., labels, standardized = TRUE, relative = FALSE, include, theme_bw = TRUE, print = TRUE)
{
  # Some dummies to get rid of NOTES:
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL
  
  ## I realize I should have used a more appropriate programmatic way of doing this. My
  ## programming is bad and I fo feel bad.
  
  Long <- centralityTable(..., standardized=standardized, labels=labels, relative=relative)

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


