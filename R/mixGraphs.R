# Mixes two qgraph graphs on pars:
# - layout
# - vsize
# - esize
# - layout
# 

mixGraphs <- function(Graph1, Graph2, mix = 0.5)
{
  # For now, break if edgelists are not identical:
  if (!identical(Graph1$Edgelist[c('from','to','directed','bidirectional')],Graph2$Edgelist[c('from','to','directed','bidirectional')]))
  {
    stop("Graphs must have identical edgelists for mixing")
  }
  
  NewGraph <- Graph2
  
  # Mix weights:
  NewGraph$Edgelist$weight <- (1-mix) * Graph1$Edgelist$weight + mix * Graph2$Edgelist$weight
  
  # Mix Layout:
  NewGraph$layout <- (1-mix) * Graph1$layout + mix * Graph2$layout
  
  # Mix vsize:
  NewGraph$graphAttributes$Nodes$width <- (1-mix) * Graph1$graphAttributes$Nodes$width + mix * Graph2$graphAttributes$Nodes$width
  NewGraph$graphAttributes$Nodes$height <- (1-mix) * Graph1$graphAttributes$Nodes$height + mix * Graph2$graphAttributes$Nodes$height
  
  # Mix edge color:
  NewGraph$graphAttributes$Edges$color <- mapply(FUN=Fade,col=Graph2$graphAttributes$Edges$color, alpha=mix, bg = Graph1$graphAttributes$Edges$color)
  
  # Mix edge width:
  NewGraph$graphAttributes$Edges$width <- (1-mix) * Graph1$graphAttributes$Edges$width + mix * Graph2$graphAttributes$Edges$width
  
  # loopRotation:
  # Overwrite looprotation of Graph1 to negative if that is closer:
  Graph1$graphAttributes$Nodes$loopRotation <- ifelse( abs(Graph1$graphAttributes$Nodes$loopRotation - Graph2$graphAttributes$Nodes$loopRotation) <  abs(Graph1$graphAttributes$Nodes$loopRotation - 2*pi - Graph2$graphAttributes$Nodes$loopRotation), 
                                                       
   ifelse( abs(Graph1$graphAttributes$Nodes$loopRotation - Graph2$graphAttributes$Nodes$loopRotation) <  abs(Graph1$graphAttributes$Nodes$loopRotation + 2*pi - Graph2$graphAttributes$Nodes$loopRotation), Graph1$graphAttributes$Nodes$loopRotation, Graph1$graphAttributes$Nodes$loopRotation + 2*pi), 
    
    Graph1$graphAttributes$Nodes$loopRotation - 2*pi)
  
  NewGraph$graphAttributes$Nodes$loopRotation <- ((1-mix) * Graph1$graphAttributes$Nodes$loopRotation + mix * Graph2$graphAttributes$Nodes$loopRotation)
  
  
  return(NewGraph)
}

smoothLayout <- function(x)
{
  
}

smoothAnimationList <- function(x, smoothing = 5)
{
  newList <- list()
  for (i in seq_len(length(x)-1))
  {
    newList <- c(newList, list(x[[i]]), lapply(seq(0,1,length=smoothing),mixGraphs,Graph1=x[[i]],Graph2=x[[i+1]]))
  }
  newList <- c(newList, list(x[[length(x)]]))
  return(newList)
}