# Clustering random graph:
Cr <- function(x){
  if ("igraph"%in%class(x)) x <- get.adjacency(x)
  
  N=nrow(x)
  p=sum(x/2)/sum(lower.tri(x))
  
  t=(p*(N-1)/N)
  t
}

# Average shortest path length random graph:
APLr <- function(x){
  if ("igraph"%in%class(x)) x <- get.adjacency(x)
  
  N=nrow(x)
  p=sum(x/2)/sum(lower.tri(x))
  
  eulers_constant <- .57721566490153
  l = (log(N)-eulers_constant)/log(p*(N-1)) +.5
  l
}


smallworldIndex <- function(x){
  if ("qgraph"%in%class(x)) x <- as.igraph(x)
  if (!all(E(x)$weight==1)) {
    warning("Edge weights removed")
    E(x)$weight[] <- 1
  }
  
  list(
    transitivity =   igraph::transitivity(x),
    transitivity_random = Cr(x),
    APL = igraph::average.path.length(x),
    APL_random = APLr(x),
    index =  (igraph::transitivity(x) / Cr(x)) / 
      (igraph::average.path.length(x) / APLr(x))
  )
}

