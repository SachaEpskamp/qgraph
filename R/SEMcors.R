
SEMcors <- function(object,vertical=FALSE,...){
  if (!"qgraph.semModel"%in%class(object)) object <- qgraphSEM(object) 
  
  if (!object@Computed) stop("SEM model has not been evaluated; there are no implied covariances")
  
  Ng <- length(object@ObsCovs)
  Groups <- unique(object@RAM$group)
  
  l <- matrix(1:(Ng*2),2,)
  if (vertical) layout(t(l)) else layout(l)
  
  for (g in 1:Ng)
  {
    qgraph(round(cov2cor(object@ObsCovs[[g]]),5),maximum=1,...)
    title(paste("Group",Groups[g],"(observed)"),line=3)
    
    qgraph(round(cov2cor(object@ImpCovs[[g]]),5),maximum=1,...)
    title(paste("Group",Groups[g],"(implied)"),line=3)
  }
}