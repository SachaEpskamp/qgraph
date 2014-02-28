

# Function averages the layout of multiple graphs:
averageWmat <- function(...)
{
  dotList <- list(...)
  
  # Get W mats:
  Wmats <- lapply(dotList,getWmat)
  
  # Replace list with averaged Wmats, and rescale:
  for (i in seq_along(Wmats))
  {

    if (is.list(Wmats[[i]]))
    {
      Wmats[[i]] <- do.call(averageWmat,Wmats[[i]])
    }
    Wmats[[i]] <- abs(Wmats[[i]]/max(Wmats[[i]]))
  }
  
  if (!(length(unique(sapply(Wmats,nrow))) == 1 | length(unique(sapply(Wmats,ncol))) == 1 )) stop("Graphs of different dimensions")
  avgWmat <- Reduce('+',Wmats)/length(Wmats)
  return(avgWmat)
  
}

averageLayout <- function(..., layout = "spring", layout.par = list())
{
  avgWmat <- averageWmat(...)
  
  Q <- qgraph(avgWmat, DoNotPlot = TRUE, layout = layout, layout.par = layout.par)
  
  return(Q$layout)
}