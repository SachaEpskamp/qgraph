clusteringTable <- function(..., labels, signed = FALSE, relative = TRUE)
{
  Wmats <- getWmat(list(...))
  
  # Check symmetric and remove:
  sym <- sapply(Wmats,isSymmetric)
  Wmats <- Wmats[sym]
  
  # If no graphs: stop:
  if (length(Wmats)==0) stop("No symmetrical graphs")
  
  ClustAuto <- lapply(Wmats, clustcoef_auto)
  names(ClustAuto) <- names(Wmats)

  # Removed signed, add method and labels to tables:
  for (i in seq_along(ClustAuto))
  {
    if (any(grepl("signed_",names(ClustAuto[[i]]))))
    {
      ClustAuto[[i]] <- ClustAuto[[i]][,grepl("signed_",names(ClustAuto[[i]])) == signed]
      names(ClustAuto[[i]]) <- gsub("signed_","",names(ClustAuto[[i]])) 
    }
    
    names(ClustAuto[[i]]) <- gsub("clust","",names(ClustAuto[[i]])) 
    
    # Relativate:
    if (relative)
    {
      for (j in seq_len(ncol(ClustAuto[[i]])))
      {
        ClustAuto[[i]][j] <- ClustAuto[[i]][j] / max(abs(ClustAuto[[i]][j]), na.rm = TRUE)
      } 
    }
    
    ClustAuto[[i]][['graph']] <- names(ClustAuto)[i]
    
    if (!missing(labels)) 
    {
      ClustAuto[[i]][['node']] <- labels
    } else if(!is.null(colnames(Wmats[[i]])))
    {
      ClustAuto[[i]][['node']] <- colnames(Wmats[[i]])
    } else  ClustAuto[[i]][['node']] <- paste("node",seq_len(nrow(ClustAuto[[i]])))
    
    
  }
  
  ## WIDE FORMAT TABLE:
  Wide <- rbind.fill(ClustAuto)
  
  # LONG FORMAT:
  Long <- melt(Wide, variable.name = "measure", id.var = c("graph","node"))
  
  return(Long)  
}