centralityTable <- function(..., labels, relative = TRUE)
{

  Wmats <- getWmat(list(...))
  
  CentAuto <- lapply(Wmats, centrality_auto)
  
  # Add method and labels to tables:
  for (i in seq_along(CentAuto))
  {
    # Relativate:
    if (relative)
    {
      for (j in seq_len(ncol(CentAuto[[i]][['node.centrality']])))
      {
        CentAuto[[i]][['node.centrality']][,j] <- CentAuto[[i]][['node.centrality']][,j] / max(abs(CentAuto[[i]][['node.centrality']][,j]), na.rm = TRUE)
      } 
    }
    
    CentAuto[[i]][['node.centrality']][['graph']] <- names(CentAuto)[i]
    CentAuto[[i]][['edge.centrality']][['graph']] <- names(CentAuto)[i]
    
    if (!missing(labels)) 
    {
      CentAuto[[i]][['node.centrality']][['node']] <- labels
    } else if(!is.null(colnames(Wmats[[i]])))
    {
      CentAuto[[i]][['node.centrality']][['node']] <- colnames(Wmats[[i]])
    } else CentAuto[[i]][['node.centrality']][['node']] <- paste("node",seq_len(nrow(CentAuto[[i]][['node.centrality']])))

  }

  ## WIDE FORMAT TABLE:
  WideCent <- rbind.fill(lapply(CentAuto,'[[','node.centrality'))
  
  # LONG FORMAT:
  LongCent <- melt(WideCent, variable.name = "measure", id.var = c("graph","node"))
  
  return(LongCent)  
}