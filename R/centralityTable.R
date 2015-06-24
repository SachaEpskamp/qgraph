centralityTable <- function(..., labels, standardized=TRUE, relative = FALSE)
{
  
  Wmats <- getWmat(list(...))


  # Fix names:
  names(Wmats) <- fixnames(Wmats,"graph ")
  
  CentAuto <- lapply(Wmats, centrality_auto)
  
  # Fix tables:
  for (g in seq_along(CentAuto))
  {
    if (!is(CentAuto[[g]],"centrality_auto"))  
    { 
      # Set type graph and labels:
      names(CentAuto[[g]]) <- fixnames(CentAuto[[g]],"type ")
      for (t in seq_along(CentAuto[[g]]))
      {
        # Set labels:
        if (!missing(labels)) 
        {
          CentAuto[[g]][[t]][['node.centrality']][['node']] <- labels
        } else if(!is.null(rownames(CentAuto[[g]][[t]][['node.centrality']])))
        {
          CentAuto[[g]][[t]][['node.centrality']][['node']] <- rownames(CentAuto[[g]][[t]][['node.centrality']])
        } else CentAuto[[g]][[t]][['node.centrality']][['node']] <- paste("Node",seq_len(nrow(CentAuto[[g]][[t]][['node.centrality']])))
        
        CentAuto[[g]][[t]]$node.centrality$graph <- names(CentAuto)[g]
        CentAuto[[g]][[t]]$node.centrality$type <- names(CentAuto[[g]])[t]
      } 
    }
    else 
    {
      # Set graph:
      CentAuto[[g]]$node.centrality$graph <- names(CentAuto)[g]
      
      # Set labels:
      if (!missing(labels)) 
      {
        CentAuto[[g]][['node.centrality']][['node']] <- labels
      } else if(!is.null(rownames(CentAuto[[g]][['node.centrality']])))
      {
        CentAuto[[g]][['node.centrality']][['node']] <- rownames(CentAuto[[g]][['node.centrality']])
      } else CentAuto[[g]][['node.centrality']][['node']] <- paste("Node",seq_len(nrow(CentAuto[[g]][['node.centrality']])))
    }
  }
  
  # If lists, fix:
  isList <- sapply(CentAuto,function(x)!"centrality_auto"%in%class(x))
  if (any(isList))
  {
    for (l in which(isList))
    {
      CentAuto <- c(CentAuto,CentAuto[[l]])
    }
    CentAuto <- CentAuto[-which(isList)]
  }
  
  
  # Add method and labels to tables:
  for (i in seq_along(CentAuto))
  {
    # Relativate or standardize:
    if (relative | standardized )
    {
      if (relative & standardized)
      {
        warning("Using 'relative' and 'standardized' together is not recommended")
      }
      for (j in which(sapply(CentAuto[[i]][['node.centrality']],mode)=="numeric"))
      {
        if (standardized) {
          # Standardize:
          CentAuto[[i]][['node.centrality']][,j]  <- scale2(CentAuto[[i]][['node.centrality']][,j])
        }
        
        if (relative)
        {
          mx <- max(abs(CentAuto[[i]][['node.centrality']][,j]), na.rm = TRUE)
          if (mx != 0)
          {
            CentAuto[[i]][['node.centrality']][,j] <- CentAuto[[i]][['node.centrality']][,j] /  mx
          }  
        }
        
        # Remove attributes:
        attributes(CentAuto[[i]][['node.centrality']][,j]) <- NULL
        
      } 
    }
  
  }
  
  ## WIDE FORMAT TABLE:
  WideCent <- rbind.fill(lapply(CentAuto,'[[','node.centrality'))
  if (is.null(WideCent$type)) WideCent$type <- NA
  
  # LONG FORMAT:
  LongCent <- reshape2::melt(WideCent, variable.name = "measure", id.var = c("graph","type", "node"))
  
  if (any(is.nan(LongCent$value))){
    warning("NaN detected in centrality measures. Try relative = FALSE")
  }
  
  return(LongCent)  
}