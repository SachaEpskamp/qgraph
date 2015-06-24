clusteringTable <- function(..., labels,standardized=TRUE,  relative = FALSE, signed = FALSE)
{
  Wmats <- getWmat(list(...))
  
  # Check symmetric and remove:
  for (i in rev(seq_along(Wmats)))
  {
    if (is.list(Wmats[[i]]))
    {
      for (j in rev(seq_along(Wmats[[i]])))
      {
        if (!isSymmetric(Wmats[[i]][[j]]))
        {
          Wmats[[i]] <- Wmats[[i]][-j]
          warning("Nonsymmetrical graph removed.")
        }
      }
      if (length(Wmats[[i]]) == 0)
      {
        Wmats <- Wmats[-i]
      }
    } else {
      if (!isSymmetric(Wmats[[i]]))
      {
        Wmats <- Wmats[-i]
        warning("Nonsymmetrical graph removed.")
      }
    }
  }
  
  # If no graphs: stop:
  if (length(Wmats)==0) stop("No symmetrical graphs")

  # Fix names:
  names(Wmats) <- fixnames(Wmats,"graph ")
  
  # Compute clustering:
  ClustAuto <- lapply(Wmats, clustcoef_auto)
#   names(ClustAuto) <- names(Wmats)

  # Fix tables:
  for (g in seq_along(ClustAuto))
  {
    if (!is(ClustAuto[[g]],"clustcoef_auto"))  
    { 
      # Set type graph and labels:
      names(ClustAuto[[g]]) <- fixnames(ClustAuto[[g]],"type ")
      for (t in seq_along(ClustAuto[[g]]))
      {
        # Set labels:
        if (!missing(labels)) 
        {
          ClustAuto[[g]][[t]][['node']] <- labels
        } else if(!is.null(rownames(ClustAuto[[g]][[t]])))
        {
          ClustAuto[[g]][[t]][['node']] <- rownames(ClustAuto[[g]][[t]])
        } else ClustAuto[[g]][[t]][['node']] <- paste("Node",seq_len(nrow(ClustAuto[[g]][[t]])))
        
        ClustAuto[[g]][[t]]$graph <- names(ClustAuto)[g]
        ClustAuto[[g]][[t]]$type <- names(ClustAuto[[g]])[t]
      } 
    }
    else 
    {
      # Set graph:
      ClustAuto[[g]]$graph <- names(ClustAuto)[g]
      
      # Set labels:
      if (!missing(labels)) 
      {
        ClustAuto[[g]][['node']] <- labels
      } else if(!is.null(rownames(ClustAuto[[g]])))
      {
        ClustAuto[[g]][['node']] <- rownames(ClustAuto[[g]])
      } else ClustAuto[[g]][['node']] <- paste("Node",seq_len(nrow(ClustAuto[[g]])))
    }
  }


# If lists, fix:
  isList <- sapply(ClustAuto,function(x)!"clustcoef_auto"%in%class(x))
  if (any(isList))
  {
    for (l in which(isList))
    {
      ClustAuto <- c(ClustAuto,ClustAuto[[l]])
    }
    ClustAuto <- ClustAuto[-which(isList)]
  }

  # Add method and labels to tables and remove signed
  for (i in seq_along(ClustAuto))
  {
    if (any(grepl("signed_",names(ClustAuto[[i]]))))
    {
      ClustAuto[[i]] <- ClustAuto[[i]][,sapply(ClustAuto[[i]],mode)!="numeric"|grepl("signed_",names(ClustAuto[[i]])) == signed]
      names(ClustAuto[[i]]) <- gsub("signed_","",names(ClustAuto[[i]])) 
    }
    
    names(ClustAuto[[i]]) <- gsub("clust","",names(ClustAuto[[i]])) 
    
    # Relativate:
    if (relative | standardized)
    {
      if (relative & standardized)
      {
        warning("Using 'relative' and 'standardized' together is not recommended")
      }
      for (j in which(sapply(ClustAuto[[i]],mode)=="numeric"))
      {
        if (standardized)
        {
          ClustAuto[[i]][,j] <- scale2(ClustAuto[[i]][,j])
        }
        
        
        if (relative)
        {
          mx <-  max(abs(ClustAuto[[i]][,j]), na.rm = TRUE)
          if (mx != 0)
          {
            ClustAuto[[i]][,j] <- ClustAuto[[i]][,j] / mx  
          }
        }
        
        # Remove attributes:
        attributes(ClustAuto[[i]][,j]) <- NULL
        

        
      } 
    }
    
  }
  
  ## WIDE FORMAT TABLE:
  WideCent <- rbind.fill(ClustAuto)
  if (is.null(WideCent$type)) WideCent$type <- NA


  # LONG FORMAT:
  LongCent <- reshape2::melt(WideCent, variable.name = "measure", id.var = c("graph","type", "node"))
  
  return(LongCent)  
  
  
  
#   
#   
#   
#   
# 
# 
#   # Removed signed, add method and labels to tables:
#   for (i in seq_along(ClustAuto))
#   {
# 
#     
#     # Relativate:
#     if (relative)
#     {
#       for (j in seq_len(ncol(ClustAuto[[i]])))
#       {
#         ClustAuto[[i]][j] <- ClustAuto[[i]][j] / max(abs(ClustAuto[[i]][j]), na.rm = TRUE)
#       } 
#     }
#     
#     ClustAuto[[i]][['graph']] <- names(ClustAuto)[i]
#     
#     if (!missing(labels)) 
#     {
#       ClustAuto[[i]][['node']] <- labels
#     } else if(!is.null(colnames(Wmats[[i]])))
#     {
#       ClustAuto[[i]][['node']] <- colnames(Wmats[[i]])
#     } else  ClustAuto[[i]][['node']] <- paste("node",seq_len(nrow(ClustAuto[[i]])))
#     
#     
#   }
#   
#   ## WIDE FORMAT TABLE:
#   Wide <- rbind.fill(ClustAuto)
#   
#   # LONG FORMAT:
#   Long <- melt(Wide, variable.name = "measure", id.var = c("graph","node"))
#   
#   return(Long)  
}