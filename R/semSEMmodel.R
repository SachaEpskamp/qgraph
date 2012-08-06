pathDiagram.sem <- function(object,...) 
{
  invisible(pathDiagram(qgraphSEM(object),...))
}


### SINGLE GROUP MODEL ###
qgraphSEM.sem <- function(res)
{
  
  # Check if object is of class "sem":
  if (!any(class(res)%in%c("sem","semmod"))) stop("Input must be a 'sem' object")
  
  # Define RAM:
  RAM <- data.frame(
    label = rownames(res$ram), 
    lhs = res$ram[,3],
    edge = "",
    rhs = res$ram[,2],
    est = res$ram[,5],
    std = standardizedCoefficients(res)[,2],
    group = 1,
    fixed = res$ram[,4]==0,
    stringsAsFactors=FALSE)
  
  # Extract parameter estimates:
  RAM$est[res$ram[,4]!=0] <- res$coef[res$ram[,4]]
  
  # Fix labels:
  for (i in unique(res$ram[,4][res$ram[,4]!=0]))
  {
    if (any(RAM$label[res$ram[,4]==i]=="") & any(RAM$label[res$ram[,4]==i]!="")) 
    {
      RAM$label[res$ram[,4]==i & RAM$label==""] <- RAM$label[res$ram[,4]==i & RAM$label!=""] 
    }
  }
  
  # Name variables:
  RAM$lhs <- res$var.names[RAM$lhs]
  RAM$rhs <- res$var.names[RAM$rhs]
  
  # Variable dataframe: 
  Vars <- data.frame(
    name = res$var.names,
    manifest = res$var.names %in% colnames(res$S),
    stringsAsFactors=FALSE)
  
  # Define operators:
  RAM$edge[res$ram[,1]==2] <- "<->"
  RAM$edge[res$ram[,1]==1] <- "->"
#   RAM$op[res$ram[,1]==1 & !Vars$manifest[match(RAM$lhs,Vars$name)] & Vars$manifest[match(RAM$rhs,Vars$name)]] <- "->"
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- TRUE
  semModel@Original <- list(res)
  
  return(semModel)
}




### MUTLI GROUP MODEL ###
qgraphSEM.msem <- function(object)
{
  
  nGroup <- length(object$ram)
  GroupNames <- object$groups
  
  RAMS <- list()
  stdRes <- standcoefmsem(object)
  
  for (g in 1:nGroup)
  {
    # Define RAM:
    RAM <- data.frame(
      label = rownames(object$ram[[g]]), 
      lhs = object$ram[[g]][,3],
      edge = "",
      rhs = object$ram[[g]][,2],
      est = object$ram[[g]][,5],
      std = stdRes[[g]][,2],
      group = GroupNames[g],
      fixed = object$ram[[g]][,4]==0,
      stringsAsFactors=FALSE)
    
    # Extract parameter estimates:
    RAM$est[object$ram[[g]][,4]!=0] <- res$coef[object$ram[[g]][,4]]
    
    # Fix labels:
    for (i in unique(object$ram[[g]][,4][object$ram[[g]][,4]!=0]))
    {
      if (any(RAM$label[object$ram[[g]][,4]==i]=="") & any(RAM$label[object$ram[[g]][,4]==i]!="")) 
      {
        RAM$label[object$ram[[g]][,4]==i & RAM$label==""] <- RAM$label[object$ram[[g]][,4]==i & RAM$label!=""] 
      }
    }
    
    # Name variables:
    RAM$lhs <- res$var.names[[g]][RAM$lhs]
    RAM$rhs <- res$var.names[[g]][RAM$rhs]
    
    
    RAMS[[g]] <- RAM
  }
  
  RAM <- do.call("rbind",RAMS)
  
  # Variable dataframe: 
  Vars <- data.frame(
    name = unique(unlist(object$var.names)),
    manifest = unique(unlist(object$var.names)) %in% unique(c(sapply(res$S,colnames))),
    stringsAsFactors=FALSE)
  
  
  #   RAM$op[res$ram[,1]==1 & !Vars$manifest[match(RAM$lhs,Vars$name)] & Vars$manifest[match(RAM$rhs,Vars$name)]] <- "->"
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- TRUE
  semModel@Original <- list(res)
  
  return(semModel)
}
          