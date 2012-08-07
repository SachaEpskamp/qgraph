pathDiagram.sem <- function(object,...) 
{
  invisible(pathDiagram(qgraphSEM(object),...))
}

pathDiagram.msem <- function(object,...) 
{
  invisible(pathDiagram(qgraphSEM(object),...))
}

pathDiagram.msemObjectiveML <- function(object,...) 
{
  invisible(pathDiagram(qgraphSEM(object),...))
}



### SINGLE GROUP MODEL ###
qgraphSEM.sem <- function(object)
{
  
  # Check if object is of class "sem":
  if (!any(class(object)%in%c("sem","semmod"))) stop("Input must be a 'sem' object")
  
  # Define RAM:
  RAM <- data.frame(
    label = rownames(object$ram), 
    lhs = object$ram[,3],
    edge = "",
    rhs = object$ram[,2],
    est = object$ram[,5],
    std = standardizedCoefficients(object)[,2],
    group = 1,
    fixed = object$ram[,4]==0,
    stringsAsFactors=FALSE)
  
  # Extract parameter estimates:
  RAM$est[object$ram[,4]!=0] <- object$coef[object$ram[,4]]
  
  # Fix labels:
  for (i in unique(object$ram[,4][object$ram[,4]!=0]))
  {
    if (any(RAM$label[object$ram[,4]==i]=="") & any(RAM$label[object$ram[,4]==i]!="")) 
    {
      RAM$label[object$ram[,4]==i & RAM$label==""] <- RAM$label[object$ram[,4]==i & RAM$label!=""] 
    }
  }
  
  # Name variables:
  RAM$lhs <- object$var.names[RAM$lhs]
  RAM$rhs <- object$var.names[RAM$rhs]
  
  # Variable dataframe: 
  Vars <- data.frame(
    name = object$var.names,
    manifest = object$var.names %in% colnames(object$S),
    stringsAsFactors=FALSE)
  
  # Define operators:
  RAM$edge[object$ram[,1]==2] <- "<->"
  RAM$edge[object$ram[,1]==1] <- "->"
#   RAM$op[object$ram[,1]==1 & !Vars$manifest[match(RAM$lhs,Vars$name)] & Vars$manifest[match(RAM$rhs,Vars$name)]] <- "->"
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- TRUE
  semModel@Original <- list(object)
  
  return(semModel)
}




### MUTLI GROUP MODEL ###
qgraphSEM.msem <- qgraphSEM.msemObjectiveML <- function(object)
{
  
  nGroup <- length(object$ram)
  GroupNames <- object$groups
  
  RAMS <- list()
  stdobject <- standcoefmsem(object)
  
  for (g in 1:nGroup)
  {
    # Define RAM:
    RAM <- data.frame(
      label = rownames(object$ram[[g]]), 
      lhs = object$ram[[g]][,3],
      edge = "",
      rhs = object$ram[[g]][,2],
      est = object$ram[[g]][,5],
      std = stdobject[[g]][,2],
      group = GroupNames[g],
      fixed = object$ram[[g]][,4]==0,
      stringsAsFactors=FALSE)
    
    # Extract parameter estimates:
    RAM$est[object$ram[[g]][,4]!=0] <- object$coef[object$ram[[g]][,4]]
    
    # Fix labels:
    for (i in unique(object$ram[[g]][,4][object$ram[[g]][,4]!=0]))
    {
      if (any(RAM$label[object$ram[[g]][,4]==i]=="") & any(RAM$label[object$ram[[g]][,4]==i]!="")) 
      {
        RAM$label[object$ram[[g]][,4]==i & RAM$label==""] <- RAM$label[object$ram[[g]][,4]==i & RAM$label!=""] 
      }
    }
    
    # Name variables:
    RAM$lhs <- object$var.names[[g]][RAM$lhs]
    RAM$rhs <- object$var.names[[g]][RAM$rhs]
    
    
    RAMS[[g]] <- RAM
  }
  
  RAM <- do.call("rbind",RAMS)
  
  # Variable dataframe: 
  Vars <- data.frame(
    name = unique(unlist(object$var.names)),
    manifest = unique(unlist(object$var.names)) %in% unique(c(sapply(object$S,colnames))),
    stringsAsFactors=FALSE)
  
  
  #   RAM$op[object$ram[,1]==1 & !Vars$manifest[match(RAM$lhs,Vars$name)] & Vars$manifest[match(RAM$rhs,Vars$name)]] <- "->"
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- TRUE
  semModel@Original <- list(object)
  
  return(semModel)
}
          