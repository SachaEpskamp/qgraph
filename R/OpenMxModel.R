### Path diagrams ###

pathDiagram_MxRAMModel <- function(object,...){
  invisible(pathDiagram(qgraphSEM(object),...))
}
          
pathDiagram_MxModel <- function(object,...){
  invisible(pathDiagram(qgraphSEM(object),...))
}

### EXTRACT MODEL ###
          
### SINGLE GROUP ###
qgraphSEM_MxRAMModel <- function(object){
  
  # Extract names:
  varNames <- object@manifestVars
  factNames <- object@latentVars
  
  # Extract directed paths:
  Dirpaths <- which(t(object@matrices$A@free | object@matrices$A@values!=0),arr.ind=TRUE)
  DirpathsFixed <- !t(object@matrices$A@free)[Dirpaths]
  DirpathsValues <- t(object@matrices$A@values)[Dirpaths]
  DirpathsLabels <- t(object@matrices$A@labels)[Dirpaths]
  
  # Extract symmetric paths:
  Sympaths <- which(t(object@matrices$S@free | object@matrices$S@values!=0) & upper.tri(object@matrices$S@values,diag=TRUE),arr.ind=TRUE)
  SympathsFixed <- !t(object@matrices$S@free)[Sympaths]
  SympathsValues <- t(object@matrices$S@values)[Sympaths]
  SympathsLabels <- t(object@matrices$A@labels)[Sympaths]
  
  if (!is.null(object@matrices$M))
  {
    # Extract intercepts:
    Means <- which(object@matrices$M@free | object@matrices$M@values!=0)
    MeansFixed <- !object@matrices$M@free[Means]
    MeansValues <- object@matrices$M@values[Means]
    MeansLabels <- object@matrices$M@labels[Means]
  } else
  {
    Means <- numeric(0)
    MeansFixed <- logical(0)
    MeansValues <- numeric(0)
    MeansLabels <- character(0)
  }
  
  ## Standardized
  if (!length(object@output)==0)
  {
    # Function by Ryne Estabrook (http://openmx.psyc.virginia.edu/thread/718)
    standObj <- standardizeRAM(object,"model")
    
    # Extract directed paths:
    DirpathsValuesStd <- t(standObj@matrices$A@values)[Dirpaths]
    
      # Extract symmetric paths:
    SympathsValuesStd <- t(standObj@matrices$S@values)[Sympaths]
      
      # Extract means:
    
    if (!is.null(standObj@matrices$M))
    {
      MeansValuesStd <- standObj@matrices$S@values[Means]
    } else
    {
      MeansValuesStd <- numeric(0)
    }
  } else 
  {
    DirpathsValuesStd <- rep(NA,nrow(Dirpaths)) 
    SympathsValuesStd <- rep(NA,nrow(Sympaths))
    MeansValuesStd <- rep(NA,length(Means))
  }
  
  # Vars dataframe:
  Vars <- data.frame(
    name = c(varNames,factNames),
    manifest = c(varNames,factNames)%in%varNames,
    stringsAsFactors=FALSE)
  
  # Define RAM:
  RAM <- data.frame(
    label = c(DirpathsLabels,SympathsLabels,MeansLabels), 
    lhs = c(Vars$name[c(Dirpaths[,1],Sympaths[,1])],rep("",length(Means))),
    edge = c(rep("->",nrow(Dirpaths)),rep("<->",nrow(Sympaths)),rep("int",length(Means))),
    rhs = Vars$name[c(Dirpaths[,2],Sympaths[,2],Means)],
    est = c(DirpathsValues,SympathsValues,MeansValues),
    std = c(DirpathsValuesStd,SympathsValuesStd,MeansValuesStd),
    group = object@name,
    fixed = c(DirpathsFixed,SympathsFixed,MeansFixed),
    stringsAsFactors=FALSE)
#   
#   # Add standardized:
#   for (i in 1:nrow(standRAM))
#   {
#     if (standRAM$matrix[i] == "A")
#     {
#       RAM$std[RAM$lhs == standRAM$col[i] & RAM$rhs == standRAM$row[i] & RAM$edge == "->"] <- standRAM[["Std. Estimate"]][i]
#     }
#     if (standRAM$matrix[i] == "S")
#     {
#       RAM$std[RAM$lhs == standRAM$col[i] & RAM$rhs == standRAM$row[i] & RAM$edge == "<->"] <- standRAM[["Std. Estimate"]][i]
#     }
#   }
  
  RAM$label[is.na(RAM$label)] <- ""
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- !length(object@output)==0
  semModel@Original <- list(object)
  
  return(semModel)
}


qgraphSEM_MxModel <- function(object){

  if (any(!"MxRAMModel"%in%sapply(object@submodels,class))) stop("Model or all submodels must be of class 'MxRAMModel'")
  for (i in 1:length(object@submodels)) object@submodels[[i]]@output <- list(TRUE)
  S4objects <- lapply(object@submodels,qgraphSEM.S4)
  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- do.call("rbind",lapply(S4objects,slot,"RAM"))
  semModel@Vars <- S4objects[[1]]@Vars
  semModel@Computed <- !length(object@output)==0
  semModel@Original <- list(object)
  
  return(semModel)
}