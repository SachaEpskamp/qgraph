### Path diagrams ###

setMethod("pathDiagram.S4",signature("lavaan"),function(object,...){
  invisible(pathDiagram(qgraphSEM(object),...))
})



## EXTRACT MODEL ###
setMethod("qgraphSEM.S4",signature("lavaan"),function(object){
  
  
  if (class(object)!="lavaan") stop("Input must me a 'lavaan' object")
  
  
  # Extract parameter estimates:
  pars <- parameterEstimates(object,standardized=TRUE)
  list <- inspect(object,"list")
  
  # Remove mean structure (TEMP SOLUTION)
  # meanstructure <- pars$op=="~1"
  # pars <- pars[!meanstructure,]
  
  # Extract variable and factor names:
  # varNames <- fit@Model@dimNames$lambda[[1]]
  # factNames <- fit@Model@dimNames$lambda[[2]]
  Lambda <- inspect(object,"coef")$lambda
  varNames <- rownames(Lambda)
  factNames <- colnames(Lambda)
  rm(Lambda)
  
  factNames <- factNames[!factNames%in%varNames]
  
  # Extract number of variables and factors
  n <- length(varNames)
  k <- length(factNames)
  
  # Extract parameter names:
  if (is.null(pars$label)) pars$label <- rep("",nrow(pars))
  
  semModel <- new("qgraph.semModel")
  
  if (is.null(pars$group)) pars$group <- ""
  
  # Create edges dataframe
  semModel@RAM <- data.frame(
    label = pars$label,
    lhs = ifelse(pars$op=="~"|pars$op=="~1",pars$rhs,pars$lhs),
    edge = pars$op,
    rhs = ifelse(pars$op=="~"|pars$op=="~1",pars$lhs,pars$rhs),
    est = pars$est,
    std = pars$std.all,
    group = pars$group,
    fixed = list$free==0,
    par = list$free,
    stringsAsFactors=FALSE)

  semModel@RAM$edge[semModel@RAM$edge=="~~"] <- "<->"  
  semModel@RAM$edge[semModel@RAM$edge=="~"] <- "->"
  semModel@RAM$edge[semModel@RAM$edge=="=~"] <- "->"
  semModel@RAM$edge[semModel@RAM$edge=="~1"] <- "int"
  
  semModel@Vars <- data.frame(
    name = c(varNames,factNames),
    manifest = c(varNames,factNames)%in%varNames,
    stringsAsFactors=FALSE)
  
  semModel@ObsCovs <- object@SampleStats@cov
  names(semModel@ObsCovs) <- object@Data@group.label
  for (i in 1:length(semModel@ObsCovs))
  {
    rownames(semModel@ObsCovs[[i]]) <- colnames(semModel@ObsCovs[[i]]) <- object@Data@ov.names[[i]]
  }
  
  semModel@ImpCovs <- object@Fit@Sigma.hat
  names(semModel@ImpCovs) <- object@Data@group.label
  for (i in 1:length(semModel@ImpCovs))
  {
    rownames(semModel@ImpCovs[[i]]) <- colnames(semModel@ImpCovs[[i]]) <- object@Data@ov.names[[i]]
  }
  
  semModel@Computed <- TRUE
  
  semModel@Original <- list(object)
  
  return(semModel)
})



