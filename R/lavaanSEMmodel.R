### Path diagrams ###

setMethod("pathDiagram.S4",signature("lavaan"),function(object,...){
  invisible(pathDiagram(qgraphSEM(object),...))
})



## EXTRACT MODEL ###
setMethod("qgraphSEM.S4",signature("lavaan"),function(object){
  
  
  if (class(object)!="lavaan") stop("Input must me a 'lavaan' object")
  
  
  # Extract parameter estimates:
  pars <- parameterEstimates(object,standardized=TRUE)
  
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
  
  # Create edges dataframe
  semModel@RAM <- data.frame(
    label = pars$label,
    lhs = ifelse(pars$op=="~"|pars$op=="~1",pars$rhs,pars$lhs),
    edge = pars$op,
    rhs = ifelse(pars$op=="~"|pars$op=="~1",pars$lhs,pars$rhs),
    est = pars$est,
    std = pars$std.all,
    group = pars$group,
    fixed = is.na(pars$z),
    stringsAsFactors=FALSE)

  semModel@RAM$edge[semModel@RAM$edge=="~~"] <- "<->"  
  semModel@RAM$edge[semModel@RAM$edge=="~"] <- "->"
  semModel@RAM$edge[semModel@RAM$edge=="=~"] <- "->"
  semModel@RAM$edge[semModel@RAM$edge=="~1"] <- "int"
  
  semModel@Vars <- data.frame(
    name = c(varNames,factNames),
    manifest = c(varNames,factNames)%in%varNames,
    stringsAsFactors=FALSE)
  
  
  semModel@Computed <- TRUE
  
  semModel@Original <- list(object)
  
  return(semModel)
})



