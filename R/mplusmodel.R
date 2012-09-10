
# object <- readModels(file.choose())
pathDiagram.mplus.model <- function(object,...) 
{
  invisible(pathDiagram(qgraphSEM(object),...))
}

qgraphSEM.mplus.model <- function(object)
{
  parsUS <- object$parameters$unstandardized
  if (is.null(parsUS$Group)) parsUS$Group <- ""
  
  # Define RAM:
  RAM <- data.frame(
    label = "", 
    lhs = "",
    edge = "",
    rhs = parsUS$param,
    est = parsUS$est,
    std = NA,
    group = parsUS$Group,
    fixed = parsUS$se==0,
    par = 1:nrow(parsUS),
    stringsAsFactors=FALSE)
  
  RAM$lhs[grepl("BY|ON",parsUS$paramHeader)] <- gsub("\\.(BY|ON)","",parsUS$paramHeader[grepl("BY|ON",parsUS$paramHeader)])
  RAM$edge[grepl("BY|ON",parsUS$paramHeader)] <- "->"
  
  RAM$lhs[grepl("WITH",parsUS$paramHeader)] <- gsub("\\.WITH","",parsUS$paramHeader[grepl("WITH",parsUS$paramHeader)])
  RAM$edge[grepl("WITH",parsUS$paramHeader)] <- "<->"
  
  RAM$lhs[grepl("Variances",parsUS$paramHeader)] <- RAM$rhs[grepl("Variances",parsUS$paramHeader)]
  RAM$edge[grepl("Variances",parsUS$paramHeader)] <- "<->"
  
  RAM$edge[grepl("Means|Intercepts",parsUS$paramHeader)] <- "int"
  
  if (!is.null(object$parameters$standardized)) RAM$std <- object$parameters$standardized$est
  
  RAM <- RAM[!grepl("Thresholds",parsUS$paramHeader),]
  
  # Detect latent/manifest:
  Latents <- unique(gsub("\\.BY","",parsUS$paramHeader[grepl("BY",parsUS$paramHeader)]))
  var <- unique(unlist(RAM[c("lhs","rhs")]))
  var <- var[var!=""]
  
  # Variable dataframe: 
  Vars <- data.frame(
    name = var,
    manifest = !var%in%Latents,
    stringsAsFactors=FALSE)
  

  
  semModel <- new("qgraph.semModel")
  semModel@RAM <- RAM
  semModel@Vars <- Vars
  semModel@Computed <- TRUE
  semModel@Original <- list(object)
  semModel@ObsCovs <- list()
  semModel@ImpCovs <- list()
  
  return(semModel)
}