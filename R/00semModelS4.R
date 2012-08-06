
setClass( "qgraph.semModel", representation(
    RAM = "data.frame",
    Vars = "data.frame",
    Computed = "logical",
    Original = "list"))

setGeneric("qgraphSEM.S4", function(object) {
  standardGeneric("qgraphSEM.S4")
})

setGeneric("pathDiagram.S4", function(object,...) {
  standardGeneric("pathDiagram.S4")
})

pathDiagram <- function(object,...)
{
  if ("MxRAMModel"%in%class(object)) return(pathDiagram_MxRAMModel(object,...)) 
  if ("MxModel"%in%class(object)) return(pathDiagram_MxModel(object,...))
  if(isS4(object)) 
  {
    pathDiagram.S4(object, ...)
  } else
  {
    UseMethod("pathDiagram", object)
  }
}

qgraphSEM <- function (object) {
  if ("MxRAMModel"%in%class(object)) return(qgraphSEM_MxRAMModel(object)) 
  if ("MxModel"%in%class(object)) return(qgraphSEM_MxModel(object))
  if(isS4(object)) 
  {
    qgraphSEM.S4(object)
  } else
  {
    UseMethod("qgraphSEM", object)
  }
}

