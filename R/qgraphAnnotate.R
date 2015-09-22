# Uses sendplot to annotate a qgraph object:
qgraphAnnotate <- function(
  graph, # graph object from qgraph
  ..., # Named vectors indicating elements of the tooltip
  fromqgraph = c("labels","nodeNames","tooltips","groups"), # Vector indicating which info should be extracted from qgraph object and plotted.
  filename = "qgraph",
  image.size = "600x600", 
  window.size = image.size,
  legend = FALSE # Overwries legend plotting
  )
{
  if(!requireNamespace("sendplot")) stop("'sendplot' package needs to be installed.")
  
  # List containing the labels:
  TooltipContents <- list(...)

  # Extract info from qgraph:
  if ("labels" %in% fromqgraph && !is.null(graph$graphAttributes$Nodes$labels) && !is.logical(graph$graphAttributes$Nodes$labels))
  {
    TooltipContents$Label <- graph$graphAttributes$Nodes$labels
  }

  if ("nodeNames" %in% fromqgraph && !is.null(graph$graphAttributes$Nodes$names) && !is.logical(graph$graphAttributes$Nodes$names))
  {
    TooltipContents$Name <- graph$graphAttributes$Nodes$names
  }
  
  if ("tooltips" %in% fromqgraph && !is.null(graph$graphAttributes$Nodes$tooltips) && !is.logical(graph$graphAttributes$Nodes$tooltips))
  {
    TooltipContents$Tooltip <- graph$graphAttributes$Nodes$tooltips
  }
  
  if ("groups" %in% fromqgraph && !is.null(graph$graphAttributes$Graph$groups) && length(graph$graphAttributes$Graph$groups) > 1)
  {
    gr <- graph$graphAttributes$Graph$groups
    if (is.null(names(gr))) names(gr) <- paste("Group",seq_along(gr))
        
    TooltipContents$Group <- sapply(seq_len(graph$graphAttributes$Graph$nNodes), function(n)  paste(names(gr)[sapply(gr,function(g)n%in%g)], collapse = "; "))
  }
  
  TooltipContents <- as.data.frame(TooltipContents)
  
  # Fix for legend:
  graph$plotOptions$legend <- legend

  # Create plot:
#   xy.send(paste0("qgraph:::plot.qgraph(",dput(graph),")"),
  save(graph, file = tempfile(fileext = ".RData") -> gObj)
  if (grepl("(windows)|(ming)",R.Version()$os,ignore.case=TRUE)){
    gObj <- gsub("\\\\","\\\\\\\\",gObj)
  }

  if (NROW(TooltipContents) > 0)
  {
    sendplot::xy.send(paste0("load('",gObj,"');qgraph:::plot.qgraph(graph)"),
            x.pos = graph$layout[,1],
            y.pos = graph$layout[,2],
            xy.labels = TooltipContents,
            fname.root = filename,
            dir = paste0(getwd(),"/"),
            image.size = image.size,
            window.size = window.size)
  } else {
    sendplot::xy.send(paste0("load('",gObj,"');qgraph:::plot.qgraph(graph)"),
            x.pos = -100,
            y.pos = -100,
            xy.labels = data.frame(` ` = ''),
            fname.root = filename,
            dir = paste0(getwd(),"/"),
            image.size = image.size,
            window.size = window.size)
  }
#     xy.send("plot.qgraph(graph)",

  
  return(paste0(filename,".html"))
}