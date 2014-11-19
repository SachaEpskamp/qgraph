colstripalpha <- function(x)
{ 
  apply(col2rgb(x, alpha = FALSE),2,function(x)do.call(rgb,as.list(x/255)))
}

# This function transforms a qgraph object into a D3 file.
qgraphD3 <- function(
  input, # Either a qgraph object, or qgraph is run on this object
  D3width = 800,
  D3height = 800,
  D3file = "qgraph",
  showFile = TRUE,
  ... # Arguments sent to qgraph. If not empty and input is qgraph object qgraph is called again
){
  if (is(input,"qgraph")){
    if (length(list(...)) > 0){
      qgraphObject <- qgraph(input, ..., DoNotPlot = TRUE)
    } else {
      qgraphObject <- input
    }
  } else {
    qgraphObject <- qgraph(input, ..., DoNotPLot = TRUE)
  }

  
  # Construct edge dataframe:
  Links <- data.frame(
      source = qgraphObject$Edgelist$from-1,
      target = qgraphObject$Edgelist$to-1,
      value = abs(qgraphObject$Edgelist$weight)/ max(abs(qgraphObject$Edgelist$weight)),
      color = colstripalpha(qgraphObject$graphAttributes$Edges$color),
      width = qgraphObject$graphAttributes$Edges$width
    )
  
  Links <- Links[qgraphObject$graphAttributes$Graph$edgesort,]
  
  # HTML file name:
  Filename <- paste0(D3file,".html")
  
  Nodes <- data.frame(
    name =   qgraphObject$graphAttributes$Nodes$labels,
    color =  colstripalpha(qgraphObject$graphAttributes$Nodes$color),
    bcolor = colstripalpha(qgraphObject$graphAttributes$Nodes$border.color),
    bwidth = qgraphObject$graphAttributes$Nodes$border.width,
    group = 1
      )
  
  # Create D3network:
  d3ForceNetwork(Links = Links, Nodes = Nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group",
                 opacity = "1", file = Filename,
                 linkWidth = "LINKREPLACEDUMMY",
                  width = D3width,height =D3height)
  

  # Read the created html:
  lines <- readLines(Filename)
  linksLine <- grep("var links =", lines)  
  nodesLine <- grep("var nodes =", lines)  
  
  # Replace links and ndoes with proper links:
  lines[linksLine] <- paste("var links =", toJSONarray(Links), "; \n")
  lines[nodesLine] <- paste("var nodes =", toJSONarray(Nodes), "; \n")
  
  # Find var link:
  linkAttrLine <- grep('LINKREPLACEDUMMY', lines)
  
  # Append:
  lines[linkAttrLine] <-
    '.style("stroke", function(d) {return d.color;})
    .style("stroke-width", function(d) {return d.width;})'
  
  
  # NODES
  nodesAttrLine <- grep('.style("fill", function(d) { return color(d.group); })', lines, fixed = TRUE)
  
  # Append:
  lines[nodesAttrLine] <-
    '.style("opacity", 1)
    .style("fill", function(d) {return d.color;})
    .style("stroke", function(d) {return d.bcolor;})
    .style("stroke-width", function(d) {return d.bwidth;})'
  
  # Remove border white:
  lines <- gsub("stroke: #fff;","",lines)
  
  # Fill text:
  lines <- gsub("text {","text {\nfill: #666;\n",lines,fixed=TRUE)
  
  # Fix other things:
  lines <- gsub("&quot;",'"',lines)
  lines <- gsub("&lt;",'<',lines)
  
  # Write file again:
  # Fix and write:
  writeLines(lines, Filename)
  
  if (showFile){
    browseURL(Filename)
  }

}
